{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module AutomatedTestRunner (Example, getGenesisConfig, loadNKeys, doUpdate, onStartup, on, getScript, runScript, NodeType(..), startNode) where

import           Brick hiding (on)
import           Brick.BChan
import           BrickUI
import           BrickUITypes
import           Control.Concurrent
import           Control.Concurrent.Async.Lifted.Safe
import           Control.Exception (throw)
import           Control.Lens (to)
import           Control.Monad.STM (orElse)
import           Data.Constraint (Dict(Dict))
import           Data.Default (Default(def))
import           Data.Ix (range)
import           Data.List ((!!))
import           Data.Reflection (Given, given, give)
import           Data.Version (showVersion)
import           Options.Applicative (Parser, execParser, footerDoc, fullDesc, header, help, helper, info, infoOption, long, progDesc)
import           Paths_cardano_sl (version)
import           Pos.Client.KeyStorage (getSecretKeysPlain, addSecretKey)
import           Pos.Crypto (emptyPassphrase, hash, hashHexF, withSafeSigners, noPassEncrypt)
import           Pos.DB.BlockIndex (getTipHeader)
import           Pos.Util.CompileInfo (CompileTimeInfo (ctiGitRevision), HasCompileInfo, compileInfo, withCompileInfo)
import           Prelude (show)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Universum hiding (when, show, on, state)
import qualified Pos.Client.CLI as CLI
import           Pos.Launcher (HasConfigurations, NodeParams (npBehaviorConfig, npUserSecret, npNetworkConfig),
                     NodeResources, WalletConfiguration,
                     bracketNodeResources, loggerBracket,
                     runNode, runRealMode, withConfigurations, InitModeContext)
import           Formatting (int, sformat, (%), Format)
import           Graphics.Vty (mkVty, defaultConfig, defAttr)
import           Ntp.Client (NtpConfiguration)
import           PocMode (AuxxContext(AuxxContext, acRealModeContext), AuxxMode, realModeToAuxx)
import           Pos.Chain.Block (LastKnownHeaderTag)
import           Pos.Chain.Genesis as Genesis (Config (configGeneratedSecrets, configProtocolMagic), configEpochSlots)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Chain.Update (UpdateData, SystemTag, mkUpdateProposalWSign, BlockVersion, SoftwareVersion, BlockVersionModifier, updateConfiguration)
import           Pos.Client.Update.Network (submitUpdateProposal)
import           Pos.Core (LocalSlotIndex, SlotId (SlotId, siEpoch, siSlot), mkLocalSlotIndex, EpochIndex(EpochIndex), SlotCount, getEpochIndex, getSlotIndex, difficultyL, getChainDifficulty, getBlockCount, getEpochOrSlot)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Infra.Network.Types (NetworkConfig (ncTopology, ncEnqueuePolicy, ncDequeuePolicy, ncFailurePolicy), Topology (TopologyAuxx), topologyDequeuePolicy, topologyEnqueuePolicy, topologyFailurePolicy, NodeId)
import           Pos.Infra.Shutdown (triggerShutdown)
import           Pos.Infra.Slotting.Util (onNewSlot, defaultOnNewSlotParams)
import           Pos.Util (logException, lensOf)
import           Pos.Util.UserSecret (usVss, readUserSecret, usPrimKey, usKeys)
import           Pos.Util.Wlog (LoggerName)
import           Pos.WorkMode (RealMode, EmptyMempoolExt)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T

class TestScript a where
  getScript :: a -> Script

data ScriptRunnerOptions = ScriptRunnerOptions
  { srCommonNodeArgs :: !CLI.CommonNodeArgs -- ^ Common CLI args for nodes
  , srPeers          :: ![NodeId]
  } deriving Show

data ScriptBuilder = ScriptBuilder
  { sbScript :: Script
  , sbEpochSlots :: SlotCount
  , sbGenesisConfig :: Config
  }
data NodeType = Core { ntIdex :: Integer }

instance Default Script where def = Script def def

data Script = Script
  { slotTriggers :: Map.Map SlotId SlotTrigger
  , startupActions :: [ SlotTrigger ]
  } deriving (Show, Generic)

data SlotTrigger = SlotTrigger (Dict HasConfigurations -> Diffusion AuxxMode -> AuxxMode ())

instance Show SlotTrigger where
  show _ = "IO ()"

newtype ExampleT m a = ExampleT { runExampleT :: StateT ScriptBuilder m a } deriving (Functor, Applicative, Monad, MonadState ScriptBuilder)
newtype Example a = Example { runExample :: ExampleT (Identity) a } deriving (Applicative, Functor, Monad, MonadState ScriptBuilder)

instance HasEpochSlots => TestScript (Example a) where
  getScript action = do
    let
      script :: ScriptBuilder
      script = snd $ runIdentity $ runStateT (runExampleT $ runExample action) (ScriptBuilder def getEpochSlots getEpochSlots')
    sbScript script

instance TestScript Script where
  getScript = identity

data NodeHandle = NodeHandle (Async ())

data EpochSlots = EpochSlots { epochSlots :: SlotCount, config :: Config }
type HasEpochSlots = Given EpochSlots

getEpochSlots :: HasEpochSlots => SlotCount
getEpochSlots = epochSlots given

getEpochSlots' :: HasEpochSlots => Config
getEpochSlots' = config given

withEpochSlots :: SlotCount -> Config -> (HasEpochSlots => a) -> a
withEpochSlots epochSlots genesisConfig = give (EpochSlots epochSlots genesisConfig)

scriptRunnerOptionsParser :: Parser ScriptRunnerOptions
scriptRunnerOptionsParser = do
  srCommonNodeArgs <- CLI.commonNodeArgsParser
  srPeers <- many $ CLI.nodeIdOption "peer" "Address of a peer."
  pure ScriptRunnerOptions{..}

usageExample :: Maybe Doc
usageExample = Just "todo"

getScriptRunnerOptions :: HasCompileInfo => IO ScriptRunnerOptions
getScriptRunnerOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> scriptRunnerOptionsParser) $
        fullDesc <> progDesc "Cardano SL CLI utilities."
                 <> header "CLI-based utilities (auxx)."
                 <> footerDoc usageExample
    versionOption :: Parser (a -> a)
    versionOption = infoOption
      ("cardano-script-runner" <> showVersion version <> ", git revision " <> toString (ctiGitRevision compileInfo))
      (long "version" <> help "Show version.")

loggerName :: LoggerName
loggerName = "script-runner"

thing :: (TestScript a, HasCompileInfo) => ScriptRunnerOptions -> InputParams a -> IO ()
thing opts@ScriptRunnerOptions{..} inputParams = do
  let
    conf = CLI.configurationOptions (CLI.commonArgs cArgs)
    cArgs@CLI.CommonNodeArgs{..} = srCommonNodeArgs
  withConfigurations Nothing cnaDumpGenesisDataPath cnaDumpConfiguration conf (runWithConfig opts inputParams)

maybeAddPeers :: [NodeId] -> NodeParams -> NodeParams
maybeAddPeers [] params = params
maybeAddPeers peers nodeParams = nodeParams { npNetworkConfig = (npNetworkConfig nodeParams) { ncTopology = TopologyAuxx peers } }

addQueuePolicies :: NodeParams -> NodeParams
addQueuePolicies nodeParams = do
  let
    topology = ncTopology $ npNetworkConfig nodeParams
  nodeParams { npNetworkConfig = (npNetworkConfig nodeParams)
    { ncEnqueuePolicy = topologyEnqueuePolicy topology
    , ncDequeuePolicy = topologyDequeuePolicy topology
    , ncFailurePolicy = topologyFailurePolicy topology
    }
  }

runWithConfig :: (TestScript a, HasCompileInfo, HasConfigurations) => ScriptRunnerOptions -> InputParams a -> Genesis.Config -> WalletConfiguration -> TxpConfiguration -> NtpConfiguration -> IO ()
runWithConfig ScriptRunnerOptions{..} inputParams genesisConfig _walletConfig txpConfig _ntpConfig = do
  let
    cArgs@CLI.CommonNodeArgs {..} = srCommonNodeArgs
    nArgs = CLI.NodeArgs {behaviorConfigPath = Nothing}
  (nodeParams', _mSscParams) <- CLI.getNodeParams loggerName cArgs nArgs (configGeneratedSecrets genesisConfig)
  let
    nodeParams = addQueuePolicies $ maybeAddPeers srPeers $ nodeParams'
    epochSlots = configEpochSlots genesisConfig
    vssSK = fromMaybe (error "no user secret given") (npUserSecret nodeParams ^. usVss)
    sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    thing1 = txpGlobalSettings genesisConfig txpConfig
    thing2 :: ReaderT InitModeContext IO ()
    thing2 = initNodeDBs genesisConfig
  script <- liftIO $ withEpochSlots epochSlots genesisConfig (ipScriptGetter inputParams)
  let
    inputParams' = InputParams2 (ipEventChan inputParams) (ipReplyChan inputParams) script
  bracketNodeResources genesisConfig nodeParams sscParams thing1 thing2 (thing3 genesisConfig txpConfig inputParams')

thing3 :: (TestScript a, HasCompileInfo, HasConfigurations) => Config -> TxpConfiguration -> InputParams2 a -> NodeResources () -> IO ()
thing3 genesisConfig txpConfig inputParams nr = do
  let
    toRealMode :: AuxxMode a -> RealMode EmptyMempoolExt a
    toRealMode auxxAction = do
      realModeContext <- ask
      lift $ runReaderT auxxAction $ AuxxContext { acRealModeContext = realModeContext }
    thing2 :: Diffusion (RealMode ()) -> RealMode EmptyMempoolExt ()
    thing2 diffusion = toRealMode (thing5 (hoistDiffusion realModeToAuxx toRealMode diffusion))
    thing5 :: Diffusion AuxxMode -> AuxxMode ()
    thing5 = runNode genesisConfig txpConfig nr thing4
    thing4 :: [ (Text, Diffusion AuxxMode -> AuxxMode ()) ]
    thing4 = workers genesisConfig inputParams
  runRealMode updateConfiguration genesisConfig txpConfig nr thing2

workers :: (HasConfigurations, TestScript a) => Genesis.Config -> InputParams2 a -> [ (Text, Diffusion AuxxMode -> AuxxMode ()) ]
workers genesisConfig InputParams2{ip2EventChan,ip2Script,ip2ReplyChan} =
  [ ( T.pack "worker1", worker1 genesisConfig ip2Script ip2EventChan)
  , ( "worker2", worker2 ip2EventChan)
  , ( "brick reply worker", brickReplyWorker ip2ReplyChan)
  ]

brickReplyWorker :: HasConfigurations => BChan Reply -> Diffusion AuxxMode -> AuxxMode ()
brickReplyWorker replyChan diffusion = do
  reply <- liftIO $ readBChan replyChan
  case reply of
    TriggerShutdown -> do
      triggerShutdown
  brickReplyWorker replyChan diffusion

worker2 :: HasConfigurations => BChan CustomEvent -> Diffusion AuxxMode -> AuxxMode ()
worker2 eventChan diffusion = do
  localTip  <- getTipHeader
  headerRef <- view (lensOf @LastKnownHeaderTag)
  mbHeader <- atomically $ readTVar headerRef `orElse` pure Nothing
  let
    globalHeight = view (difficultyL . to getChainDifficulty) <$> mbHeader
    localHeight = view (difficultyL . to getChainDifficulty) localTip
    f (Just v) = Just $ getBlockCount v
    f Nothing = Nothing
  liftIO $ do
    writeBChan eventChan $ CENodeInfo $ NodeInfo (getBlockCount localHeight) (getEpochOrSlot localTip) (f globalHeight)
    threadDelay 100000
  worker2 eventChan diffusion

worker1 :: (HasConfigurations, TestScript a) => Genesis.Config -> a -> BChan CustomEvent -> Diffusion (AuxxMode) -> AuxxMode ()
worker1 genesisConfig script eventChan diffusion = do
  let
    handler :: SlotId -> AuxxMode ()
    handler slotid = do
      liftIO $ writeBChan eventChan $ CESlotStart $ SlotStart (getEpochIndex $ siEpoch slotid) (getSlotIndex $ siSlot slotid)
      case Map.lookup slotid (slotTriggers realScript) of
        Just (SlotTrigger act) -> runAction act
        Nothing -> pure ()
      pure ()
    realScript = getScript script
    errhandler :: Show e => e -> AuxxMode ()
    errhandler e = print e
    runAction :: (Dict HasConfigurations -> Diffusion AuxxMode -> AuxxMode ()) -> AuxxMode ()
    runAction act = do
      act Dict diffusion `catch` errhandler @SomeException
    realWorker = do
      mapM_ (\(SlotTrigger act) -> runAction act) (startupActions realScript)
      onNewSlot (configEpochSlots genesisConfig) defaultOnNewSlotParams handler
      pure ()
  realWorker `catch` errhandler @SomeException

data TestScript a => InputParams a = InputParams
  { ipEventChan :: BChan CustomEvent
  , ipReplyChan :: BChan Reply
  , ipScriptGetter :: HasEpochSlots => IO a
  }
data TestScript a => InputParams2 a = InputParams2
  { ip2EventChan :: BChan CustomEvent
  , ip2ReplyChan :: BChan Reply
  , ip2Script :: a
  }

runScript :: TestScript a => (HasEpochSlots => IO a) -> IO ()
runScript scriptGetter = withCompileInfo $ do
  (eventChan, replyChan, asyncUi) <- runUI
  opts <- getScriptRunnerOptions
  let
    inputParams = InputParams eventChan replyChan scriptGetter
    loggingParams = CLI.loggingParams loggerName (srCommonNodeArgs opts)
  loggerBracket "script-runner" loggingParams . logException "script-runner" $ do
    thing opts inputParams
    pure ()
  liftIO $ writeBChan eventChan QuitEvent
  finalState <- wait asyncUi
  --print finalState
  pure ()

runUI :: IO (BChan CustomEvent, BChan Reply, Async AppState)
runUI = do
  eventChan <- newBChan 10
  replyChan <- newBChan 10
  let
    app = App
      { appDraw = ui
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent
      , appStartEvent = \x -> pure x
      , appAttrMap = const $ attrMap defAttr []
      }
    state :: AppState
    state = AppState 0 Nothing "" Nothing replyChan
    go :: IO AppState
    go = do
      finalState <- customMain (mkVty defaultConfig) (Just eventChan) app state
      writeBChan replyChan TriggerShutdown
      pure finalState
  brick <- async go
  pure (eventChan, replyChan, brick)

getGenesisConfig :: Example Config
getGenesisConfig = do
  oldsb <- get
  pure $ sbGenesisConfig oldsb

data SlotCreationFailure = SlotCreationFailure { msg :: Text, slotsInEpoch :: SlotCount } deriving Show
instance Exception SlotCreationFailure where

onStartup :: (Dict HasConfigurations -> Diffusion AuxxMode -> AuxxMode ()) -> Example ()
onStartup action = do
  oldsb <- get
  let
    oldscript = sbScript oldsb
    script = oldscript {
      startupActions = [ SlotTrigger action ] <> (startupActions oldscript)
    }
    newsb = oldsb {
      sbScript = script
    }
  put newsb
  pure ()

on :: (Word64, Word16) -> (Dict HasConfigurations -> Diffusion AuxxMode -> AuxxMode ()) -> Example ()
on (epoch, slot) action = do
  oldsb <- get
  let
    todo = sbEpochSlots oldsb
    go :: Either Text LocalSlotIndex -> Example LocalSlotIndex
    go (Right localSlot) = pure localSlot
    go (Left err) = do
      throw $ SlotCreationFailure err todo
  localSlot <- go $ mkLocalSlotIndex todo slot
  let
    slot' = SlotId (EpochIndex epoch) localSlot
    oldscript = sbScript oldsb
    script = oldscript {
      slotTriggers = Map.insert slot' (SlotTrigger action) (slotTriggers oldscript)
    }
    newsb = oldsb {
      sbScript = script
    }
  put newsb
  pure ()

doUpdate :: HasConfigurations => Diffusion AuxxMode -> Config -> Int -> BlockVersion -> SoftwareVersion -> BlockVersionModifier -> AuxxMode ()
doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier = do
  let
    --tag = SystemTag "win64"
    updateData :: HM.HashMap SystemTag UpdateData
    updateData = HM.fromList [
        --(tag, UpdateData dummyHash dummyHash dummyHash dummyHash)
      ]
    voteAll = True
    errmsg :: Format r (Int -> Int -> r)
    errmsg = "Number of safe signers: " % int % ", expected " % int
    pm = configProtocolMagic genesisConfig
  skeys <- if voteAll then
      getSecretKeysPlain
    else do
      skey <- (!! keyIndex) <$> getSecretKeysPlain
      pure [ skey ]
  withSafeSigners skeys (pure emptyPassphrase) $ \ss -> do
    unless (length skeys == length ss) $ error $ sformat errmsg (length ss) (length skeys)
    let
      publisherSS = ss !! if not voteAll then 0 else keyIndex
      updateProposal = mkUpdateProposalWSign pm blockVersion blockVersionModifier softwareVersion updateData def publisherSS
      upid = hash updateProposal
    submitUpdateProposal pm diffusion ss updateProposal
    if not voteAll then
      putText (sformat ("Update proposal submitted, upId: "%hashHexF%"\n") upid)
    else
      putText (sformat ("Update proposal submitted along with votes, upId: "%hashHexF%"\n") upid)
    print updateProposal

loadNKeys :: Integer -> AuxxMode ()
loadNKeys n = do
  let
    fmt :: Format r (Integer -> r)
    fmt = "../state-demo/generated-keys/rich/" % int % ".key"
    loadKey :: Integer -> AuxxMode ()
    loadKey x = do
      secret <- readUserSecret (T.unpack $ sformat fmt x)
      let
        sk = maybeToList $ secret ^. usPrimKey
        secret' = secret & usKeys %~ (++ map noPassEncrypt sk)
      let primSk = fromMaybe (error "Primary key not found") (secret' ^. usPrimKey)
      addSecretKey $ noPassEncrypt primSk
  mapM_ loadKey (range (0,n - 1))

startNode :: NodeType -> IO NodeHandle
startNode (Core idx) = do
  let
    _params = [ "--configuration-file", "../lib/configuration.yaml"
             , "--system-start", "1543100429"
             , "--db-path", "poc-state/core" <> (show idx) <> "-db"
             , "--keyfile", "poc-state/secret" <> (show idx) <> ".key"
             ]
  later <- async $ do
    pure ()
  pure $ NodeHandle later
