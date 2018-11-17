{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Execution mode used in Auxx.

module Mode
       (
       -- * Mode, context, etc.
         AuxxContext (..)
       , AuxxMode
       , MonadAuxxMode

       -- * Helpers
       , realModeToAuxx
       ) where

import           Universum

import           Control.Lens (lens, makeLensesWith)
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.Trans.Resource (transResourceT)
import           Data.Conduit (transPipe)

import           Pos.Chain.Block (HasSlogGState (slogGState), HasSlogContext(slogContext))
import           Pos.Chain.Ssc (HasSscContext (sscContext))
import           Pos.Client.KeyStorage (MonadKeys (modifySecret), MonadKeysRead (getSecret), getSecretDefault, modifySecretDefault)
import           Pos.Client.Txp.Balances (MonadBalances (getOwnUtxos, getBalance), getBalanceFromUtxo, getOwnUtxosGenesis)
import           Pos.Client.Txp.History (MonadTxHistory (getBlockHistory, getLocalHistory, saveTx), getBlockHistoryDefault, getLocalHistoryDefault, saveTxDefault)
import           Pos.Context (HasNodeContext (nodeContext))
import           Pos.Core (HasPrimaryKey (primaryKey))
import           Pos.Core.JsonLog (CanJsonLog (jsonLog))
import           Pos.Core.Reporting (HasMisbehaviorMetrics (misbehaviorMetrics), MonadReporting (report))
import           Pos.Core.Slotting (HasSlottingVar (slottingVar, slottingTimestamp), MonadSlotsData)
import           Pos.DB (DBSum (RealDB, PureDB), MonadGState (gsAdoptedBVData), NodeDBs)
import           Pos.DB.Block (MonadBListener (onApplyBlocks, onRollbackBlocks))
import           Pos.DB.Class (MonadDB (dbPut, dbWriteBatch, dbDelete, dbPutSerBlunds), MonadDBRead (dbGet, dbIterSource, dbGetSerBlock, dbGetSerUndo, dbGetSerBlund))
import           Pos.DB.Txp (MempoolExt, MonadTxpLocal (txpNormalize, txpProcessTx), txNormalize,
                     txProcessTransaction)
import           Pos.GState (HasGStateContext (gStateContext), getGStateImplicit)
import           Pos.Infra.Network.Types (HasNodeType (getNodeType), NodeType (NodeEdge))
import           Pos.Infra.Shutdown (HasShutdownContext (shutdownContext))
import           Pos.Infra.Slotting.Class (MonadSlots (getCurrentSlot, getCurrentSlotBlocking, getCurrentSlotInaccurate, currentTimeSlotting))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util (HasLens (lensOf), postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.LoggerName (HasLoggerName' (loggerName))
import           Pos.Util.UserSecret (HasUserSecret (userSecret))
import           Pos.Util.Wlog (HasLoggerName (askLoggerName, modifyLoggerName))
import           Pos.WorkMode (EmptyMempoolExt, RealMode, RealModeContext)

type AuxxMode = ReaderT AuxxContext IO

class (m ~ AuxxMode, HasConfigurations, HasCompileInfo) => MonadAuxxMode m
instance (HasConfigurations, HasCompileInfo) => MonadAuxxMode AuxxMode

data AuxxContext = AuxxContext
    { acRealModeContext :: !(RealModeContext EmptyMempoolExt)
    }

makeLensesWith postfixLFields ''AuxxContext

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Turn 'RealMode' action into 'AuxxMode' action.
realModeToAuxx :: RealMode EmptyMempoolExt a -> AuxxMode a
realModeToAuxx = withReaderT acRealModeContext

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

-- hacky instance needed to make blockgen work
instance HasLens DBSum AuxxContext DBSum where
    lensOf =
        let getter ctx = RealDB (ctx ^. (lensOf @NodeDBs))
            setter ctx (RealDB db') = ctx & (lensOf @NodeDBs) .~ db'
            setter _ (PureDB _)     = error "Auxx: tried to set pure db insteaf of nodedb"
        in lens getter setter

instance HasGStateContext AuxxContext where
    gStateContext = getGStateImplicit

instance HasSscContext AuxxContext where
    sscContext = acRealModeContext_L . sscContext

instance HasPrimaryKey AuxxContext where
    primaryKey = acRealModeContext_L . primaryKey

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
-- The pieces of the software which the block generator uses should never
-- even try to report.
instance MonadReporting AuxxMode where
    report _ = pure ()

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
instance HasMisbehaviorMetrics AuxxContext where
    misbehaviorMetrics = lens (const Nothing) const

instance HasUserSecret AuxxContext where
    userSecret = acRealModeContext_L . userSecret

instance HasShutdownContext AuxxContext where
    shutdownContext = acRealModeContext_L . shutdownContext

instance HasNodeContext AuxxContext where
    nodeContext = acRealModeContext_L . nodeContext

instance HasSlottingVar AuxxContext where
    slottingTimestamp = acRealModeContext_L . slottingTimestamp
    slottingVar = acRealModeContext_L . slottingVar

instance HasNodeType AuxxContext where
    getNodeType _ = NodeEdge

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
    HasLens tag AuxxContext r
  where
    lensOf = acRealModeContext_L . lensOf @tag

instance HasLoggerName' AuxxContext where
    loggerName = acRealModeContext_L . loggerName

instance HasSlogContext AuxxContext where
    slogContext = acRealModeContext_L . slogContext

instance HasSlogGState AuxxContext where
    slogGState = acRealModeContext_L . slogGState

instance MonadSlotsData ctx AuxxMode => MonadSlots ctx AuxxMode where
    getCurrentSlot = realModeToAuxx . getCurrentSlot
    getCurrentSlotBlocking = realModeToAuxx . getCurrentSlotBlocking
    getCurrentSlotInaccurate = realModeToAuxx . getCurrentSlotInaccurate
    currentTimeSlotting = realModeToAuxx currentTimeSlotting

instance {-# OVERLAPPING #-} HasLoggerName AuxxMode where
    askLoggerName = realModeToAuxx askLoggerName
    modifyLoggerName f action = do
        auxxCtx <- ask
        let auxxToRealMode :: AuxxMode a -> RealMode EmptyMempoolExt a
            auxxToRealMode = withReaderT (\realCtx -> set acRealModeContext_L realCtx auxxCtx)
        realModeToAuxx $ modifyLoggerName f $ auxxToRealMode action

instance {-# OVERLAPPING #-} CanJsonLog AuxxMode where
    jsonLog = realModeToAuxx ... jsonLog

instance MonadDBRead AuxxMode where
    dbGet = realModeToAuxx ... dbGet
    dbIterSource tag p =
        transPipe (transResourceT realModeToAuxx) (dbIterSource tag p)
    dbGetSerBlock = realModeToAuxx ... dbGetSerBlock
    dbGetSerUndo = realModeToAuxx ... dbGetSerUndo
    dbGetSerBlund = realModeToAuxx ... dbGetSerBlund

instance MonadDB AuxxMode where
    dbPut = realModeToAuxx ... dbPut
    dbWriteBatch = realModeToAuxx ... dbWriteBatch
    dbDelete = realModeToAuxx ... dbDelete
    dbPutSerBlunds = realModeToAuxx ... dbPutSerBlunds

instance MonadGState AuxxMode where
    gsAdoptedBVData = realModeToAuxx ... gsAdoptedBVData

instance MonadBListener AuxxMode where
    onApplyBlocks = realModeToAuxx ... onApplyBlocks
    onRollbackBlocks = realModeToAuxx ... onRollbackBlocks

instance MonadBalances AuxxMode where
    getOwnUtxos genesisData addrs = getOwnUtxosGenesis genesisData addrs
    getBalance = getBalanceFromUtxo

instance MonadTxHistory AuxxMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKeysRead AuxxMode where
    getSecret = getSecretDefault

instance MonadKeys AuxxMode where
    modifySecret = modifySecretDefault

type instance MempoolExt AuxxMode = EmptyMempoolExt

instance MonadTxpLocal AuxxMode where
    txpNormalize pm = withReaderT acRealModeContext . txNormalize pm
    txpProcessTx genesisConfig txpConfig = withReaderT acRealModeContext . txProcessTransaction genesisConfig txpConfig

