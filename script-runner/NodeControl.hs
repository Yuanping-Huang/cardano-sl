{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeControl (NodeHandle, NodeType(..), startNode, stopNode, genSystemStart) where

import           Control.Concurrent.Async.Lifted.Safe
import           Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale,
                     formatTime, getCurrentTime)
import           Pos.Util.Wlog (logWarning)
import           System.Posix.Signals
import           System.Process
import           Universum hiding (on, state, when)

data NodeHandle = NodeHandle (Async ()) ProcessHandle
data NodeType = Core { ntIdex :: Integer, ntSystemStart :: String }

startNode :: NodeType -> IO NodeHandle
startNode (Core idx systemStart) = do
  childStdout <- openFile ("poc-state/core-stdout-" <> show idx) WriteMode
  let
    params = [ "--configuration-file", "../lib/configuration.yaml"
             , "--system-start", systemStart
             , "--db-path", "poc-state/core" <> (show idx) <> "-db"
             , "--keyfile", "poc-state/secret" <> (show idx) <> ".key"
             ]
    pc :: CreateProcess
    pc = (proc "cardano-node-simple" params) { std_out = UseHandle childStdout }
  (_stdin, _stdout, _stderr, ph) <- createProcess pc
  later <- async $ do
    _ <- waitForProcess ph
    pure ()
  pure $ NodeHandle later ph

stopNode :: NodeHandle -> IO ()
stopNode (NodeHandle async ph) = do
  maybePid <- getPid ph
  case maybePid of
    Just pid -> do
      signalProcess sigINT pid
    Nothing -> do
      logWarning "node already stopped when trying to stop it"

genSystemStart :: NominalDiffTime -> IO String
genSystemStart offset = formatTime defaultTimeLocale "%s" . addUTCTime offset <$> getCurrentTime
