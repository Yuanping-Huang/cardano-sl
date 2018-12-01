{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           AutomatedTestRunner
import           BlockParser ()
import           Data.Constraint (Dict (Dict))
import           Data.Default (def)
import           Data.Ix (range)
import           Formatting (Format, int, sformat, (%))
import           NodeControl (NodeHandle, NodeType (..), genSystemStart,
                     startNode, stopNode)
import           PocMode
import           Pos.Chain.Update (ApplicationName (ApplicationName),
                     BlockVersion (BlockVersion),
                     BlockVersionData (bvdMaxBlockSize, bvdMaxTxSize),
                     BlockVersionModifier, SoftwareVersion (SoftwareVersion))
import           Pos.DB.Class (gsAdoptedBVData)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.Wlog (logInfo)
import           Serokell.Data.Memory.Units (Byte)
import           Universum hiding (on)

printbvd :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
printbvd Dict _diffusion = do
  let
    bvdfmt :: Format r (Byte -> Byte -> r)
    bvdfmt = "BVD: max-tx: " %int% ", max-block: " %int
  bar <- gsAdoptedBVData
  print $ sformat bvdfmt (bvdMaxTxSize bar) (bvdMaxBlockSize bar)

test4 :: Example ()
test4 = do
  genesisConfig <- getGenesisConfig
  --on (82,19160) $ print "it is now epoch 0 slot 10"
  --on (82,19170) $ print "it is now epoch 0 slot 10"
  --on (82,19180) $ print "it is now epoch 0 slot 10"
  --on (82,19200) $ print "it is now epoch 0 slot 20"
  let
    proposal1 :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
    proposal1 Dict diffusion = do
      let
        keyIndex :: Int
        keyIndex = 0
        blockVersion = BlockVersion 0 0 0
        softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 1
        blockVersionModifier :: BlockVersionModifier
        blockVersionModifier = def -- { bvmMaxTxSize = Just 131072 }
      doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier
      print ("done?"::String)
  onStartup $ \Dict _diffusion -> loadNKeys 4
  on (1,10) proposal1
  forM_ (range (0,20)) $ \epoch -> on(epoch, 0) printbvd

main :: IO ()
main = do
  systemStart <- genSystemStart 60
  let
    createNodes :: IO [NodeHandle]
    createNodes = do
      corenodes <- forM (range (0,3)) $ \node -> startNode (Core node systemStart)
      pure corenodes
    cleanupNodes :: [NodeHandle] -> IO ()
    cleanupNodes corenodes = do
      logInfo "stopping all nodes"
      mapM_ stopNode corenodes
    runScript' :: [NodeHandle] -> IO ()
    runScript' _corenodes = do
      runScript $ return $ getScript test4
  bracket createNodes cleanupNodes runScript'
