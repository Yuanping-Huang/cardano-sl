{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           AutomatedTestRunner
import           BlockParser ()
import           Data.Constraint (Dict (Dict))
import           Data.Default (def)
import           Formatting (int, sformat, (%), Format)
import           PocMode
import           Pos.Chain.Update (BlockVersion(BlockVersion), SoftwareVersion(SoftwareVersion), ApplicationName(ApplicationName), BlockVersionModifier(bvmMaxTxSize), BlockVersionData(bvdMaxTxSize, bvdMaxBlockSize))
import           Pos.DB.Class (gsAdoptedBVData)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (HasConfigurations)
import           Serokell.Data.Memory.Units (Byte)
import           Universum hiding (on)
import           Data.Ix (range)

printbvd :: Dict HasConfigurations -> Diffusion AuxxMode -> AuxxMode ()
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
    proposal1 :: Dict HasConfigurations -> Diffusion AuxxMode -> AuxxMode ()
    proposal1 Dict diffusion = do
      let
        keyIndex :: Int
        keyIndex = 0
        blockVersion = BlockVersion 0 0 0
        softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 1
        blockVersionModifier = def { bvmMaxTxSize = Just 131072 }
      doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier
      print ("done?"::String)
  onStartup $ \Dict _diffusion -> loadNKeys 4
  on (1,0) printbvd
  on (1,10) proposal1
  on (2,0) printbvd
  on (3,0) printbvd

main :: IO ()
main = do
  _corenodes <- forM (range (0,3)) $ \node -> startNode (Core node)
  runScript $ return $ getScript test4
