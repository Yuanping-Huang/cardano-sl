{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BlockParser where

import           Universum hiding (when)
import Codec.CBOR.Read (deserialiseFromBytes)
import Pos.Binary.Class (decode)
import Pos.Chain.Block (Block)

import qualified Data.ByteString.Lazy as LBS

file1 :: FilePath
file1 = "/home/clever/dedup/staging-poc/blocks/data/68/c85b75adcc99048e633e4b84337b36542e020337c618742d7716e6acc22b39.blund"
file2 :: FilePath
file2 = "9f5124c1f20924f809e741dc9ceda2d2c5422ce8f7024f800060df4e787bc9d3.blund"

printBlock :: FilePath -> IO ()
printBlock filename = do
  raw <- LBS.readFile filename
  let
    blockraw :: LBS.ByteString
    undoraw :: LBS.ByteString
    Right ("", (blockraw, undoraw)) = deserialiseFromBytes decode raw
    block :: Block
    Right ("", block) = deserialiseFromBytes decode blockraw
  case block of
    Left gb -> print ("genesis block" :: String)
    Right mb -> do
      print ("main block" :: String)
      print mb
