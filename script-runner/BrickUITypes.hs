{-# LANGUAGE NoImplicitPrelude #-}

module BrickUITypes (AppState(..), Name, CustomEvent(..), Reply(..), SlotStart(..), NodeInfo(..)) where

import           Brick.BChan
import           Pos.Core (EpochOrSlot)
import           Universum hiding (on, state, when)

data AppState = AppState
  { asLocalHeight      :: Word64
  , asGlobalHeight     :: Maybe Word64
  , asLastMsg          :: String
  , asLocalEpochOrSlot :: Maybe EpochOrSlot
  , asReplyChan        :: BChan Reply
  }

data Reply = TriggerShutdown

data SlotStart = SlotStart
  { ssEpoch :: Word64
  , ssSlot  :: Word16
  } deriving Show

data NodeInfo = NodeInfo
  { niLocalHeight      :: Word64
  , niLocalEpochOrSlot :: EpochOrSlot
  , niGlobalHeight     :: Maybe Word64
  } deriving Show

data CustomEvent
    = CESlotStart SlotStart
    | CENodeInfo NodeInfo
    | QuitEvent
    deriving Show

data Name = None deriving (Show, Ord, Eq)
