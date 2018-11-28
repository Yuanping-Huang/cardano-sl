{-# LANGUAGE NamedFieldPuns #-}

module BrickUI (AppState(..), ui, CustomEvent(..), handleEvent) where

import           Brick hiding (on)
import           Graphics.Vty (Event(EvKey), Key(KEnter))
import           Universum hiding (when, show, on, state)
import           Pos.Core (ChainDifficulty, EpochOrSlot, HasDifficulty (..))

data AppState = AppState
  { asLocalHeight :: Word64
  , asGlobalHeight :: Maybe Word64
  , asLastMsg :: String
  , asLocalEpochOrSlot :: Maybe EpochOrSlot
  } deriving Show
data CustomEvent = SlotStart
    { ceEpoch :: Word64
    , ceSlot :: Word16
    }
    | NodeInfoEvent
    { ceLocalHeight :: Word64
    , ceLocalEpochOrSlot :: EpochOrSlot
    , ceGlobalHeight :: Maybe Word64
    }
    | QuitEvent deriving Show

localHeight :: AppState -> Widget ()
localHeight AppState{asLocalHeight} = str $ "Local Block Count: " <> show asLocalHeight

globalHeight :: AppState -> Widget ()
globalHeight AppState{asGlobalHeight} = str $ "global: " <> maybe "unknown" show asGlobalHeight

percent :: AppState -> Widget ()
percent AppState{asLocalHeight,asGlobalHeight} = do
  let
    computeProgress :: Word64 -> String
    computeProgress global = (show (((fromIntegral asLocalHeight) / (fromIntegral global)) * 100)) <> "%"
  str $ "Percent: " <> maybe "unknown" computeProgress asGlobalHeight

lastMessage :: AppState -> Widget ()
lastMessage AppState{asLastMsg} = str asLastMsg

currentTip :: AppState -> Widget ()
currentTip AppState{asLocalEpochOrSlot} = str $ "Local Slot: " <> show asLocalEpochOrSlot

ui :: AppState -> [ Widget () ]
ui state = [ localHeight state <=> globalHeight state <=> percent state <=> lastMessage state <=> currentTip state ]

handleEvent :: AppState -> BrickEvent () CustomEvent -> EventM n (Next AppState)
handleEvent state (VtyEvent (EvKey KEnter [])) = halt state
handleEvent state (AppEvent ae) = do
  case ae of
    NodeInfoEvent{ceLocalHeight,ceGlobalHeight,ceLocalEpochOrSlot} -> do
      continue $ state
        { asLocalHeight = ceLocalHeight
        , asGlobalHeight = ceGlobalHeight
        , asLocalEpochOrSlot = Just ceLocalEpochOrSlot
        }
    QuitEvent -> halt state
    SlotStart e s -> continue $ state { asLastMsg = (show e) <> " " <> (show s) }
handleEvent state evt = do
  continue $ state { asLastMsg = show evt }
