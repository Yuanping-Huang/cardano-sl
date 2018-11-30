{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BrickUI (ui, handleEvent) where

import           Brick hiding (on)
import           Formatting
import           Graphics.Vty (Event(EvKey), Key(KChar))
import           Universum hiding (when, on, state)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import BrickUITypes

localHeight :: AppState -> Widget Name
localHeight AppState{asLocalHeight} = str $ "Local Block Count: " <> show asLocalHeight

globalHeight :: AppState -> Widget Name
globalHeight AppState{asGlobalHeight} = str $ "global: " <> maybe "unknown" show asGlobalHeight

percent :: AppState -> Widget Name
percent AppState{asLocalHeight,asGlobalHeight} = do
  let
    fmt :: Format Text (Double -> Text)
    fmt = "Percent: " % float % "%"
    go :: Maybe Word64 -> Widget Name
    go (Just global) = txt $ sformat fmt (((fromIntegral asLocalHeight) / (fromIntegral global)) * 100)
    go Nothing = emptyWidget
  go asGlobalHeight

lastMessage :: AppState -> Widget Name
lastMessage AppState{asLastMsg} = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "last debug msg")
  $ padAll 1
  $ strWrap asLastMsg

currentTip :: AppState -> Widget Name
currentTip AppState{asLocalEpochOrSlot} = strWrap $ "Local Slot: " <> show asLocalEpochOrSlot

ui :: AppState -> [ Widget Name ]
ui state = [ vBox [ localHeight state, globalHeight state, percent state, lastMessage state, currentTip state ] ]

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM n (Next AppState)
handleEvent state (VtyEvent (EvKey key [])) = do
  case key of
    KChar 'q' -> halt state
    _ -> continue $ state { asLastMsg = show key }
handleEvent state (AppEvent ae) = do
  case ae of
    CENodeInfo (NodeInfo{niLocalHeight,niGlobalHeight,niLocalEpochOrSlot}) -> do
      continue $ state
        { asLocalHeight = niLocalHeight
        , asGlobalHeight = niGlobalHeight
        , asLocalEpochOrSlot = Just niLocalEpochOrSlot
        }
    QuitEvent -> halt state
    CESlotStart (SlotStart e s) -> continue $ state { asLastMsg = (show e) <> " " <> (show s) }
handleEvent state evt = do
  continue $ state { asLastMsg = show evt }
