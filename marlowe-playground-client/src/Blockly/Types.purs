module Blockly.Types
  ( Block
  , Blockly
  , BlocklyState
  , BlocklyEvent(..)
  , Connection
  , Field
  , Input
  , NewBlockFunction
  , Workspace
  , isDragStart
  , isDragStop
  ) where

import Prologue
import Blockly.Events
  ( ChangeEvent
  , CreateEvent
  , FinishLoadingEvent
  , MoveEvent
  , UIEvent
  , element
  , SelectEvent
  )
import Effect (Effect)

foreign import data Blockly :: Type

foreign import data Workspace :: Type

foreign import data Block :: Type

foreign import data Input :: Type

foreign import data Field :: Type

foreign import data Connection :: Type

type BlocklyState =
  { blockly :: Blockly
  , workspace :: Workspace
  , rootBlockName :: String
  , blocksElementId :: String
  }

data BlocklyEvent
  = Change ChangeEvent
  | Create CreateEvent
  | Move MoveEvent
  | FinishLoading FinishLoadingEvent
  | UI UIEvent
  | Select SelectEvent

isDragStart :: BlocklyEvent -> Boolean
isDragStart (UI event) = element event == (Just "dragStart")

isDragStart _ = false

isDragStop :: BlocklyEvent -> Boolean
isDragStop (UI event) = element event == (Just "dragStop")

isDragStop _ = false

-- This is needed for the headless blockly test
type NewBlockFunction = Workspace -> String -> Effect Block
