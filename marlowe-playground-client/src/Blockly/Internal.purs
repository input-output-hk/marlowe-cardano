module Blockly.Internal
  ( AlignDirection(..)
  , Arg(..)
  , BasicBlockDefinition
  , BlockDefinition(..)
  , ElementId(..)
  , GridConfig
  , Pair(..)
  , XML(..)
  , addBlockTypes
  , addChangeListener
  , block
  , blockType
  , centerOnBlock
  , clearUndoStack
  , clearWorkspace
  , connect
  , connectToOutput
  , connectToPrevious
  , createBlocklyInstance
  , defaultBlockDefinition
  , fieldName
  , fieldRow
  , getBlockById
  , getBlockType
  , getInputWithName
  , hideChaff
  , initializeWorkspace
  , inputList
  , inputName
  , inputType
  , loadWorkspace
  , newBlock
  , nextConnection
  , previousConnection
  , removeChangeListener
  , render
  , resize
  , select
  , setFieldText
  , style
  , typedArguments
  , updateToolbox
  , workspaceToDom
  , workspaceXML
  , x
  , xml
  , y
  ) where

import Prologue
import Blockly.Toolbox (Toolbox, encodeToolbox)
import Blockly.Types
  ( Block
  , Blockly
  , BlocklyState
  , Connection
  , Field
  , Input
  , Workspace
  )
import Data.Argonaut.Core (Json)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Newtype (class Newtype)
import Data.Number (infinity)
import Data.Traversable (class Foldable, traverse_)
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Halogen.HTML (AttrName(..), ElemName(..), Node)
import Halogen.HTML.Elements (element)
import Halogen.HTML.Properties (IProp, attr)
import Record as Record
import Simple.JSON (class WriteForeign)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.EventTarget (EventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type GridConfig
  =
  { spacing :: Int
  , length :: Int
  , colour :: String
  , snap :: Boolean
  }

type ZoomConfig =
  { controls :: Boolean
  , wheel :: Boolean
  , startScale :: Number
  , maxScale :: Number
  , minScale :: Number
  , scaleSpeed :: Number
  }

type Move =
  { scrollbars :: Boolean
  , drag :: Boolean
  , wheel :: Boolean
  }

type WorkspaceConfig =
  { toolbox :: Json
  , collapse :: Boolean
  , comments :: Boolean
  , disable :: Boolean
  , maxBlocks :: Number
  , trashcan :: Boolean
  , horizontalLayout :: Boolean
  , toolboxPosition :: String
  , css :: Boolean
  , media :: String
  , rtl :: Boolean
  , sounds :: Boolean
  , oneBasedIndex :: Boolean
  , move :: Move
  , zoom :: ZoomConfig
  , grid :: GridConfig
  }

newtype XML
  = XML String

derive instance newtypeXML :: Newtype XML _

derive newtype instance semigroupXML :: Semigroup XML

derive newtype instance monoidXML :: Monoid XML

derive newtype instance eqXML :: Eq XML

foreign import createWorkspace
  :: Blockly -> String -> WorkspaceConfig -> Effect Workspace

foreign import resize :: Blockly -> Workspace -> Effect Unit

foreign import addChangeListener :: Workspace -> EventListener -> Effect Unit

foreign import removeChangeListener :: Workspace -> EventListener -> Effect Unit

foreign import render :: Workspace -> Effect Unit

foreign import workspaceXML :: Blockly -> Workspace -> Effect XML

foreign import loadWorkspace :: Blockly -> Workspace -> XML -> Effect Unit

-- This function exposes the blockly state in the global window so it's easier to debug/test functionalities
-- It is only called once per editor at the creation of the editor, so it doesn't consume resources and
-- could be left enabled.
foreign import debugBlockly :: String -> BlocklyState -> Effect Unit

foreign import workspaceToDom :: Blockly -> Workspace -> Effect Element

foreign import select :: Block -> Effect Unit

foreign import centerOnBlock :: Workspace -> String -> Effect Unit

foreign import hideChaff :: Blockly -> Effect Unit

foreign import getBlockType :: Block -> String

foreign import clearUndoStack :: Workspace -> Effect Unit

foreign import isWorkspaceEmpty :: Workspace -> Effect Boolean

foreign import setGroup :: Blockly -> Boolean -> Effect Unit

foreign import inputList :: Block -> Array Input

foreign import connectToPrevious :: Block -> Input -> Effect Unit

foreign import previousConnection :: Block -> Connection

foreign import nextConnection :: Block -> Connection

foreign import connect :: Connection -> Connection -> Effect Unit

foreign import connectToOutput :: Block -> Input -> Effect Unit

foreign import newBlock :: Workspace -> String -> Effect Block

foreign import inputName :: Input -> String

foreign import inputType :: Input -> Int

foreign import clearWorkspace :: Workspace -> Effect Unit

foreign import fieldRow :: Input -> Array Field

foreign import setFieldText :: Field -> String -> Effect Unit

foreign import fieldName :: Field -> String

getInputWithName :: String -> Array Input -> Maybe Input
getInputWithName n = Array.find (eq n <<< inputName)

newtype ElementId
  = ElementId String

derive instance newtypeElementId :: Newtype ElementId _

foreign import createBlocklyInstance_ :: Effect Blockly

-- TODO: Now that ActusBlockly is removed we should pass two Elements instead
-- of two ElementIds.
createBlocklyInstance
  :: String -> ElementId -> ElementId -> Toolbox -> Effect BlocklyState
createBlocklyInstance
  rootBlockName
  (ElementId workspaceElementId)
  (ElementId blocksElementId)
  toolbox = do
  blockly <- createBlocklyInstance_
  workspace <- createWorkspace blockly workspaceElementId config
  debugBlockly workspaceElementId
    { blockly, workspace, rootBlockName, blocksElementId }
  pure { blockly, workspace, rootBlockName, blocksElementId }
  where
  config =
    { toolbox: encodeToolbox toolbox
    , collapse: true
    , comments: true
    , disable: true
    , maxBlocks: infinity
    , trashcan: true
    , horizontalLayout: false
    , toolboxPosition: "start"
    , css: true
    , media: "https://blockly-demo.appspot.com/static/media/"
    , rtl: false
    , sounds: true
    , oneBasedIndex: true
    , move:
        { scrollbars: true
        , drag: true
        , wheel: true
        }
    , zoom:
        { controls: true
        , wheel: false
        , startScale: 1.0
        , maxScale: 3.0
        , minScale: 0.3
        , scaleSpeed: 1.2
        }
    , grid:
        { spacing: 20
        , length: 3
        , colour: "#ccc"
        , snap: true
        }
    }

foreign import addBlockType_ :: Blockly -> String -> Foreign -> Effect Unit

addBlockType :: Blockly -> BlockDefinition -> Effect Unit
addBlockType blockly (BlockDefinition fields) =
  let
    definition = JSON.write $ Record.delete type_ fields

    type' = fields.type
  in
    addBlockType_ blockly type' definition

addBlockTypes
  :: forall f. Foldable f => Blockly -> f BlockDefinition -> Effect Unit
addBlockTypes blocklyState = traverse_ (addBlockType blocklyState)

foreign import initializeWorkspace_
  :: Blockly -> Workspace -> Element -> Effect Unit

initializeWorkspace :: BlocklyState -> Effect Unit
initializeWorkspace bs = do
  mBlockElement <- getElementById bs.blocksElementId =<<
    (map toNonElementParentNode $ document =<< window)
  case mBlockElement of
    Just blocksElement -> initializeWorkspace_ bs.blockly bs.workspace
      blocksElement
    Nothing -> throw "Blocks element not found"

foreign import getBlockById_
  :: forall a. (Block -> a) -> a -> Workspace -> String -> Effect a

getBlockById :: Workspace -> String -> Effect (Maybe Block)
getBlockById = getBlockById_ Just Nothing

foreign import updateToolbox_ :: Json -> Workspace -> Effect Unit

updateToolbox :: Toolbox -> Workspace -> Effect Unit
updateToolbox = updateToolbox_ <<< encodeToolbox

data Pair
  = Pair String String

instance writeForeignPair :: WriteForeign Pair where
  writeImpl (Pair first second) = JSON.write [ first, second ]

data Arg
  = Input { name :: String, text :: String, spellcheck :: Boolean }
  | Dropdown { name :: String, options :: Array Pair }
  | Checkbox { name :: String, checked :: Boolean }
  | Colour { name :: String, colour :: String }
  | Number
      { name :: String
      , value :: Number
      , min :: Maybe Number
      , max :: Maybe Number
      , precision :: Maybe Number
      }
  | Angle { name :: String, angle :: Number }
  | Variable { name :: String, variable :: String }
  -- Dates don't work in Blockly, see: https://developers.google.com/blockly/guides/create-custom-blocks/fields/built-in-fields/date
  | Date { name :: String, date :: String }
  | Label { text :: Maybe String, class :: Maybe String }
  | Image { src :: String, width :: Number, height :: Number, alt :: String }
  | Value { name :: String, check :: String, align :: AlignDirection }
  | Statement { name :: String, check :: String, align :: AlignDirection }
  | DummyRight
  | DummyLeft
  | DummyCentre

argType :: Arg -> Maybe { name :: String, check :: String }
argType (Value { name: _name, check }) = Just { name: _name, check }

argType (Statement { name: _name, check }) = Just { name: _name, check }

argType _ = Nothing

type_ :: Proxy "type"
type_ = Proxy

instance writeForeignArg :: WriteForeign Arg where
  writeImpl (Input fields) = JSON.write $ Record.insert type_ "field_input"
    fields
  writeImpl (Dropdown fields) = JSON.write $ Record.insert type_
    "field_dropdown"
    fields
  writeImpl (Checkbox fields) = JSON.write $ Record.insert type_
    "field_checkbox"
    fields
  writeImpl (Colour fields) = JSON.write $ Record.insert type_ "field_colour"
    fields
  writeImpl (Number fields) = JSON.write $ Record.insert type_ "field_number"
    fields
  writeImpl (Angle fields) = JSON.write $ Record.insert type_ "field_angle"
    fields
  writeImpl (Variable fields) = JSON.write $ Record.insert type_
    "field_variable"
    fields
  writeImpl (Date fields) = JSON.write $ Record.insert type_ "field_date" fields
  writeImpl (Label fields) = JSON.write $ Record.insert type_ "field_label"
    fields
  writeImpl (Image fields) = JSON.write $ Record.insert type_ "field_image"
    fields
  writeImpl (Value fields) = JSON.write $ Record.insert type_ "input_value"
    fields
  writeImpl (Statement fields) = JSON.write $ Record.insert type_
    "input_statement"
    fields
  writeImpl DummyRight = JSON.write $ { type: "input_dummy", align: AlignRight }
  writeImpl DummyLeft = JSON.write $ { type: "input_dummy", align: AlignLeft }
  writeImpl DummyCentre = JSON.write $
    { type: "input_dummy", align: AlignCentre }

data AlignDirection
  = AlignLeft
  | AlignCentre
  | AlignRight

instance writeForeignAlignDirection :: WriteForeign AlignDirection where
  writeImpl AlignLeft = JSON.write "LEFT"
  writeImpl AlignCentre = JSON.write "CENTRE"
  writeImpl AlignRight = JSON.write "RIGHT"

type BasicBlockDefinition r =
  ( message0 :: String
  , args0 :: Array Arg
  , lastDummyAlign0 :: AlignDirection
  , colour :: String
  , fieldValue :: Maybe Pair
  , helpUrl :: String
  , inputsInline :: Maybe Boolean
  , nextStatement :: Maybe String
  , output :: Maybe String
  , previousStatement :: Maybe String
  , tooltip :: Maybe String
  , extensions :: Array String
  , mutator :: Maybe String
  | r
  )

newtype BlockDefinition
  = BlockDefinition (Record (BasicBlockDefinition (type :: String)))

derive instance newtypeBlockDefinition :: Newtype BlockDefinition _

instance writeForeignBlockDefinition :: WriteForeign BlockDefinition where
  writeImpl (BlockDefinition fields) = JSON.write fields

defaultBlockDefinition ::
  { extensions :: Array String
  , lastDummyAlign0 :: AlignDirection
  , args0 :: Array Arg
  , fieldValue :: Maybe Pair
  , helpUrl :: String
  , inputsInline :: Maybe Boolean
  , mutator :: Maybe String
  , nextStatement :: Maybe String
  , output :: Maybe String
  , previousStatement :: Maybe String
  , tooltip :: Maybe String
  }
defaultBlockDefinition =
  { fieldValue: Nothing
  , lastDummyAlign0: AlignLeft
  , args0: []
  , helpUrl: ""
  , inputsInline: Just true
  , nextStatement: Nothing
  , output: Nothing
  , previousStatement: Nothing
  , tooltip: Nothing
  , extensions: []
  , mutator: Nothing
  }

typedArguments :: BlockDefinition -> Array { name :: String, check :: String }
typedArguments (BlockDefinition { args0 }) = catMaybes $ argType <$> args0

xml :: forall p i. Node (id :: String, style :: String) p i
xml = element (ElemName "xml")

block
  :: forall p i
   . Node (id :: String, type :: String, x :: String, y :: String) p i
block = element (ElemName "block")

blockType :: forall i r. String -> IProp (type :: String | r) i
blockType = attr (AttrName "type")

style :: forall i r. String -> IProp (style :: String | r) i
style = attr (AttrName "style")

x :: forall i r. String -> IProp (x :: String | r) i
x = attr (AttrName "x")

y :: forall i r. String -> IProp (y :: String | r) i
y = attr (AttrName "y")
