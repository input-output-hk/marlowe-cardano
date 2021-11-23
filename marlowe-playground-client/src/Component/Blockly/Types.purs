module Component.Blockly.Types where

import Prologue hiding (div)
import Blockly.Dom (Block)
import Blockly.Internal (BlockDefinition, XML)
import Blockly.Toolbox (Toolbox)
import Blockly.Types as BT
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.List (List)
import Type.Proxy (Proxy(..))
import Halogen (RefLabel(..), SubscriptionId)
import Marlowe.Linter (Warning)

type State
  = { blocklyState :: Maybe BT.BlocklyState
    , errorMessage :: Maybe String
    , blocklyEventSubscription :: Maybe SubscriptionId
    , eventsWhileDragging :: Maybe (List BT.BlocklyEvent)
    -- For some reason the "FinishLoading" event can be triggered when doing an UNDO in some cases
    -- we only need to fire the BlocklyReady once in the lifetime of this component, so we
    -- store a flag to avoid firing it multiple times.
    , blocklyReadyFired :: Boolean
    }

_blocklyState :: Lens' State (Maybe BT.BlocklyState)
_blocklyState = prop (Proxy :: _ "blocklyState")

_errorMessage :: Lens' State (Maybe String)
_errorMessage = prop (Proxy :: _ "errorMessage")

_blocklyEventSubscription :: Lens' State (Maybe SubscriptionId)
_blocklyEventSubscription = prop (Proxy :: _ "blocklyEventSubscription")

_blocklyReadyFired :: Lens' State Boolean
_blocklyReadyFired = prop (Proxy :: _ "blocklyReadyFired")

emptyState :: State
emptyState =
  { blocklyState: Nothing
  , errorMessage: Nothing
  , blocklyEventSubscription: Nothing
  , eventsWhileDragging: Nothing
  , blocklyReadyFired: false
  }

data Query a
  = SetCode String a
  | SetError String a
  | GetWorkspace (XML -> a)
  | LoadWorkspace XML a
  | GetBlockRepresentation (Block -> a)
  | SelectWarning Warning a
  | SetToolbox Toolbox a

data Action
  = Inject String (Array BlockDefinition) Toolbox
  | SetData Unit
  | BlocklyEvent BT.BlocklyEvent
  | ResizeWorkspace
  | Finalize

data Message
  = CodeChange
  | BlockSelection (Maybe { blockId :: String, blockType :: String })
  | BlocklyReady

blocklyRef :: RefLabel
blocklyRef = RefLabel "blockly"
