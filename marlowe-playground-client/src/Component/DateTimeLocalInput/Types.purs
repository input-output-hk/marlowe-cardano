module Component.DateTimeLocalInput.Types where

import Data.DateTime (DateTime)
import Halogen as H

data Message = ValueChanged DateTime

data Action = ChangeValue String

type Input =
  { classList :: Array String
  -- ^ Optional classes to style the component
  , value :: DateTime
  -- ^ Initial value
  , trimSeconds :: Boolean
  -- ^ Wether the component should use a component with seconds
  --   or discard them.
  }

type State =
  { value :: String
  , classList :: Array String
  , trimSeconds :: Boolean
  }

type ChildSlots :: forall k. Row k
type ChildSlots = ()

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type Component query m = H.Component query Input Message m

type DSL m a =
  H.HalogenM State Action ChildSlots Message m a

data Query (a :: Type)

type Slot m = H.Slot Query Message m
