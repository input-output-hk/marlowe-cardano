module Component.CurrencyInput.Types where

import Data.BigInt.Argonaut (BigInt)
import Halogen as H

data Message = ValueChanged BigInt

data Action
  = ChangeValue String
  | Receive BigInt

type Input =
  { classList :: Array String
  -- ^ Optional classes to style the component
  , value :: BigInt
  -- ^ Initial value
  , prefix :: String
  -- ^ Symbol that represents the currency
  , numDecimals :: Int
  -- ^ Number of decimals
  }

type State =
  { value :: String
  , classList :: Array String
  , prefix :: String
  , numDecimals :: Int
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
