module Component.BigIntInput where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BI
import Data.Either (Either(..))
import Data.Lens (over) as L
import Data.Lens.Record (prop) as L
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (info)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.Css (classNames) as HH
import Halogen.HTML (HTML, input) as HH
import Halogen.HTML.Events (onValueInput) as HH
import Halogen.HTML.Properties
  ( InputType(..)
  , StepValue(..)
  , ref
  , step
  , type_
  , value
  ) as HH
import Type.Prelude (Proxy(..))

type Input =
  { classList :: Array String
  -- ^ Number of decimals
  , value :: BigInt
  -- ^ Initial value
  }

data Action
  = ChangeValue String
  | Receive Input

type Output = Either String BigInt

type State =
  { classList :: Array String
  , value :: String
  }

component
  :: forall query m
   . MonadEffect m
  => H.Component query Input Output m
component = H.mkComponent
  { initialState: L.over (L.prop (Proxy :: Proxy "value")) BI.toString
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

handleAction
  :: forall m slots
   . MonadEffect m
  => Action
  -> H.HalogenM State Action slots Output m Unit
handleAction (Receive _) =
  info
    "Input updates are not allowed in input to prevent cursor position changes"
handleAction (ChangeValue s) = do
  case BI.fromString s of
    Just v -> H.raise $ Right v
    Nothing -> H.raise $ Left s
  H.modify_ _ { value = s }

refLabel :: H.RefLabel
refLabel = RefLabel "Component.BigIntInput"

render :: forall w. State -> HH.HTML w Action
render state = HH.input
  [ HH.classNames $ state.classList
  , HH.onValueInput ChangeValue
  , HH.step $ HH.Step 1.0
  , HH.type_ HH.InputNumber
  , HH.value $ state.value
  , HH.ref refLabel
  ]

