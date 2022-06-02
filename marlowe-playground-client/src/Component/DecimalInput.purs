module Component.DecimalInput where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal as D
import Data.Either (Either(..))
import Data.Int as I
import Data.Lens (over) as L
import Data.Lens.Record (prop) as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Numbers.Natural (Natural)
import Data.Numbers.Natural as N
import Effect.Class (class MonadEffect)
import Effect.Class.Console (info)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.Css (classNames) as HH
import Halogen.HTML (HTML, input) as HH
import Halogen.HTML.Events (onBlur, onValueInput) as HH
import Halogen.HTML.Properties
  ( InputType(..)
  , StepValue(..)
  , placeholder
  , ref
  , step
  , type_
  , value
  ) as HH
import Math as M
import Type.Prelude (Proxy(..))

type Input =
  { classList :: Array String
  -- ^ Optional classes to style the component
  , precision :: Natural
  -- ^ Number of decimals
  , value :: Decimal
  -- ^ Initial value
  }

data Action
  = ChangeValue String
  | Receive Input

type Output = Either String Decimal

type State =
  { classList :: Array String
  , precision :: Natural
  , value :: String
  }

component
  :: forall query m
   . MonadEffect m
  => H.Component query Input Output m
component = H.mkComponent
  { initialState: case _ of
      i@{ value } | value == zero -> i { value = "" }
      i -> L.over (L.prop (Proxy :: Proxy "value")) (printDecimal i.precision) i
  , render
  , eval: H.mkEval $
      H.defaultEval
        { handleAction = handleAction
        }
  }

handleAction
  :: forall m slots
   . MonadEffect m
  => Action
  -> H.HalogenM State Action slots Output m Unit
handleAction (ChangeValue s) = do
  case D.fromString s of
    Just v -> H.raise $ Right v
    Nothing -> H.raise $ Left s
  H.modify_ _ { value = s }

handleAction (Receive _) =
  info
    "Input updates are not allowed in input to prevent cursor position changes"

printDecimal :: Natural -> Decimal -> String
printDecimal precision = D.toFixed (N.toInt precision)

refLabel :: H.RefLabel
refLabel = RefLabel "Component.DecimalInput.input"

render :: forall w. State -> HH.HTML w Action
render state = HH.input
  [ HH.classNames $ state.classList
  , HH.onValueInput ChangeValue
  , HH.onBlur $ const $ ChangeValue $ fromMaybe state.value do
      d <- D.fromString state.value
      pure $ printDecimal state.precision d
  , HH.step $ HH.Step (M.pow 10.0 (I.toNumber $ -1 * N.toInt state.precision))
  , HH.placeholder $ printDecimal state.precision $ D.fromInt 0
  , HH.type_ HH.InputNumber
  , HH.value $ state.value
  , HH.ref refLabel
  ]

