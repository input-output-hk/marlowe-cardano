module Component.CurrencyInput.State (component) where

import Prologue

import Component.CurrencyInput.Types (Action(..), Component, DSL, Input, State)
import Component.CurrencyInput.View (render)
import Data.Decimal as D
import Data.Numbers.Natural as N
import Effect.Class (class MonadEffect)
import Halogen as H

component
  :: forall query m
   . MonadEffect m
  => Component query m
component = H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

deriveState :: Input -> State
deriveState input = do
  let
    toRatio = map \precision ->
      { ratio: D.fromInt 10 `D.pow` D.fromInt (-1 * N.toInt precision)
      , precision
      }
  { amountInMinor: input.amountInMinor
  , classList: input.classList
  , currencySymbol: input.currencySymbol
  , amountParseError: Nothing
  , majorCurrencyRatio: toRatio input.majorCurrencyFactor
  }

handleAction
  :: forall m
   . MonadEffect m
  => Action
  -> DSL m Unit
handleAction (Receive input) = do
  state <- H.get
  let
    state' = deriveState input
  when (state /= state') $
    H.put state'

handleAction (ChangeValue value) = do
  H.modify_ _ { amountInMinor = value, amountParseError = Nothing }
  H.raise value

handleAction (AmountParseError err) = do
  H.modify_ _ { amountParseError = Just err }
  pure unit

