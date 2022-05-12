module Component.CurrencyInput.State (component) where

import Prologue

import Component.CurrencyInput.Types (Action(..), Component, DSL, Message(..))
import Component.CurrencyInput.View (render)
import Data.Array (replicate, (!!))
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Foldable (for_)
import Data.Maybe (maybe)
import Data.String (Pattern(..), Replacement(..), length, replace, split)
import Data.String.CodeUnits (dropRight, fromCharArray)
import Halogen (gets)
import Halogen as H
import Pretty (showBigIntAsCurrency)

component
  :: forall query m
   . Component query m
component = H.mkComponent
  { initialState: deriveState
  , render
  , eval:
      H.mkEval
        ( H.defaultEval
            { handleAction = handleAction
            , receive = (\input -> pure $ Receive input.value)
            }
        )
  }
  where
  deriveState input =
    { value: fixDecimals input.numDecimals
        (showBigIntAsCurrency input.value $ max 0 input.numDecimals)
    , classList: input.classList
    , prefix: input.prefix
    , numDecimals: input.numDecimals
    }

handleAction
  :: forall m
   . Action
  -> DSL m Unit
handleAction (Receive value) = do
  numDecimals <- gets _.numDecimals
  H.modify_ (_ { value = showBigIntAsCurrency value $ max 0 numDecimals })

handleAction (ChangeValue newValue) = do
  numDecimals <- gets _.numDecimals
  let
    mParsed = parseInput numDecimals newValue
  for_ mParsed \parsedValue -> do
    --    H.modify_ (_ { value = parsedValue })
    H.raise $ ValueChanged parsedValue

parseInput :: Int -> String -> Maybe BigInt
parseInput numDecimals =
  BigInt.fromString
    <<< replace dot (Replacement "")
    <<< fixDecimals numDecimals

fixDecimals :: Int -> String -> String
fixDecimals numDecimals value = case compare providedDecimals numDecimals of
  GT -> dropRight (providedDecimals - numDecimals) value
  LT -> pad (numDecimals - providedDecimals) value
  EQ -> value
  where
  providedDecimals = maybe 0 length $ split dot value !! 1

pad :: Int -> String -> String
pad num value = value <> fromCharArray (replicate num '0')

dot :: Pattern
dot = Pattern "."
