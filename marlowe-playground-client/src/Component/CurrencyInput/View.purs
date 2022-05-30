module Component.CurrencyInput.View (render) where

import Prologue

import Component.BigIntInput as BigIntInput
import Component.CurrencyInput.Types (Action(..), ComponentHTML, State)
import Component.DecimalInput as DecimalInput
import Contrib.Data.Decimal (fromBigInt, toBigInt) as D
import Data.BigInt.Argonaut (BigInt)
import Data.Decimal (Decimal)
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Maybe (isJust)
import Data.Monoid (guard)
import Effect.Class (class MonadEffect)
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Type.Prelude (Proxy(..))

render
  :: forall m
   . MonadEffect m
  => State
  -> ComponentHTML m
render state = do
  let
    hasErrors = isJust state.amountParseError
  HH.div
    [ classNames
        ( [ "bg-gray-light"
          , "flex"
          , "items-center"
          , "border-solid"
          , "border"
          , "rounded-sm"
          , "overflow-hidden"
          , "box-border"
          , "focus-within:ring-1"
          , "focus-within:ring-black"
          ]
            <> guard
              hasErrors
              [ "text-[color:red]" ]
            <> state.classList
        )
    ]
    $
      ( flip foldMap state.currencySymbol \symbol -> pure $
          HH.div
            [ classNames $
                [ "flex-none"
                , "px-2"
                , "py-0"
                , "box-border"
                , "self-center"
                ]
            ]
            [ HH.text symbol ]
      )
        <> [ renderInput state ]

renderInput
  :: forall m
   . MonadEffect m
  => State
  -> ComponentHTML m
renderInput state = case state.majorCurrencyRatio of
  Nothing ->
    HH.slot (Proxy :: Proxy "bigIntInput") unit BigIntInput.component
      { classList
      , value: state.amountInMinor
      }
      (either AmountParseError ChangeValue)
  Just { precision, ratio } -> do
    let
      amountInMajor = D.fromBigInt state.amountInMinor * ratio

      toMinor :: Decimal -> BigInt
      toMinor a = D.toBigInt $ a / ratio

    HH.slot (Proxy :: Proxy "decimalInput") unit DecimalInput.component
      { classList
      , precision
      , value: amountInMajor
      }
      (either AmountParseError (ChangeValue <<< toMinor))
  where
  classList =
    [ "flex-1"
    , "px-1"
    , "box-border"
    , "self-stretch"
    , "border-0"
    , "outline-none"
    ]
