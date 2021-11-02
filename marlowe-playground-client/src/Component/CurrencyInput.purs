module Component.CurrencyInput where

import Prologue hiding (div)
import Control.MonadZero (guard)
import Data.Array (filter)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Compactable (compact)
import Data.String (trim)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse_)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (Hook, OutputToken, StateId, UseEffect)
import Halogen.Hooks as Hooks
import Pretty (showBigIntAsCurrency)

type Input
  = { classList :: Array String
    , value :: BigInt
    , prefix :: String
    , numDecimals :: Int
    }

currencyInput :: forall a m. H.Component a Input BigInt m
currencyInput =
  Hooks.component \{ outputToken } input -> Hooks.do
    Tuple value valueId <- Hooks.useState input.value
    useInputEffect valueId input
    useChangeEffect outputToken value
    Hooks.pure do
      let
        { classList, prefix, numDecimals } = input
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
                <> classList
            )
        ]
        $ compact
            [ guard (trim prefix /= "")
                $> HH.div
                    [ classNames
                        [ "flex-none"
                        , "px-2"
                        , "py-0"
                        , "box-border"
                        , "self-center"
                        ]
                    ]
                    [ HH.text prefix ]
            , Just
                $ HH.input
                    [ classNames
                        [ "flex-1"
                        , "px-1"
                        , "box-border"
                        , "self-stretch"
                        , "border-0"
                        , "outline-none"
                        ]
                    , HE.onValueChange $ handleChange $ Hooks.put valueId
                    , HP.type_ HP.InputNumber
                    , HP.value $ showBigIntAsCurrency value $ max 0 numDecimals
                    ]
            ]

useInputEffect :: forall m. StateId BigInt -> Input -> Hook m UseEffect Unit
useInputEffect valueId { value } =
  Hooks.captures { value } Hooks.useTickEffect do
    Hooks.put valueId value
    pure Nothing

useChangeEffect :: forall m. OutputToken BigInt -> BigInt -> Hook m UseEffect Unit
useChangeEffect outputToken value =
  Hooks.captures { value } Hooks.useTickEffect do
    Hooks.raise outputToken value
    pure Nothing

handleChange ::
  forall m. Applicative m => (BigInt -> m Unit) -> String -> m Unit
handleChange cb =
  traverse_ cb
    <<< BigInt.fromString
    <<< fromCharArray
    <<< filter (_ /= '.')
    <<< toCharArray
