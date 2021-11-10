module Component.CurrencyInput where

import Prologue hiding (div)
import Control.MonadZero (guard)
import Data.Array (replicate, (!!))
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..), length, replace, split, trim)
import Data.String.CodeUnits (dropRight, fromCharArray)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Pretty (showBigIntAsCurrency)

type Input =
  { classList :: Array String
  , value :: BigInt
  , prefix :: String
  , numDecimals :: Int
  }

currencyInput :: forall a m. H.Component a Input BigInt m
currencyInput =
  Hooks.component \{ outputToken } input@{ classList, prefix, numDecimals } ->
    Hooks.do
      Tuple value valueId <- Hooks.useState $ showBigIntAsCurrency input.value $
        max 0 numDecimals
      Hooks.captures { value } Hooks.useTickEffect do
        let
          parsed = parseInput numDecimals value
        Hooks.put valueId
          $ flip showBigIntAsCurrency (max 0 numDecimals)
          $ fromMaybe zero parsed
        traverse_ (Hooks.raise outputToken) $ filter (_ /= input.value) parsed
        pure Nothing
      Hooks.pure do
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
                      , HE.onValueChange $ Hooks.put valueId
                      , HP.type_ HP.InputNumber
                      , HP.value value
                      ]
              ]
  where
  parseInput numDecimals =
    BigInt.fromString
      <<< replace dot (Replacement "")
      <<< fixDecimals numDecimals

  fixDecimals numDecimals value = case compare providedDecimals numDecimals of
    GT -> dropRight (providedDecimals - numDecimals) value
    LT -> pad (numDecimals - providedDecimals) value
    EQ -> value
    where
    providedDecimals = maybe 0 length $ split dot value !! 1

  pad num value = value <> fromCharArray (replicate num '0')

  dot = Pattern "."
