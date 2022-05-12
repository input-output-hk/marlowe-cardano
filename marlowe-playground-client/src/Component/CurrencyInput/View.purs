module Component.CurrencyInput.View (render) where

import Prologue

import Component.CurrencyInput.Types (Action(..), ComponentHTML, State)
import Control.MonadZero (guard)
import Data.Compactable (compact)
import Data.String (trim)
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render
  :: forall m
   . State
  -> ComponentHTML m
render state =
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
            <> state.classList
        )
    ]
    $ compact
        [ guard (trim state.prefix /= "")
            $> HH.div
              [ classNames
                  [ "flex-none"
                  , "px-2"
                  , "py-0"
                  , "box-border"
                  , "self-center"
                  ]
              ]
              [ HH.text state.prefix ]
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
                , HE.onValueInput $ ChangeValue
                , HP.type_ HP.InputNumber
                , HP.value state.value
                ]
        ]
