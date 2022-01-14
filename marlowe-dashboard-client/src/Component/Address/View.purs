module Component.Address.View
  ( defaultInput
  , render
  ) where

import Component.Address.Types (Input)
import Component.Icons (icon)
import Component.Icons as Icon
import Component.Input.View as Input
import Component.Label.View as Label
import Data.Address (Address)
import Data.Address as A
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onClick_)

defaultInput :: Address -> Input
defaultInput value =
  { inputId: "address"
  , label: "Wallet address"
  , value
  }

render :: forall w. Input -> HH.HTML w Address
render { inputId, label, value } =
  let
    inputInput = Input.defaultInput { id = inputId, value = A.toString value }
  in
    Input.renderWithChildren inputInput \input ->
      [ Label.render Label.defaultInput { for = inputId, text = label }
      , input
      , HH.button
          [ classNames
              [ "cursor-pointer", "h-4", "flex", "items-center", "self-center" ]
          , onClick_ value
          ]
          [ icon Icon.Copy [ "w-6" ] ]
      ]
