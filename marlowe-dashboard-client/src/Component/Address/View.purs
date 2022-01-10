module Component.Address.View
  ( defaultInput
  , render
  ) where

import Prologue

import Component.Address.Types (Input)
import Component.Icons (icon)
import Component.Icons as Icon
import Component.Input.View as Input
import Component.Label.View as Label
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onClick_)
import Marlowe.Semantics (PubKeyHash)

defaultInput :: Input
defaultInput =
  { inputId: "walletId"
  , label: "Wallet ID"
  , value: mempty
  }

render :: forall w. Input -> HH.HTML w PubKeyHash
render { inputId, label, value } =
  let
    inputInput = Input.defaultInput { id = inputId, value = value }
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
