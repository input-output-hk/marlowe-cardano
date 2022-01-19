module Component.AddContactForm where

import Prologue

import Component.Button.Types (Variant(..)) as Button
import Component.Button.View (button)
import Component.Contacts.Types (Action(..), CardSection(..))
import Css (button, primaryButton) as Css
import Data.AddressBook (addresses, nicknames) as AB
import Data.AddressBook (nicknames)
import Data.Maybe (isJust, isNothing)
import Data.Traversable (for)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Forms (address, walletNickname) as Forms
import Halogen.Css (classNames)
import Halogen.Form (split) as Form
import Halogen.Form.Component (Msg(..), component) as FC
import Halogen.HTML (a, button, div, div_, slot, text) as HH
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (disabled) as HP
import Halogen.Hooks (bind, captures, component, pure, raise, useMemo) as Hooks
import Halogen.Hooks.Extra.Hooks (usePutState)
import Type.Prelude (Proxy(..))

initialInput = Tuple "" ""

component = Hooks.component \{ outputToken } { mTokenName, addressBook } ->
  Hooks.do
    result /\ putState <- usePutState Nothing

    form <- Hooks.captures { addressBook } Hooks.useMemo \_ ->
      FC.component
        { form: Form.split
            (Forms.walletNickname $ AB.nicknames addressBook)
            (Forms.address $ AB.addresses addressBook)
        , formClasses: [ "space-y-4", "p-4" ]
        }

    let
      raise = Hooks.raise outputToken
      submit nickname address =
        raise $ SaveWallet mTokenName nickname address

    Hooks.pure $ HH.div_
      [ HH.slot (Proxy :: Proxy "form") unit form initialInput case _ of
          FC.Updated res -> putState res
          FC.Raised saveAction -> raise saveAction
      , HH.div
          [ classNames [ "flex", "gap-4", "p-4" ] ]
          [ HH.a
              [ classNames $ Css.button <> [ "flex-1", "text-center" ]
              , onClick_ case mTokenName of
                  Just _ -> raise CancelNewContactForRole
                  Nothing -> raise $ SetCardSection Home
              ]
              [ HH.text "Back" ]
          , button
              Button.Primary
              (uncurry submit <$> result)
              [ "flex-1" ]
              [ HH.text "Save" ]
          ]
      ]

