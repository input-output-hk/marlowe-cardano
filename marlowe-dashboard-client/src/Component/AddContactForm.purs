module Component.AddContactForm where

import Prologue

import Component.Button.Types (Variant(..)) as Button
import Component.Button.View (button)
import Component.Contacts.Types (Action(..), CardSection(..))
import Control.Monad.Rec.Class (class MonadRec)
import Css (button) as Css
import Data.AddressBook (AddressBook)
import Data.AddressBook (addresses, nicknames) as AB
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Forms (address, walletNickname) as Forms
import Halogen (Component)
import Halogen.Css (classNames)
import Halogen.Form (split) as Form
import Halogen.Form.Component (Msg(..), component) as FC
import Halogen.HTML (a, div, slot, text) as HH
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.Hooks (bind, captures, component, pure, raise, useMemo) as Hooks
import Halogen.Hooks.Extra.Hooks (usePutState)
import Type.Prelude (Proxy(..))

initialInput :: Tuple String String
initialInput = Tuple "" ""

component
  :: forall m query
   . MonadAff m
  => MonadRec m
  => Component
       query
       { addressBook :: AddressBook
       , mTokenName :: Maybe String
       }
       Action
       m
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

    Hooks.pure $ HH.div
      [ classNames
          [ "h-full"
          , "grid"
          , "grid-rows-1fr-auto"
          , "divide-y"
          , "divide-gray"
          ]
      ]
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
