module Component.Contacts.State
  ( component
  , adaToken
  , getAda
  ) where

import Prologue

import Capability.Marlowe (class ManageMarlowe)
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.AddContact.Types as AddContact
import Component.Contacts.Lenses (_cardSection)
import Component.Contacts.Types
  ( Action(..)
  , CardSection(..)
  , ChildSlots
  , Component
  , Msg(..)
  , State
  )
import Component.Contacts.View (contactsCard)
import Control.Monad.Reader (class MonadAsk)
import Data.AddressBook as AddressBook
import Data.BigInt.Argonaut (BigInt)
import Data.Lens (assign)
import Data.Map (lookup)
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen as H
import Halogen.Component.Reactive (defaultReactiveEval, mkReactiveComponent)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Marlowe.Semantics (Assets, CurrencySymbol, Token(..), TokenName)
import Store as Store
import Toast.Types (successToast)

type HalogenM = H.HalogenM State Action ChildSlots Msg

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => Toast m
  => MonadClipboard m
  => Component m
component = connect (selectEq _.addressBook) $ mkReactiveComponent
  { render: contactsCard
  , deriveState: \{ context, input } mState ->
      { cardSection: maybe Home _.cardSection mState
      , wallet: input
      , addressBook: context
      }
  , eval: defaultReactiveEval
      { handleAction = handleAction
      }
  }

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => Toast m
  => MonadClipboard m
  => Action
  -> HalogenM m Unit
handleAction CloseContactsCard = H.raise Closed

handleAction (SetCardSection cardSection) = do
  assign _cardSection cardSection

-- TODO SCP-3468 Restore add contact from contract form
handleAction (OnAddContactMsg (AddContact.SaveClicked { nickname, address })) =
  do
    updateStore $ Store.ModifyAddressBook (AddressBook.insert nickname address)
    addToast $ successToast "Contact added"
    H.raise Closed

handleAction (OnAddContactMsg AddContact.BackClicked) = do
  assign _cardSection Home

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"

------------------------------------------------------------
-- The cardano Blockchain has Multi-Asset support, each monetary policy is identified
-- by a policyId which is also known as the CurrencySymbol. The ADA token is a special
-- case that has the empty string as both the token name and the currency symbol
-- https://docs.cardano.org/native-tokens/learn
-- https://github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs#L45
-- TODO: We should probably move this to the Semantic module (marlowe-web-common), or a common Ledger module (web-common?), but definitely not
--       the contacts module.
adaCurrencySymbol :: CurrencySymbol
adaCurrencySymbol = ""

adaTokenName :: TokenName
adaTokenName = ""

adaToken :: Token
adaToken = Token adaCurrencySymbol adaTokenName

getAda :: Assets -> BigInt
getAda assets = fromMaybe zero $ lookup adaTokenName =<< lookup
  adaCurrencySymbol
  (unwrap assets)
