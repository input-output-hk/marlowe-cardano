module Component.Contacts.State
  ( initialState
  , handleAction
  , adaToken
  , getAda
  ) where

import Prologue

import Capability.MainFrameLoop (callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe)
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.AddContact.Types as AddContact
import Component.Contacts.Lenses (_cardSection)
import Component.Contacts.Types (Action(..), CardSection(..), State)
import Control.Monad.Reader (class MonadAsk)
import Data.AddressBook as AddressBook
import Data.BigInt.Argonaut (BigInt)
import Data.Lens (assign)
import Data.Map (lookup)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Monad (class MonadStore, updateStore)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Semantics (Assets, CurrencySymbol, Token(..), TokenName)
import Page.Dashboard.Types as Dashboard
import Store as Store
import Toast.Types (successToast)

initialState :: State
initialState = { cardSection: Home }

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => Toast m
  => MonadClipboard m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction CloseContactsCard = callMainFrameAction
  $ MainFrame.DashboardAction
  $ Dashboard.CloseCard

handleAction (SetCardSection cardSection) = do
  assign _cardSection cardSection

handleAction
  (OnAddContactMsg mTokenName (AddContact.SaveClicked { nickname, address })) =
  do
    updateStore $ Store.ModifyAddressBook (AddressBook.insert nickname address)
    addToast $ successToast "Contact added"
    case mTokenName of
      -- if a tokenName was also passed, we are inside a template contract and we need to update role
      Just tokenName -> callMainFrameAction $ MainFrame.DashboardAction $
        Dashboard.SetContactForRole tokenName nickname
      -- If we don't have a tokenName, then we added the contact from the contact dialog and we should close the panel
      Nothing -> callMainFrameAction $ MainFrame.DashboardAction $
        Dashboard.CloseCard

handleAction (OnAddContactMsg Nothing AddContact.BackClicked) = do
  assign _cardSection Home

handleAction (OnAddContactMsg _ AddContact.BackClicked) = do
  pure unit -- handled in Dashboard.State

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
