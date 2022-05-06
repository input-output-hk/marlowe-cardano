module Component.Contacts.State
  ( component
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
import Data.Lens (assign)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen as H
import Halogen.Component.Reactive (fromHandleAction, mkReactiveComponent)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
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
  , deriveState: const unit
  , initialTransient: Home
  , eval: fromHandleAction handleAction
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

handleAction (OnAddContactMsg (AddContact.SaveClicked _)) =
  H.raise Closed

handleAction (OnAddContactMsg AddContact.BackClicked) = do
  assign _cardSection Home

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"
