module Component.Template.Types where

import Prologue

import Capability.Marlowe (CreateError)
import Component.AddContact.Types as AddContact
import Component.ContractSetup.Types (ContractFields, ContractParams)
import Component.ContractSetup.Types as ContractSetup
import Component.LoadingSubmitButton.Types as LoadingSubmitButton
import Data.NewContract (NewContract)
import Data.PABConnectedWallet (PABConnectedWallet)
import Effect.Aff (Aff)
import Halogen as H
import Language.Marlowe.Core.V1.Semantics.Types (MarloweParams, TokenName)
import Language.Marlowe.Extended.V1.Metadata (ContractTemplate)

type Input = PABConnectedWallet

data Msg
  = Closed
  | ContractStarted NewContract (Aff (Either CreateError MarloweParams))

data Query (a :: Type)

type Component = H.Component Query Input Msg

type ComponentHTML m = H.ComponentHTML Action ChildSlots m

type ChildSlots =
  ( addContact :: AddContact.Slot Unit
  , contractSetup :: ContractSetup.Slot Unit
  , hintSlot :: forall query. H.Slot query Void String
  , submitButtonSlot :: H.Slot LoadingSubmitButton.Query Unit String
  )

type Slot = H.Slot Query Msg

data Wizard
  = Start
  | Overview ContractTemplate (Maybe ContractFields)
  | Setup ContractTemplate ContractSetup.Input
  | AddContact String ContractTemplate ContractSetup.Input
  | Review ContractTemplate ContractParams

derive instance Eq Wizard

type State =
  { wallet :: PABConnectedWallet
  , wizard :: Wizard
  }

data Action
  = OnReset
  | OnBack
  | OnTemplateChosen ContractTemplate
  | OnSetup ContractTemplate (Maybe ContractFields)
  | OpenCreateWalletCard TokenName
  | OnStartContract ContractTemplate ContractParams
  | OnContractSetupMsg ContractSetup.Msg
  | OnAddContactMsg AddContact.Msg
