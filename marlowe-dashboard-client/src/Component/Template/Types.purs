module Component.Template.Types
  ( State(..)
  , Input
  , Action(..)
  ) where

import Prologue

import Component.ContractSetupForm (ContractParams)
import Data.AddressBook (AddressBook)
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.Semantics (TokenName)

data State
  = Start
  | Overview ContractTemplate
  | Setup ContractTemplate (Maybe ContractParams)
  | Review ContractTemplate ContractParams

derive instance Eq State

type Input =
  { addressBook :: AddressBook
  }

data Action
  = OnReset
  | OnTemplateChosen ContractTemplate
  | OnSetup ContractTemplate (Maybe ContractParams)
  | OpenCreateWalletCard TokenName
  | OnReview ContractTemplate ContractParams
  | OnStartContract ContractTemplate ContractParams
  | OnBack
