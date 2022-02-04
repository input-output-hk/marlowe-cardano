module Component.Template.Types
  ( Action(..)
  , State(..)
  ) where

import Prologue

import Component.ContractSetup.Types (ContractFields, ContractParams)
import Component.ContractSetup.Types as ContractSetup
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.Semantics (TokenName)

data State
  = Start
  | Overview ContractTemplate
  | Setup ContractTemplate ContractSetup.Input ContractFields
  | Review ContractTemplate ContractParams

derive instance Eq State

data Action
  = OnReset
  | OnBack
  | OnTemplateChosen ContractTemplate
  | OnSetup ContractTemplate (Maybe ContractParams)
  | OpenCreateWalletCard TokenName
  | OnStartContract ContractTemplate ContractParams
  | OnContractSetupMsg ContractSetup.Msg
