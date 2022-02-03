module Component.Template.Types
  ( State(..)
  , Action(..)
  ) where

import Prologue

import Component.ContractSetupForm (ContractParams)
import Component.ContractSetupForm as ContractSetupForm
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.Semantics (TokenName)

data State
  = Start
  | Overview ContractTemplate
  | Setup ContractTemplate ContractSetupForm.Input
  | Review ContractTemplate ContractParams

derive instance Eq State

data Action
  = OnReset
  | OnTemplateChosen ContractTemplate
  | OnSetup ContractTemplate (Maybe ContractParams)
  | OpenCreateWalletCard TokenName
  | OnReview ContractTemplate ContractParams
  | OnStartContract ContractTemplate ContractParams
  | OnBack
