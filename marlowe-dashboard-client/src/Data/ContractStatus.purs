module Data.ContractStatus where

import Prelude

import Data.UUID.Argonaut (UUID)
import Language.Marlowe.Core.V1.Semantics.Types (MarloweParams)

-- This data type help us keep different data for the different status
-- of a contract.
data ContractStatus starting started
  = Starting starting
  | Started started

derive instance
  ( Eq starting
  , Eq started
  ) =>
  Eq (ContractStatus starting started)

derive instance
  ( Ord starting
  , Ord started
  ) =>
  Ord (ContractStatus starting started)

-- When we want to reference a Starting contract we use the UUID
-- of the request, once the contract has started we use the MarloweParams
type ContractStatusId = ContractStatus UUID MarloweParams
