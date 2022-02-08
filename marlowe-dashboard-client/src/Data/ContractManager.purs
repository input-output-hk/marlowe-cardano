module Data.ContractManager
  ( ContractManager
  , addStartingContract
  , emptyContractManager
  , followerContractExists
  ) where

import Prologue

import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.ContractNickname (ContractNickname)
import Data.Lens (Lens', iso, over, to, view)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Marlowe.Execution.Types as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams)
import Type.Proxy (Proxy(..))

-- This data type contains information on all the contract a user have

newtype ContractManager = ContractManager ContractManagerFields

type ContractManagerFields
  =
  {
    -- This map lets you now the status of a contract that is synced with the PAB.
    -- This is what eventually is shown as cards in the Dashboard or as steps
    -- in the Page.Contract
    syncedContracts :: Map MarloweParams Execution.State
  -- This Map is used to hold the placeholders for new contracts. The key is the
  -- request id of calling the create endpoint, the value is what is needed to
  -- show a "loading" card.
  -- See UC-FIXME-3208
  , newContracts :: Map UUID (ContractNickname /\ MetaData)
  -- This bimap help us have one Follower contract per Marlowe contract.
  , contractIndex :: Bimap MarloweParams PlutusAppId
  }

------------------------------------------------------------
_ContractManager :: Lens' ContractManager ContractManagerFields
_ContractManager = iso
  (\(ContractManager fields) -> fields)
  (\fields -> ContractManager fields)

-- _syncedContracts :: Lens' ContractManager (Map MarloweParams Execution.State)
-- _syncedContracts = _ContractManager <<< prop (Proxy :: _ "syncedContracts")

_newContracts :: Lens' ContractManager (Map UUID (ContractNickname /\ MetaData))
_newContracts = _ContractManager <<< prop (Proxy :: _ "newContracts")

_contractIndex :: Lens' ContractManager (Bimap MarloweParams PlutusAppId)
_contractIndex = _ContractManager <<< prop (Proxy :: _ "contractIndex")

------------------------------------------------------------
emptyContractManager :: ContractManager
emptyContractManager = ContractManager
  { syncedContracts: Map.empty
  , newContracts: Map.empty
  , contractIndex: Bimap.empty
  }

addStartingContract
  :: (UUID /\ ContractNickname /\ MetaData)
  -> ContractManager
  -> ContractManager
addStartingContract (reqId /\ contractNickname /\ metadata) =
  over _newContracts $ Map.insert reqId (contractNickname /\ metadata)

followerContractExists :: MarloweParams -> ContractManager -> Boolean
followerContractExists marloweParams = view
  (_contractIndex <<< to (Bimap.memberL marloweParams))

