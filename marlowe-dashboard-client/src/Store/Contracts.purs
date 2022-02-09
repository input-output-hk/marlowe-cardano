module Store.Contracts
  ( ContractStore
  , addFollowerContract
  , addStartingContract
  , emptyContractStore
  , followerContractExists
  , getFollowerContract
  ) where

import Prologue

import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.ContractNickname (ContractNickname)
import Data.Lens (Lens', _1, iso, over, to, view)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Marlowe.Client (ContractHistory, _chParams)
import Marlowe.Execution.State (restoreState) as Execution
import Marlowe.Execution.Types (State) as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams, Slot)
import Type.Proxy (Proxy(..))

newtype ContractStore = ContractStore ContractStoreFields

type ContractStoreFields
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
_ContractStore :: Lens' ContractStore ContractStoreFields
_ContractStore = iso
  (\(ContractStore fields) -> fields)
  (\fields -> ContractStore fields)

_syncedContracts :: Lens' ContractStore (Map MarloweParams Execution.State)
_syncedContracts = _ContractStore <<< prop (Proxy :: _ "syncedContracts")

_newContracts :: Lens' ContractStore (Map UUID (ContractNickname /\ MetaData))
_newContracts = _ContractStore <<< prop (Proxy :: _ "newContracts")

_contractIndex :: Lens' ContractStore (Bimap MarloweParams PlutusAppId)
_contractIndex = _ContractStore <<< prop (Proxy :: _ "contractIndex")

------------------------------------------------------------
emptyContractStore :: ContractStore
emptyContractStore = ContractStore
  { syncedContracts: Map.empty
  , newContracts: Map.empty
  , contractIndex: Bimap.empty
  }

addStartingContract
  :: (UUID /\ ContractNickname /\ MetaData)
  -> ContractStore
  -> ContractStore
addStartingContract (reqId /\ contractNickname /\ metadata) =
  over _newContracts $ Map.insert reqId (contractNickname /\ metadata)

addFollowerContract
  :: Slot
  -> PlutusAppId
  -> ContractHistory
  -> ContractStore
  -> ContractStore
addFollowerContract currentSlot followerId history =
  let
    marloweParams = view (_chParams <<< _1) history
    updateIndexes = over _contractIndex $ Bimap.insert marloweParams followerId
    updateSyncedContracts = over _syncedContracts $ Map.insert marloweParams
      $ Execution.restoreState currentSlot history
  in
    updateIndexes <<< updateSyncedContracts

------------------------------------------------------------

followerContractExists :: MarloweParams -> ContractStore -> Boolean
followerContractExists marloweParams = view
  (_contractIndex <<< to (Bimap.memberL marloweParams))

getFollowerContract :: MarloweParams -> ContractStore -> Maybe PlutusAppId
getFollowerContract marloweParams = view
  (_contractIndex <<< to (Bimap.lookupL marloweParams))
