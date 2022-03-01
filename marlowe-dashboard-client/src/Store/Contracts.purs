module Store.Contracts
  ( ContractStore
  , addFollowerContract
  , addStartingContract
  , tick
  , emptyContractStore
  , followerContractExists
  , getClosedContracts
  , getContract
  , getContractNickname
  , getContractNicknames
  , getFollowerContract
  , getRunningContracts
  , mkContractStore
  , modifyContract
  , modifyContractNicknames
  ) where

import Prologue

import Control.Apply (lift2)
import Data.Array (filter)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.ContractNickname (ContractNickname)
import Data.DateTime.Instant (Instant)
import Data.Either (note)
import Data.Lens
  ( Lens'
  , _1
  , filtered
  , iso
  , over
  , to
  , traverseOf
  , traversed
  , view
  , (^?)
  )
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.LocalContractNicknames
  ( LocalContractNicknames
  , emptyLocalContractNicknames
  )
import Data.LocalContractNicknames as LocalContractNicknames
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Marlowe.Client (ContractHistory, _chParams)
import Marlowe.Execution.State (isClosed, restoreState) as Execution
import Marlowe.Execution.State (timeoutState)
import Marlowe.Execution.Types (State) as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams)
import Type.Proxy (Proxy(..))

newtype ContractStore = ContractStore ContractStoreFields

type ContractStoreFields =
  {
    -- This map lets you now the status of a contract that is synced with the PAB.
    -- This is what eventually is shown as cards in the Dashboard or as steps
    -- in the Page.Contract
    syncedContracts :: Map MarloweParams Execution.State
  -- This Map is used to hold the placeholders for new contracts. The key is the
  -- request id of calling the create endpoint, the value is what is needed to
  -- show a "loading" card.
  -- See UC-CONTRACT-1
  , newContracts :: Map UUID (ContractNickname /\ MetaData)
  -- This bimap help us have one Follower contract per Marlowe contract.
  , contractIndex :: Bimap MarloweParams PlutusAppId
  , contractNicknames :: LocalContractNicknames
  }

derive instance Eq ContractStore

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

_contractNicknames :: Lens' ContractStore LocalContractNicknames
_contractNicknames = _ContractStore <<< prop (Proxy :: _ "contractNicknames")

------------------------------------------------------------
emptyContractStore :: ContractStore
emptyContractStore = ContractStore
  { syncedContracts: Map.empty
  , newContracts: Map.empty
  , contractIndex: Bimap.empty
  , contractNicknames: emptyLocalContractNicknames
  }

mkContractStore :: LocalContractNicknames -> ContractStore
mkContractStore nicknames = modifyContractNicknames (const nicknames) $
  emptyContractStore

addStartingContract
  :: (UUID /\ ContractNickname /\ MetaData)
  -> ContractStore
  -> ContractStore
addStartingContract (reqId /\ contractNickname /\ metadata) =
  over _newContracts $ Map.insert reqId (contractNickname /\ metadata)

addFollowerContract
  :: Instant
  -> PlutusAppId
  -> MetaData
  -> ContractHistory
  -> ContractStore
  -> Either String ContractStore
addFollowerContract currentTime followerId metadata history store = do
  marloweParams <- note "params not available" $ history ^? _chParams <<< _1
  let
    mContractNickname = getContractNickname marloweParams store
    updateIndexes = over _contractIndex $ Bimap.insert marloweParams followerId
    updateSyncedContracts = traverseOf _syncedContracts
      $
        lift2
          (Map.insert marloweParams)
          ( Execution.restoreState currentTime mContractNickname metadata
              history
          )
          <<< pure
  updateIndexes <$> updateSyncedContracts store

modifyContractNicknames
  :: (LocalContractNicknames -> LocalContractNicknames)
  -> ContractStore
  -> ContractStore
modifyContractNicknames f = over _contractNicknames f

modifyContract
  :: MarloweParams
  -> (Execution.State -> Execution.State)
  -> ContractStore
  -> ContractStore
modifyContract marloweParams f =
  over (_syncedContracts <<< at marloweParams) (map f)

tick :: Instant -> ContractStore -> Either String ContractStore
tick currentTime =
  traverseOf
    ( _syncedContracts
        <<< traversed
        <<< filtered
          ( \executionState ->
              executionState.mNextTimeout /= Nothing
                && executionState.mNextTimeout <= Just currentTime
          )
    )
    (timeoutState currentTime)

------------------------------------------------------------

followerContractExists :: MarloweParams -> ContractStore -> Boolean
followerContractExists marloweParams = view
  (_contractIndex <<< to (Bimap.memberL marloweParams))

getFollowerContract :: MarloweParams -> ContractStore -> Maybe PlutusAppId
getFollowerContract marloweParams = view
  (_contractIndex <<< to (Bimap.lookupL marloweParams))

getContract :: MarloweParams -> ContractStore -> Maybe Execution.State
getContract marloweParams = view
  (_syncedContracts <<< at marloweParams)

getContractNicknames :: ContractStore -> LocalContractNicknames
getContractNicknames = view _contractNicknames

getContractNickname :: MarloweParams -> ContractStore -> Maybe ContractNickname
getContractNickname marloweParams =
  LocalContractNicknames.getContractNickname marloweParams <<<
    getContractNicknames

getRunningContracts :: ContractStore -> Array Execution.State
getRunningContracts = filter (not <<< Execution.isClosed) <<< map snd
  <<< Map.toUnfoldable
  <<< view _syncedContracts

getClosedContracts :: ContractStore -> Array Execution.State
getClosedContracts = filter Execution.isClosed <<< map snd <<< Map.toUnfoldable
  <<< view _syncedContracts
