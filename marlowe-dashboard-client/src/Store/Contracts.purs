module Store.Contracts
  ( ContractStore
  , contractCreated
  , contractStarted
  , followerContractExists
  , getClosedContracts
  , getContract
  , getContractNickname
  , getContractNicknames
  , getFollowerContract
  , getNewContract
  , getNewContracts
  , getRunningContracts
  , historyUpdated
  , initialFollowersReceived
  , mkContractStore
  , modifyContract
  , modifyContractNicknames
  , resetContractStore
  , tick
  ) where

import Prologue

import Control.Apply (lift2)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.ContractNickname (ContractNickname)
import Data.DateTime.Instant (Instant)
import Data.Lens
  ( Lens'
  , filtered
  , iso
  , over
  , preview
  , set
  , to
  , toArrayOf
  , traverseOf
  , traversed
  , view
  )
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.LocalContractNicknames
  ( LocalContractNicknames
  , insertContractNickname
  )
import Data.LocalContractNicknames as LocalContractNicknames
import Data.Map (Map)
import Data.Map as Map
import Data.NewContract (NewContract(..))
import Data.Set (Set)
import Data.UUID.Argonaut (UUID)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Client (getMarloweParams)
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
    startedContracts :: Map MarloweParams Execution.State
  -- This Map is used to hold the placeholders for new contracts. The key is the
  -- request id of calling the create endpoint, the value is what is needed to
  -- show a "loading" card.
  -- See UC-CONTRACT-1
  , newContracts :: Map UUID NewContract
  -- A lookup to find the originating request ID for a particular marlowe
  -- params.
  , newMarloweParams :: Map MarloweParams UUID
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

_startedContracts :: Lens' ContractStore (Map MarloweParams Execution.State)
_startedContracts = _ContractStore <<< prop (Proxy :: _ "startedContracts")

_newContracts :: Lens' ContractStore (Map UUID NewContract)
_newContracts = _ContractStore <<< prop (Proxy :: _ "newContracts")

_contractIndex :: Lens' ContractStore (Bimap MarloweParams PlutusAppId)
_contractIndex = _ContractStore <<< prop (Proxy :: _ "contractIndex")

_contractNicknames :: Lens' ContractStore LocalContractNicknames
_contractNicknames = _ContractStore <<< prop (Proxy :: _ "contractNicknames")

resetContractStore :: ContractStore -> ContractStore
resetContractStore (ContractStore { contractNicknames }) =
  mkContractStore contractNicknames

mkContractStore :: LocalContractNicknames -> ContractStore
mkContractStore nicknames = ContractStore
  { startedContracts: Map.empty
  , newContracts: Map.empty
  , newMarloweParams: Map.empty
  , contractIndex: Bimap.empty
  , contractNicknames: nicknames
  }

initialFollowersReceived
  :: Set (Tuple MarloweParams PlutusAppId) -> ContractStore -> ContractStore
initialFollowersReceived = set _contractIndex <<< Bimap.fromFoldable

contractCreated :: NewContract -> ContractStore -> ContractStore
contractCreated newContract@(NewContract reqId _ _) =
  over _newContracts $ Map.insert reqId newContract

-- Called upon receipt of a ContractHistory record from the follower app.
-- May be called before or after `contractStarted` is called, and we have
-- to handle both cases.
historyUpdated
  :: Instant
  -> PlutusAppId
  -> MetaData
  -> ContractHistory
  -> ContractStore
  -- TODO: change String for a proper error type
  -> Either String ContractStore
historyUpdated currentTime followerId metadata history store =
  let
    marloweParams = getMarloweParams history
    mContractNickname = getContractNickname marloweParams store
    updateIndexes = over _contractIndex $ Bimap.insert marloweParams
      followerId
    updateSyncedContracts = traverseOf _startedContracts
      $
        lift2
          (Map.insert marloweParams)
          ( Execution.restoreState
              followerId
              currentTime
              mContractNickname
              metadata
              history
          )
          <<< pure
  in
    removeNewContractIfNoLongerNeeded marloweParams <<< updateIndexes
      <$> updateSyncedContracts store
  where
  removeNewContractIfNoLongerNeeded marloweParams (ContractStore s) =
    -- Was this called before or after `contractStarted`?
    ContractStore case Map.lookup marloweParams s.newMarloweParams of
      -- If not, we don't need to do anything here.
      Nothing -> s
      -- If so, we need to delete the newContract and the request ID mapping.
      Just reqId -> s
        { newContracts = Map.delete reqId s.newContracts
        , newMarloweParams = Map.delete marloweParams s.newMarloweParams
        }

-- Called upon receipt of the MarloweParams when creating a contract.
-- May be called before or after the first call to `historyUpdate`, and we have
-- to handle both cases.
contractStarted
  :: NewContract
  -> MarloweParams
  -> ContractStore
  -> ContractStore
contractStarted (NewContract reqId nickname _) marloweParams =
  removeNewContractIfNoLongerNeeded
    <<< modifyContractNicknames (insertContractNickname marloweParams nickname)
  where
  removeNewContractIfNoLongerNeeded (ContractStore store) =
    -- Was this called before or after `historyUpdated`?
    ContractStore case getContract marloweParams (ContractStore store) of
      -- If not, we need to store the reqId with the marlowe params so that
      -- `historyUpdated` can remove the new contract when it gets called.
      Nothing -> store
        { newMarloweParams =
            Map.insert marloweParams reqId store.newMarloweParams
        }
      -- If so, we can just delete the new contract now. We also need to update
      -- the contract added by `historyUpdated` with the correct nickname.
      Just contract -> store
        { newContracts = Map.delete reqId store.newContracts
        , newMarloweParams = Map.delete marloweParams store.newMarloweParams
        , startedContracts = Map.insert
            marloweParams
            (contract { contractNickname = Just nickname })
            store.startedContracts
        }

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
modifyContract marloweParams = over (_startedContracts <<< ix marloweParams)

tick :: Instant -> ContractStore -> Either String ContractStore
tick currentTime =
  traverseOf
    ( _startedContracts
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
getContract marloweParams = preview (_startedContracts <<< ix marloweParams)

getNewContract :: UUID -> ContractStore -> Maybe NewContract
getNewContract reqId = view
  (_newContracts <<< at reqId)

getContractNicknames :: ContractStore -> LocalContractNicknames
getContractNicknames = view _contractNicknames

getContractNickname :: MarloweParams -> ContractStore -> Maybe ContractNickname
getContractNickname marloweParams =
  LocalContractNicknames.getContractNickname marloweParams <<<
    getContractNicknames

getRunningContracts :: ContractStore -> Array Execution.State
getRunningContracts = toArrayOf
  ( _startedContracts
      <<< traversed
      <<< filtered (not <<< Execution.isClosed)
  )

getClosedContracts :: ContractStore -> Array Execution.State
getClosedContracts = toArrayOf
  ( _startedContracts
      <<< traversed
      <<< filtered Execution.isClosed
  )

getNewContracts :: ContractStore -> Array NewContract
getNewContracts = toArrayOf (_newContracts <<< traversed)
