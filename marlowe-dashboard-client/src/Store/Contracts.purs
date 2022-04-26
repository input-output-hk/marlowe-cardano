module Store.Contracts
  ( Action(..)
  , ContractStore
  , reduce
  , followerContractExists
  , getContract
  , getContractNickname
  , getContractNicknames
  , getFollowerContract
  , getNewContract
  , isFollowerContract
  , mkContractStore
  , partitionContracts
  , tick
  ) where

import Prologue

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.ContractNickname (ContractNickname)
import Data.ContractStatus (ContractStatus(..), ContractStatusId)
import Data.DateTime.Instant (Instant)
import Data.Either (either)
import Data.Foldable (find, foldr)
import Data.Lens
  ( Lens'
  , filtered
  , iso
  , over
  , preview
  , set
  , takeBoth
  , to
  , traverseOf
  , traversed
  , view
  , (^.)
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
import Data.Maybe (maybe)
import Data.NewContract (NewContract(..), _newContractError)
import Data.NewContract as NC
import Data.Set (Set)
import Data.Tuple (uncurry)
import Data.UUID.Argonaut (UUID, emptyUUID)
import Language.Marlowe.Client (ContractHistory, MarloweError)
import Marlowe.Client (getMarloweParams)
import Marlowe.Client as Client
import Marlowe.Execution.State (restoreState) as Execution
import Marlowe.Execution.State (timeoutState)
import Marlowe.Execution.Types (State) as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams, TransactionError)
import Type.Proxy (Proxy(..))

newtype ContractStore = ContractStore ContractStoreFields

data Action
  = FollowerAppsActivated (Set (Tuple MarloweParams PlutusAppId))
  | ContractCreated NewContract
  | ContractHistoryUpdated Instant PlutusAppId MetaData ContractHistory
  | ContractNicknameUpdated ContractStatusId ContractNickname
  | ModifySyncedContract MarloweParams (Execution.State -> Execution.State)
  | ContractStarted NewContract MarloweParams
  | ContractStartFailed NewContract MarloweError
  | Reset

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
  , newMarloweParams :: Bimap MarloweParams UUID
  -- This bimap help us have one Follower contract per Marlowe contract.
  , contractIndex :: Bimap MarloweParams PlutusAppId
  , contractNicknames :: LocalContractNicknames
  }

derive instance Eq ContractStore

reduce :: ContractStore -> Action -> ContractStore
reduce store@(ContractStore s) = case _ of
  FollowerAppsActivated followers ->
    followerAppsActivated followers store
  ContractCreated startingContractInfo ->
    contractCreated startingContractInfo store
  ContractHistoryUpdated currentTime followerId metadata history ->
    historyUpdated currentTime followerId metadata history store
  ContractNicknameUpdated (Started marloweParams) nickname ->
    modifyContractNicknames
      (insertContractNickname marloweParams nickname)
      store
  ContractNicknameUpdated (Starting reqId) nickname -> ContractStore s
    { newContracts =
        let
          setNickname (NewContract id _ metadata error contract) =
            NewContract id nickname metadata error contract
        in
          Map.update (Just <<< setNickname) reqId s.newContracts
    , contractNicknames =
        maybe
          s.contractNicknames
          ( \marloweParams -> insertContractNickname
              marloweParams
              nickname
              s.contractNicknames
          )
          $ Bimap.lookupR reqId s.newMarloweParams
    }
  ModifySyncedContract marloweParams f ->
    modifyContract marloweParams f store
  ContractStarted newContract marloweParams ->
    contractStarted newContract marloweParams store
  ContractStartFailed newContract marloweError ->
    contractStartFailed newContract marloweError store
  Reset ->
    mkContractStore $ store ^. _contractNicknames

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

mkContractStore :: LocalContractNicknames -> ContractStore
mkContractStore nicknames = ContractStore
  { startedContracts: Map.empty
  , newContracts: Map.empty
  , newMarloweParams: Bimap.empty
  , contractIndex: Bimap.empty
  , contractNicknames: nicknames
  }

followerAppsActivated
  :: Set (Tuple MarloweParams PlutusAppId) -> ContractStore -> ContractStore
followerAppsActivated apps = over _contractIndex \index ->
  foldr (uncurry Bimap.insert) index apps

contractCreated :: NewContract -> ContractStore -> ContractStore
contractCreated newContract@(NewContract reqId _ _ _ _) =
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
  -> ContractStore
historyUpdated currentTime followerId metadata history store =
  let
    marloweParams = getMarloweParams history
    mNewContract =
      newContractById marloweParams store <|> newContractByMatch store
    updateIndexes = over _contractIndex $ Bimap.insert marloweParams
      followerId
    updateSyncedContracts = traverseOf _startedContracts
      $
        lift2
          (Map.insert marloweParams)
          ( Execution.restoreState
              followerId
              currentTime
              metadata
              history
          )
          <<< pure
    mNickname = getContractNickname marloweParams store
      <|> NC.getContractNickname <$> mNewContract
    updateNicknames = maybe
      identity
      (modifyContractNicknames <<< insertContractNickname marloweParams)
      mNickname
  in
    either
      (const store)
      ( maybe identity (removeNewContract marloweParams) mNewContract
          <<< updateNicknames
          <<< updateIndexes
      )
      (updateSyncedContracts store)
  where
  newContractById marloweParams (ContractStore s) =
    flip Map.lookup s.newContracts
      =<< Bimap.lookupL marloweParams s.newMarloweParams
  newContractByMatch (ContractStore s) =
    find (matchingContract emptyUUID) s.newContracts
  matchingContract _ (NewContract _ _ _ _ contract) =
    contract == Client.getContract history
  removeNewContract
    marloweParams
    (NewContract reqId _ _ _ _)
    (ContractStore s) = ContractStore s
    { newContracts = Map.delete reqId s.newContracts
    , newMarloweParams = Bimap.deleteL marloweParams s.newMarloweParams
    }

-- Called upon receipt of the MarloweParams when creating a contract.
-- May be called before or after the first call to `historyUpdate`, and we have
-- to handle both cases.
contractStarted
  :: NewContract
  -> MarloweParams
  -> ContractStore
  -> ContractStore
contractStarted (NewContract reqId nickname _ _ _) marloweParams =
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
            Bimap.insert marloweParams reqId store.newMarloweParams
        }
      -- If so, we can just delete the new contract now. We also need to record
      -- the contract's nickname in the contract nicknames collection.
      Just _ -> store
        { newContracts = Map.delete reqId store.newContracts
        , newMarloweParams = Bimap.deleteL marloweParams store.newMarloweParams
        }

contractStartFailed
  :: NewContract
  -> MarloweError
  -> ContractStore
  -> ContractStore
contractStartFailed (NewContract reqId _ _ _ _) =
  set (_newContracts <<< ix reqId <<< _newContractError) <<< Just

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

tick :: Instant -> ContractStore -> Either TransactionError ContractStore
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

isFollowerContract :: PlutusAppId -> ContractStore -> Boolean
isFollowerContract instanceId = view
  (_contractIndex <<< to (Bimap.memberR instanceId))

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

partitionContracts
  :: ContractStore
  -> { started :: Map MarloweParams Execution.State
     , starting :: Map UUID NewContract
     }
partitionContracts store = { started, starting }
  where
  Tuple started starting = store ^. takeBoth _startedContracts _newContracts
