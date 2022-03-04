module Store.Contracts
  ( ContractStore
  , addFollowerContract
  , addStartingContract
  , emptyContractStore
  , followerContractExists
  , getClosedContracts
  , getContract
  , getContractNickname
  , getContractNicknames
  , getFollowerContract
  , getNewContract
  , getNewContracts
  , getRunningContracts
  , mkContractStore
  , modifyContract
  , modifyContractNicknames
  , swapStartingToStartedContract
  , tick
  ) where

import Prologue

import Control.Apply (lift2)
import Data.Array (filter)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.ContractNickname (ContractNickname)
import Data.DateTime.Instant (Instant)
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
  , (^.)
  )
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.LocalContractNicknames
  ( LocalContractNicknames
  , emptyLocalContractNicknames
  , insertContractNickname
  )
import Data.LocalContractNicknames as LocalContractNicknames
import Data.Map (Map)
import Data.Map as Map
import Data.NewContract (NewContract(..))
import Data.UUID.Argonaut (UUID)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Client (_chParams)
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
  , newContracts :: Map UUID NewContract
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

_newContracts :: Lens' ContractStore (Map UUID NewContract)
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
  :: NewContract
  -> ContractStore
  -> ContractStore
addStartingContract newContract@(NewContract reqId _ _) =
  over _newContracts $ Map.insert reqId newContract

addFollowerContract
  :: Instant
  -> PlutusAppId
  -> MetaData
  -> ContractHistory
  -> ContractStore
  -- TODO: change String for a proper error type
  -> Either String ContractStore
addFollowerContract currentTime followerId metadata history store =
  let
    marloweParams = history ^. _chParams <<< _1
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
  in
    updateIndexes <$> updateSyncedContracts store

swapStartingToStartedContract
  :: NewContract
  -> Instant
  -> PlutusAppId
  -> ContractHistory
  -> ContractStore
  -- TODO: change String for a proper error type
  -> Either String ContractStore
swapStartingToStartedContract
  (NewContract reqId nickname metadata)
  currentTime
  followerId
  history
  store =
  let
    marloweParams = history ^. _chParams <<< _1
    store' =
      store
        # modifyContractNicknames
            (insertContractNickname marloweParams nickname)
        # over _newContracts (Map.delete reqId)
  in
    addFollowerContract currentTime followerId metadata history store'

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
getRunningContracts = filter (not <<< Execution.isClosed)
  <<< map snd
  <<< Map.toUnfoldable
  <<< view _syncedContracts

getClosedContracts :: ContractStore -> Array Execution.State
getClosedContracts = filter Execution.isClosed
  <<< map snd
  <<< Map.toUnfoldable
  <<< view _syncedContracts

getNewContracts :: ContractStore -> Array NewContract
getNewContracts =
  map snd
    <<< Map.toUnfoldable
    <<< view _newContracts
