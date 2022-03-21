module Store.Contracts
  ( ContractStore
  , StartedContract(..)
  , _ActivatingFollower
  , _AwaitingFirstUpdate
  , _FollowerActivationFailed
  , _Ready
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
import Data.Array as Array
import Data.ContractNickname (ContractNickname)
import Data.DateTime.Instant (Instant)
import Data.Lens
  ( Lens'
  , Prism'
  , filtered
  , iso
  , over
  , preview
  , prism'
  , to
  , toListOf
  , traverseOf
  , traversed
  , view
  )
import Data.Lens.At (at)
import Data.Lens.Index (ix)
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
import Marlowe.Client (getMarloweParams)
import Marlowe.Execution.State (isClosed, restoreState) as Execution
import Marlowe.Execution.State (timeoutState)
import Marlowe.Execution.Types (State) as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams)
import Type.Proxy (Proxy(..))

newtype ContractStore = ContractStore ContractStoreFields

data StartedContract
  = ActivatingFollower
  | FollowerActivationFailed
  | AwaitingFirstUpdate PlutusAppId
  | Ready Execution.State

derive instance Eq StartedContract

_ActivatingFollower :: Prism' StartedContract Unit
_ActivatingFollower = prism' (const ActivatingFollower) case _ of
  ActivatingFollower -> Just unit
  _ -> Nothing

_FollowerActivationFailed :: Prism' StartedContract Unit
_FollowerActivationFailed = prism' (const FollowerActivationFailed) case _ of
  FollowerActivationFailed -> Just unit
  _ -> Nothing

_AwaitingFirstUpdate :: Prism' StartedContract PlutusAppId
_AwaitingFirstUpdate = prism' AwaitingFirstUpdate case _ of
  AwaitingFirstUpdate appId -> Just appId
  _ -> Nothing

_Ready :: Prism' StartedContract Execution.State
_Ready = prism' Ready case _ of
  Ready state -> Just state
  _ -> Nothing

type ContractStoreFields =
  {
    -- This map lets you now the status of a contract that is synced with the PAB.
    -- This is what eventually is shown as cards in the Dashboard or as steps
    -- in the Page.Contract
    syncedContracts :: Map MarloweParams StartedContract
  -- This Map is used to hold the placeholders for new contracts. The key is the
  -- request id of calling the create endpoint, the value is what is needed to
  -- show a "loading" card.
  -- See UC-CONTRACT-1
  , newContracts :: Map UUID NewContract
  , contractNicknames :: LocalContractNicknames
  }

derive instance Eq ContractStore

------------------------------------------------------------
_ContractStore :: Lens' ContractStore ContractStoreFields
_ContractStore = iso
  (\(ContractStore fields) -> fields)
  (\fields -> ContractStore fields)

_syncedContracts :: Lens' ContractStore (Map MarloweParams StartedContract)
_syncedContracts = _ContractStore <<< prop (Proxy :: _ "syncedContracts")

_newContracts :: Lens' ContractStore (Map UUID NewContract)
_newContracts = _ContractStore <<< prop (Proxy :: _ "newContracts")

_contractNicknames :: Lens' ContractStore LocalContractNicknames
_contractNicknames = _ContractStore <<< prop (Proxy :: _ "contractNicknames")

------------------------------------------------------------
emptyContractStore :: ContractStore
emptyContractStore = ContractStore
  { syncedContracts: Map.empty
  , newContracts: Map.empty
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
    marloweParams = getMarloweParams history
    mContractNickname = getContractNickname marloweParams store
    updateSyncedContracts = traverseOf _syncedContracts
      $
        lift2
          (Map.insert marloweParams)
          ( Ready <$> Execution.restoreState
              followerId
              currentTime
              mContractNickname
              metadata
              history
          )
          <<< pure
  in
    updateSyncedContracts store

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
    marloweParams = getMarloweParams history
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
modifyContract marloweParams =
  over (_syncedContracts <<< ix marloweParams <<< _Ready)

tick :: Instant -> ContractStore -> Either String ContractStore
tick currentTime =
  traverseOf
    ( _syncedContracts
        <<< traversed
        <<< _Ready
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
  (_syncedContracts <<< to (Map.member marloweParams))

-- TODO refactor usages of `getContract` to handle StartedContract and unify
-- with getFollowerContract.
getFollowerContract :: MarloweParams -> ContractStore -> Maybe StartedContract
getFollowerContract marloweParams =
  preview $ _syncedContracts <<< ix marloweParams

getContract :: MarloweParams -> ContractStore -> Maybe Execution.State
getContract marloweParams = preview
  (_syncedContracts <<< ix marloweParams <<< _Ready)

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
getRunningContracts = Array.fromFoldable <<< toListOf
  ( _syncedContracts
      <<< traversed
      <<< _Ready
      <<< filtered (not <<< Execution.isClosed)
  )

getClosedContracts :: ContractStore -> Array Execution.State
getClosedContracts = Array.fromFoldable <<< toListOf
  ( _syncedContracts
      <<< traversed
      <<< _Ready
      <<< filtered Execution.isClosed
  )

getNewContracts :: ContractStore -> Array NewContract
getNewContracts =
  map snd
    <<< Map.toUnfoldable
    <<< view _newContracts
