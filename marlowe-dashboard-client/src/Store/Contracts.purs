module Store.Contracts
  ( AwaitingContract(..)
  , ContractStore
  , contractCreated
  , contractStarted
  , followerContractExists
  , getAwaitingContracts
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
  , Prism'
  , filtered
  , iso
  , over
  , preview
  , prism'
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

data AwaitingContract = AwaitingContract MarloweParams MetaData

derive instance Eq AwaitingContract

newtype ContractStore = ContractStore ContractStoreFields

data StartedContract
  = AwaitingFirstUpdate AwaitingContract
  | Ready Execution.State

_AwaitingFirstUpdate :: Prism' StartedContract AwaitingContract
_AwaitingFirstUpdate = prism' AwaitingFirstUpdate case _ of
  AwaitingFirstUpdate ac -> Just ac
  _ -> Nothing

_Ready :: Prism' StartedContract Execution.State
_Ready = prism' Ready case _ of
  Ready es -> Just es
  _ -> Nothing

derive instance Eq StartedContract

type ContractStoreFields =
  {
    -- This map lets you now the status of a contract that is synced with the PAB.
    -- This is what eventually is shown as cards in the Dashboard or as steps
    -- in the Page.Contract
    startedContracts :: Map MarloweParams StartedContract
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

_startedContracts :: Lens' ContractStore (Map MarloweParams StartedContract)
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
  , contractIndex: Bimap.empty
  , contractNicknames: nicknames
  }

initialFollowersReceived
  :: Set (Tuple MarloweParams PlutusAppId) -> ContractStore -> ContractStore
initialFollowersReceived = set _contractIndex <<< Bimap.fromFoldable

contractCreated :: NewContract -> ContractStore -> ContractStore
contractCreated newContract@(NewContract reqId _ _) =
  over _newContracts $ Map.insert reqId newContract

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
    updateIndexes = over _contractIndex $ Bimap.insert marloweParams followerId
    updateSyncedContracts = traverseOf _startedContracts
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
    updateIndexes <$> updateSyncedContracts store

contractStarted
  :: NewContract
  -> MarloweParams
  -> ContractStore
  -- TODO: change String for a proper error type
  -> ContractStore
contractStarted (NewContract reqId nickname metadata) marloweParams =
  over _startedContracts
    ( Map.insert marloweParams
        ( AwaitingFirstUpdate
            $ AwaitingContract marloweParams metadata
        )
    )
    <<< over _newContracts (Map.delete reqId)
    <<< modifyContractNicknames (insertContractNickname marloweParams nickname)

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
  over (_startedContracts <<< ix marloweParams <<< _Ready)

tick :: Instant -> ContractStore -> Either String ContractStore
tick currentTime =
  traverseOf
    ( _startedContracts
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
  (_contractIndex <<< to (Bimap.memberL marloweParams))

getFollowerContract :: MarloweParams -> ContractStore -> Maybe PlutusAppId
getFollowerContract marloweParams = view
  (_contractIndex <<< to (Bimap.lookupL marloweParams))

getContract :: MarloweParams -> ContractStore -> Maybe Execution.State
getContract marloweParams = preview
  (_startedContracts <<< ix marloweParams <<< _Ready)

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
      <<< _Ready
      <<< filtered (not <<< Execution.isClosed)
  )

getClosedContracts :: ContractStore -> Array Execution.State
getClosedContracts = toArrayOf
  ( _startedContracts
      <<< traversed
      <<< _Ready
      <<< filtered Execution.isClosed
  )

getNewContracts :: ContractStore -> Array NewContract
getNewContracts = toArrayOf (_newContracts <<< traversed)

getAwaitingContracts :: ContractStore -> Array AwaitingContract
getAwaitingContracts = toArrayOf
  (_startedContracts <<< traversed <<< _AwaitingFirstUpdate)
