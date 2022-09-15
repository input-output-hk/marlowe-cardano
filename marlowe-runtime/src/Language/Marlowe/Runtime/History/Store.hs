{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}

module Language.Marlowe.Runtime.History.Store where

import Control.Applicative (empty)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVar, readTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Monad (forever, mfilter)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Semialign (Semialign(alignWith))
import Data.These (These(..))
import Data.Type.Equality (testEquality, type (:~:)(..))
import GHC.Show (showSpace)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion(..), assertVersionsEqual)
import Language.Marlowe.Runtime.History.Api (ContractStep, SomeCreateStep)
import Language.Marlowe.Runtime.History.Follower (ContractChanges(..), SomeContractChanges(..))
import Language.Marlowe.Runtime.History.FollowerSupervisor (UpdateContract(..))

-- | Data access methods for managing contract history.
data HistoryQueries m = HistoryQueries
  { findCreateStep   :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , findIntersection :: ContractId -> [BlockHeader] -> m (Maybe Intersection)
  , findNextSteps    :: ContractId -> ChainPoint -> m FindNextStepsResponse
  , commitChanges    :: Map ContractId UpdateContract -> m ()
  }

-- | Change the underlying monad with a natural transformation.
hoistHistoryQueries :: (forall x. m x -> n x) -> HistoryQueries m -> HistoryQueries n
hoistHistoryQueries nat HistoryQueries{..} = HistoryQueries
  { findCreateStep = nat . findCreateStep
  , findIntersection = fmap nat . findIntersection
  , findNextSteps = fmap nat . findNextSteps
  , commitChanges = nat . commitChanges
  }

-- | Dependencies of the history store.
data HistoryStoreDependencies = HistoryStoreDependencies
  { changes        :: STM (Map ContractId UpdateContract)
  -- ^ A transactional source of contract history updates.
  , historyQueries :: HistoryQueries IO
  -- ^ The set of history queries to use.
  }

-- | API of the history store.
data HistoryStore = HistoryStore
  { runHistoryStore   :: IO ()
  -- ^ Run the history store process.
  , findContract      :: ContractId -> IO (Maybe (BlockHeader, SomeCreateStep))
  -- ^ Lookup a contract's creation context by its ID.
  , intersectContract :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> IO (Maybe BlockHeader)
  -- ^ Find the latest common block header from the provided list in a contract's history
  , getNextSteps      :: forall v. ContractId -> MarloweVersion v -> ChainPoint -> IO (GetNextStepsResponse v)
  -- ^ Get the next steps for a contract after the given chain point.
  }

-- | Represents the possible outcomes of a getNextSteps request. This is the
-- type exposed by the history store to its consumers.
data GetNextStepsResponse v
  -- | The chain point in the request has been rolled back (or the contract its
  -- self has).
  = Rollback ChainPoint
  -- | The next steps are not available yet. The last known block of the
  -- contract is provided along with a transactional source of the current
  -- latest point in the contract (when it changes, a new query should be
  -- made).
  | Wait BlockHeader (STM ChainPoint)
  -- | The next steps were found in the provided block.
  | Next BlockHeader [ContractStep v]

-- | Represents the possible outcomes of a findNextSteps request. This is the
-- type exposed by the database to be consumed by the history store.
data FindNextStepsResponse
  -- | The chain point in the request has been rolled back.
  = FindRollback ChainPoint
  -- | The next steps are not yet known. Included is the latest known block in
  -- the contract history.
  | FindWait BlockHeader
  -- | The next steps were found in the provided block.
  | FindNext BlockHeader SomeContractSteps
  deriving (Show, Eq)

-- | A block header with an existentially quantified marlowe version.
data Intersection = forall v. Intersection (MarloweVersion v) BlockHeader

instance Eq Intersection where
  Intersection v1 h1 == Intersection v2 h2 = case testEquality v1 v2 of
    Just Refl -> h1 == h2
    Nothing   -> False

instance Show Intersection where
  showsPrec p (Intersection v h) = showParen (p >= 11)
    ( showString "Intersection"
    . showSpace
    . showsPrec 11 v
    . showSpace
    . showsPrec 11 h
    )

-- | A list of contract steps with an existentially quantified marlowe version.
data SomeContractSteps = forall v. SomeContractSteps (MarloweVersion v) [ContractStep v]

instance Eq SomeContractSteps where
  SomeContractSteps MarloweV1 h1 == SomeContractSteps MarloweV1 h2 = h1 == h2

instance Show SomeContractSteps where
  showsPrec p (SomeContractSteps v h) = showParen (p >= 11)
    ( showString "SomeContractSteps"
    . showSpace
    . showsPrec 11 v
    . showSpace
    . case v of
        MarloweV1 -> showsPrec 11 h
    )

-- | Create a new history store from a set of dependencies.
mkHistoryStore :: HistoryStoreDependencies -> STM HistoryStore
mkHistoryStore HistoryStoreDependencies{..} = do
  -- A transactional cache of the latest block in each contract's history.
  latestBlocksPerContractVar <- newTVar (Map.empty :: Map ContractId (TVar (Maybe BlockHeader)))
  let
    runHistoryStore :: IO ()
    runHistoryStore = forever do
      newChanges <- atomically awaitChanges
      commitChanges newChanges
      atomically $ updateLatestBlocks newChanges

    -- | Waits for non-empty changes to come from the source of changes.
    awaitChanges :: STM (Map ContractId UpdateContract)
    awaitChanges = mfilter (not . Map.null) changes

    -- | Update or add to the latest block TVars in the latest block cache
    -- based on a set of changes.
    updateLatestBlocks :: Map ContractId UpdateContract -> STM ()
    updateLatestBlocks newChanges = do
      latestBlocksPerContract <- readTVar latestBlocksPerContractVar
      writeTVar latestBlocksPerContractVar
        -- Align the current map of latest blocks with the new changes map and
        -- update the values.
        =<< sequence (alignWith updateLatestBlock latestBlocksPerContract newChanges)

    -- | Resolve a new latest block TVar for an update.
    updateLatestBlock :: These (TVar (Maybe BlockHeader)) UpdateContract -> STM (TVar (Maybe BlockHeader))
    updateLatestBlock = \case
      -- This contract was not updated, return the TVar unchanged.
      This latestBlockVar -> pure latestBlockVar
      -- This contract was removed but was not found. Return an empty TVar.
      That RemoveContract -> newTVar Nothing
      -- This contract was just added. Create a new TVar containing the latest
      -- block in the changes.
      That (UpdateContract (SomeContractChanges _ contractChanges)) ->
        newTVar $ latestBlockFromContractChanges contractChanges
      -- This contract was removed. Write 'Nothing' to the existing TVar and
      -- return it.
      These latestBlockVar RemoveContract -> do
        writeTVar latestBlockVar Nothing
        pure latestBlockVar
      -- This contract was updated. Write the maximum of the current block and
      -- the blocks in the changes to the TVar and return it.
      These latestBlockVar (UpdateContract (SomeContractChanges _ contractChanges)) -> do
        modifyTVar latestBlockVar \latestBlock ->
          max latestBlock (latestBlockFromContractChanges contractChanges)
        pure latestBlockVar
      where
        -- Get the latest block from a set of contract changes.
        latestBlockFromContractChanges ContractChanges{..} =
          max (fst <$> create) $ fst <$> listToMaybe (Map.toDescList steps)

    findContract = findCreateStep

    intersectContract :: ContractId -> MarloweVersion v -> [BlockHeader] -> IO (Maybe BlockHeader)
    intersectContract contractId version headers = runMaybeT do
      -- Lookup the intersection via a database query
      Intersection version' blockHeader <- MaybeT $ findIntersection contractId headers
      -- Make sure that the Marlowe Version reported by the query matches the
      -- expected one. We use testEquality here and return Nothing instead of
      -- assertVersionsEqual because the source of the version is an external
      -- user, and therefore can be expected to be possibly false.
      case testEquality version version' of
        Just _  -> pure blockHeader
        Nothing -> empty

    getNextSteps :: ContractId -> MarloweVersion v -> ChainPoint -> IO (GetNextStepsResponse v)
    getNextSteps contractId version point = do
      -- Lookup the latest known block for the contract.
      latestBlocksPerContract <- readTVarIO latestBlocksPerContractVar
      case Map.lookup contractId latestBlocksPerContract of
        -- If not found, tell the client to roll back to genesis
        Nothing -> pure $ Rollback Genesis
        Just latestBlockVar -> do
          -- Lookup the next steps via a database query.
          result <- findNextSteps contractId point
          case result of
            FindRollback point'      -> pure $ Rollback point'
            -- Tell the client to wait and give them read access to the latest
            -- block in the contract so they can wait for it to update and know
            -- when to query again.
            FindWait lastBlockHeader -> pure $ Wait lastBlockHeader $ maybe Genesis At <$> readTVar latestBlockVar
            -- The next steps were found. Assert that the reported version
            -- matches the expected one. Since the version provided to
            -- getNextSteps comes from a trusted source, we perform an
            -- assertion here and throw an error to indicate programmer error.
            FindNext blockHeader (SomeContractSteps version' steps) -> case assertVersionsEqual version' version of
              Refl -> pure $ Next blockHeader steps

  pure HistoryStore{..}
  where
    HistoryQueries{..} = historyQueries
