{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.ChainSeekClient
  where

import Cardano.Api (CardanoMode, EraHistory, SystemStart)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, newTQueue, readTQueue, writeTQueue)
import Data.Bifoldable (Bifoldable(bifoldMap))
import Data.Foldable (fold)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These(..))
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Traversable (for)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.Indexer.Types
  (MarloweBlock(..), MarloweUTxO(..), UnspentContractOutput(..), extractMarloweBlock)
import Network.Protocol.Driver (RunClient)

-- | Injectable dependencies for the chain seek client
data ChainSeekClientDependencies = ChainSeekClientDependencies
  { securityParameter :: Int
  -- ^ The protocol security parameter. The maximum number of blocks that can be rolled back.

  , databaseQueries :: DatabaseQueries IO
  -- ^ Implementations of the database queries.

  , runChainSeekClient :: RunClient IO RuntimeChainSeekClient
  -- ^ A function that runs a client of the chain seek protocol.

  , pollingInterval :: NominalDiffTime
  -- ^ How frequently to poll the chain seek server when waiting.

  , marloweScriptHashes :: Set ScriptHash
  -- ^ The set of known marlowe script hashes.

  , systemStart :: SystemStart
  -- ^ The starting type of the blockchain.

  , eraHistory :: EraHistory CardanoMode
  -- ^ The history of era switches in the blockchain.
  }

-- | A change to the chain with respect to Marlowe contracts
data ChainEvent
  -- | A change in which a new block of Marlowe transactions is added to the chain.
  = RollForward MarloweBlock

  -- | A change in which the chain is reverted to a previous point, discarding later blocks.
  | RollBackward ChainPoint

-- | A component that runs a chain seek client to traverse the blockchain and
-- extract blocks of Marlowe transactions. The sequence of changes to the chain
-- can be read by repeatedly running the resulting STM action.
chainSeekClient :: Component IO ChainSeekClientDependencies (STM ChainEvent)
chainSeekClient = component \ChainSeekClientDependencies{..} -> do
  -- Initialize a TQueue for emitting ChainEvents.
  eventQueue <- newTQueue

  -- Return the component's thread action and the action to pull the next chain
  -- event.
  pure
    -- In this component's thread, run the
    ( runChainSeekClient $ client
        (atomically . writeTQueue eventQueue)
        databaseQueries
        securityParameter
        pollingInterval
        marloweScriptHashes
        systemStart
        eraHistory
    , readTQueue eventQueue
    )
  where
  client
    :: (ChainEvent -> IO ())
    -> DatabaseQueries IO
    -> Int
    -> NominalDiffTime
    -> Set ScriptHash
    -> SystemStart
    -> EraHistory CardanoMode
    -> RuntimeChainSeekClient IO ()
  client emit DatabaseQueries{..} securityParameter pollingInterval marloweScriptHashes systemStart eraHistory =
    ChainSeekClient $ pure $ SendMsgRequestHandshake moveSchema $ ClientStHandshake
      { recvMsgHandshakeRejected = \_ -> fail "unsupported chain seek version"
      , recvMsgHandshakeConfirmed = do
          intersectionPoints <- getIntersectionPoints
          let
            clientNextIntersect = ClientStNext
              { recvMsgQueryRejected = \_ _ -> do
                  emit $ RollBackward Genesis
                  let
                    rollbackStates = RollbackStates
                      { currentState = MarloweUTxO mempty mempty
                      , currentBlockNumber = Genesis
                      , previousStates = []
                      }
                  pure $ clientIdle rollbackStates
              , recvMsgRollForward = \_ point tip -> do
                  emit $ RollBackward point
                  let
                    tipBlockNo = case tip of
                      Genesis -> -1
                      At BlockHeader{..} -> fromIntegral blockNo
                    isYoungerThanSecurityParameter BlockHeader{blockNo} =
                      tipBlockNo - fromIntegral blockNo <= securityParameter
                    utxoHistoryRange = takeWhile isYoungerThanSecurityParameter
                      $ drop 1
                      $ dropWhile ((> point) . At)
                      $ sortOn Down intersectionPoints
                  rollbackStates <- runConcurrently do
                    currentState <- case point of
                      Genesis -> pure $ MarloweUTxO mempty mempty
                      At block -> Concurrently $ getMarloweUTxO block
                    previousStates <- for  utxoHistoryRange \block ->
                      Concurrently $ (At $ blockNo block,) <$> getMarloweUTxO block
                    pure RollbackStates
                      { currentState
                      , currentBlockNumber = blockNo <$> point
                      , previousStates
                      }
                  pure $ clientIdle rollbackStates
              , recvMsgRollBackward = \_ _ -> pure clientIdleIntersect
              , recvMsgWait = pollWithNext clientNextIntersect
              }
            clientIdleIntersect = SendMsgQueryNext (Intersect intersectionPoints) clientNextIntersect
          pure clientIdleIntersect
      }
      where
      pollWithNext
        :: ClientStNext Move err res ChainPoint (WithGenesis BlockHeader) IO a
        -> IO (ClientStPoll Move err res ChainPoint (WithGenesis BlockHeader) IO a)
      pollWithNext next = do
        threadDelay $ floor $ 1_000_000 * nominalDiffTimeToSeconds pollingInterval
        pure $ SendMsgPoll next

      clientIdle
        :: RollbackStates MarloweUTxO
        -> ClientStIdle Move ChainPoint (WithGenesis BlockHeader) IO a
      clientIdle states@RollbackStates{..} = SendMsgQueryNext (mkQuery currentState) (clientNext states)

      mkQuery :: MarloweUTxO -> Move (These FindTxsToError (Map TxOutRef UTxOError)) (These (Set Transaction) (Map TxOutRef Transaction))
      mkQuery MarloweUTxO{..} = Fork (FindTxsTo $ Set.map ScriptCredential marloweScriptHashes) (FindConsumingTxs allUnspentOutputs)
        where
          allUnspentOutputs = unspentContractTxOuts <> unspentPayoutTxOuts
          unspentContractTxOuts = Set.fromList $ (\UnspentContractOutput{..} -> txOutRef) <$> Map.elems unspentContractOutputs
          unspentPayoutTxOuts = fold unspentPayoutOutputs

      clientNext
        :: RollbackStates MarloweUTxO
        -> ClientStNext
            Move
            (These FindTxsToError (Map TxOutRef UTxOError))
            (These (Set Transaction) (Map TxOutRef Transaction))
            ChainPoint
            (WithGenesis BlockHeader)
            IO
            a
      clientNext states = ClientStNext
        { recvMsgQueryRejected = \err _ -> fail $ "Query rejected by chainseekd: " <> show err

        , recvMsgRollForward = \result point tip -> do
            block <- case point of
              Genesis -> fail "Rolled forward to Genesis"
              At block -> pure block
            tipBlock <- case tip of
              Genesis -> fail "Unexpected tip at Genesis"
              At tipBlock -> pure tipBlock
            let txs = bifoldMap id (Set.fromList . Map.elems) result
            marloweUTxO <- case extractMarloweBlock systemStart eraHistory marloweScriptHashes block txs $ currentState states of
              Nothing -> pure $ currentState states
              Just (marloweUTxO, marloweBlock) -> do
                emit $ RollForward marloweBlock
                pure marloweUTxO
            pure $ clientIdle $ pushState securityParameter (blockNo block) (blockNo tipBlock) marloweUTxO states

        , recvMsgRollBackward = \point _ -> do
            emit $ RollBackward point
            pure $ clientIdle $ rollback (blockNo <$> point) states

        , recvMsgWait = pollWithNext $ clientNext states
        }

-- A current state with a collection of previous states
data RollbackStates a = RollbackStates
  { currentState :: a
  , currentBlockNumber :: WithGenesis BlockNo
  , previousStates :: [(WithGenesis BlockNo, a)]
  }

-- Add a new state at the given block number to the collection, moving the
-- current state to the previous states.
pushState :: Int -> BlockNo -> BlockNo -> a -> RollbackStates a -> RollbackStates a
pushState securityParameter blockNo tip state RollbackStates{..} = RollbackStates
  { currentState = state
  , currentBlockNumber = At blockNo
  , previousStates = takeWhile isYoungerThanSecurityParameter $ (currentBlockNumber, currentState) : previousStates
  }
  where
    isYoungerThanSecurityParameter (Genesis, _) = fromIntegral tip <= securityParameter
    isYoungerThanSecurityParameter (At blockNo', _) = fromIntegral (tip - blockNo') <= securityParameter

-- Rollback the state collection to a previous state. Throws an exception if
-- there are no previous states to rollback to.
rollback :: WithGenesis BlockNo -> RollbackStates a -> RollbackStates a
rollback rollbackBlock states@RollbackStates{..}
  | currentBlockNumber <= rollbackBlock = states
  | otherwise = case previousStates of
      [] -> error "No previous states to rollback"
      (currentBlockNumber', currentState') : previousStates' -> rollback rollbackBlock RollbackStates
        { currentState = currentState'
        , currentBlockNumber = currentBlockNumber'
        , previousStates = previousStates'
        }
