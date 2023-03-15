{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.ChainSeekClient
  where

import Cardano.Api (SystemStart)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, TVar, atomically, newTQueue, newTVar, readTQueue, readTVar, writeTQueue, writeTVar)
import Control.Exception (finally)
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock(..), MarloweUTxO(..), extractMarloweBlock)
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (SomeClientConnector)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Query.Client (QueryClient, liftQuery)
import Observe.Event.Backend (EventBackend)
import Observe.Event.Explicit (addField, withEvent)

data ChainSeekClientSelector f where
  LoadMarloweUTxO :: ChainSeekClientSelector MarloweUTxO

-- | Injectable dependencies for the chain sync client
data ChainSeekClientDependencies r = ChainSeekClientDependencies
  { databaseQueries :: DatabaseQueries IO
  -- ^ Implementations of the database queries.

  , chainSyncConnector :: SomeClientConnector RuntimeChainSeekClient IO
  -- ^ A connector that connects a client of the chain sync protocol.

  , chainSyncQueryConnector :: SomeClientConnector (QueryClient ChainSyncQuery) IO
  -- ^ A connector that connects a client of the chain sync query protocol.

  , pollingInterval :: NominalDiffTime
  -- ^ How frequently to poll the chain sync server when waiting.

  , marloweScriptHashes :: NESet ScriptHash
  -- ^ The set of known marlowe script hashes.

  , payoutScriptHashes :: NESet ScriptHash
  -- ^ The set of known payout script hashes.

  , eventBackend :: EventBackend IO r ChainSeekClientSelector
  }

-- | A change to the chain with respect to Marlowe contracts
data ChainEvent
  -- | A change in which a new block of Marlowe transactions is added to the chain.
  = RollForward MarloweBlock ChainPoint ChainPoint

  -- | A change in which the chain is reverted to a previous point, discarding later blocks.
  | RollBackward ChainPoint ChainPoint

-- | A component that runs a chain sync client to traverse the blockchain and
-- extract blocks of Marlowe transactions. The sequence of changes to the chain
-- can be read by repeatedly running the resulting STM action.
chainSeekClient :: forall r. Component IO (ChainSeekClientDependencies r) (STM Bool, STM ChainEvent)
chainSeekClient = component \ChainSeekClientDependencies{..} -> do
  -- Initialize a TQueue for emitting ChainEvents.
  eventQueue <- newTQueue

  -- Initialize TVar for connectedness probe
  connectedVar <- newTVar False

  -- Return the component's thread action and the action to pull the next chain
  -- event.
  pure
    -- In this component's thread, run the chain sync client that will pull the
    -- transactions for discovering and following Marlowe contracts
    ( flip finally (atomically $ writeTVar connectedVar False) do
        runSomeConnector chainSyncConnector $ client
          (atomically . writeTQueue eventQueue)
          databaseQueries
          pollingInterval
          marloweScriptHashes
          payoutScriptHashes
          chainSyncQueryConnector
          eventBackend
          connectedVar
    , (readTVar connectedVar, readTQueue eventQueue)
    )
  where
  -- | A chain sync client that discovers and follows all Marlowe contracts
  client
    :: (ChainEvent -> IO ())
    -> DatabaseQueries IO
    -> NominalDiffTime
    -> NESet ScriptHash
    -> NESet ScriptHash
    -> SomeClientConnector (QueryClient ChainSyncQuery) IO
    -> EventBackend IO r ChainSeekClientSelector
    -> TVar Bool
    -> RuntimeChainSeekClient IO ()
  client emit DatabaseQueries{..} pollingInterval marloweScriptHashes payoutScriptHashes chainSyncQueryConnector eventBackend connectedVar =
    ChainSeekClient do
      let
        queryChainSync :: ChainSyncQuery Void err a -> IO a
        queryChainSync query = do
          result <- runSomeConnector chainSyncQueryConnector $ liftQuery query
          case result of
            Left _ -> fail "Failed to query chain sync"
            Right a -> pure a
      atomically $ writeTVar connectedVar True
      systemStart <- queryChainSync GetSystemStart
      securityParameter <- queryChainSync GetSecurityParameter
      -- Get the intersection points - the most recent block headers stored locally.
      intersectionPoints <- getIntersectionPoints
      let
        -- A client state for handling the intersect response.
        clientNextIntersect = ClientStNext
          -- Rejection of an intersection request implies no intersection was found.
          -- In this case, we have no choice but to start synchronization from Genesis.
          { recvMsgQueryRejected = \_ tip -> do
              -- Roll everything back to Genesis.
              emit $ RollBackward Genesis tip

              -- Start the main synchronization loop
              pure $ clientIdle securityParameter systemStart queryChainSync $ MarloweUTxO mempty mempty

          -- An intersection point was found, resume synchronization from
          -- that point.
          , recvMsgRollForward = \_ point tip -> do
              -- Always emit a rollback at the start.
              emit $ RollBackward point tip

              -- Load the MarloweUTxO
              utxo <- withEvent eventBackend LoadMarloweUTxO \ev -> case point of
                -- If the intersection point is at Genesis, return an empty MarloweUTxO.
                Genesis -> pure $ MarloweUTxO mempty mempty

                -- Otherwise load it from the database.
                At block -> do
                  utxo <- getMarloweUTxO block
                  addField ev utxo
                  pure utxo

              -- Start the main synchronization loop.
              pure $ clientIdle securityParameter systemStart queryChainSync utxo

          -- Since the client is at Genesis at the start of this request,
          -- it will never be rolled back. Handle the perfunctory case by
          -- looping.
          , recvMsgRollBackward = \_ _ -> pure clientIdleIntersect

          -- If the client is caught up to the tip, poll for the query results.
          , recvMsgWait = pollWithNext clientNextIntersect
          }

        -- A client state for sending the intersect request.
        clientIdleIntersect = SendMsgQueryNext (Intersect intersectionPoints) clientNextIntersect

      pure case intersectionPoints of
        -- Just start the loop right away with an empty UTxO.
        [] -> clientIdle securityParameter systemStart queryChainSync $ MarloweUTxO mempty mempty
        -- Request an intersection
        _ -> clientIdleIntersect
      where
      allScriptCredentials = NESet.map ScriptCredential $ NESet.union marloweScriptHashes payoutScriptHashes
      -- A helper function to poll pending query results after a set timeout and
      -- continue with the given ClientStNext.
      pollWithNext
        :: ClientStNext Move err res ChainPoint (WithGenesis BlockHeader) IO a
        -> IO (ClientStPoll Move err res ChainPoint (WithGenesis BlockHeader) IO a)
      pollWithNext next = do
        -- Wait for the polling interval to elapse (converted from seconds to
        -- milliseconds).
        threadDelay $ floor $ 1_000_000 * nominalDiffTimeToSeconds pollingInterval

        -- Poll for results and handle the response with the given ClientStNext.
        pure $ SendMsgPoll next

      -- The client's idle state handler for the main synchronization loop.
      -- Sends the next query to the chain sync server and handles the
      -- response.
      clientIdle
        :: Int
        -> SystemStart
        -> (forall err x. ChainSyncQuery Void err x -> IO x)
        -> MarloweUTxO
        -> ClientStIdle Move ChainPoint (WithGenesis BlockHeader) IO a
      clientIdle securityParameter systemStart queryChainSync = SendMsgQueryNext (FindTxsFor allScriptCredentials) . clientNext securityParameter systemStart queryChainSync

      -- Handles responses from the main synchronization loop query.
      clientNext
        :: Int
        -> SystemStart
        -> (forall err x. ChainSyncQuery Void err x -> IO x)
        -> MarloweUTxO
        -> ClientStNext Move Void (Set Transaction) ChainPoint (WithGenesis BlockHeader) IO a
      clientNext securityParameter systemStart queryChainSync utxo = ClientStNext
        -- Fail with an error if marlowe-chain-sync rejects the query. This is safe
        -- from bad user input, because our queries are derived from the ledger
        -- state, and so will only be rejected if the query derivation is
        -- incorrect, or marlowe-chain-sync is corrupt. Because both are unexpected
        -- errors, it is a non-recoverable error state.
        { recvMsgQueryRejected = absurd

        -- Handle the next block by extracting Marlowe transactions into a
        -- MarloweBlock and updating the MarloweUTxO.
        , recvMsgRollForward = \txs point tip -> do
            -- Get the current block (not expected ever to be Genesis).
            block <- case point of
              Genesis -> fail "Rolled forward to Genesis"
              At block -> pure block

            -- Get the era history
            eraHistory <- queryChainSync GetEraHistory

            -- Extract the Marlowe block and compute the next MarloweUTxO.
            nextUtxo <- case extractMarloweBlock systemStart eraHistory (NESet.toSet marloweScriptHashes) block txs utxo of
              -- If no MarloweBlock was extracted (not expected, but harmless), do nothing and return the current MarloweUTxO.
              -- This is not expected because the query would only be satisfied by a block that contains some usable Marlowe information.
              Nothing -> pure utxo

              Just (nextUtxo, marloweBlock) -> do
                -- Emit the marlowe block in a roll forward event to a downstream consumer.
                emit $ RollForward marloweBlock point tip

                -- Return the new MarloweUTxO
                pure nextUtxo

            -- Loop back into the main synchronization loop with an updated
            -- rollback state.
            pure $ clientIdle securityParameter systemStart queryChainSync nextUtxo

        , recvMsgRollBackward = \point tip -> do
            emit $ RollBackward point tip
            nextUtxo <- case point of
              Genesis -> pure $ MarloweUTxO mempty mempty
              At block -> getMarloweUTxO block
            pure $ clientIdle securityParameter systemStart queryChainSync nextUtxo

        , recvMsgWait = pollWithNext $ clientNext securityParameter systemStart queryChainSync utxo
        }
