{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.ChainSeekClient where

import Cardano.Api (SystemStart)
import Colog (Message, WithLog)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, TVar, newTQueue, newTVar, readTQueue, readTVar, writeTQueue, writeTVar)
import Control.Monad.Event.Class (MonadInjectEvent, withEvent)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries (..))
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock (..), MarloweUTxO (..), extractMarloweBlock)
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Query.Client (QueryClient, request)
import Observe.Event (addField, reference)
import UnliftIO (MonadUnliftIO, atomically, finally)
import UnliftIO.Concurrent (threadDelay)

data ChainSeekClientSelector r f where
  EmitEvent :: ChainSeekClientSelector r (ChainEvent r)
  LoadMarloweUTxO :: ChainSeekClientSelector r MarloweUTxO

-- | Injectable dependencies for the chain sync client
data ChainSeekClientDependencies m = ChainSeekClientDependencies
  { databaseQueries :: DatabaseQueries m
  -- ^ Implementations of the database queries.
  , chainSyncConnector :: Connector RuntimeChainSeekClient m
  -- ^ A connector that connects a client of the chain sync protocol.
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  -- ^ A connector that connects a client of the chain sync query protocol.
  , pollingInterval :: NominalDiffTime
  -- ^ How frequently to poll the chain sync server when waiting.
  , marloweScriptHashes :: NESet ScriptHash
  -- ^ The set of known marlowe script hashes.
  , payoutScriptHashes :: NESet ScriptHash
  -- ^ The set of known payout script hashes.
  }

-- | A change to the chain with respect to Marlowe contracts
data ChainEvent r
  = -- | A change in which a new block of Marlowe transactions is added to the chain.
    RollForward MarloweBlock ChainPoint ChainPoint r
  | -- | A change in which the chain is reverted to a previous point, discarding later blocks.
    RollBackward ChainPoint ChainPoint r

-- | A component that runs a chain sync client to traverse the blockchain and
-- extract blocks of Marlowe transactions. The sequence of changes to the chain
-- can be read by repeatedly running the resulting STM action.
chainSeekClient
  :: forall r s env m
   . (MonadUnliftIO m, MonadInjectEvent r (ChainSeekClientSelector r) s m, MonadFail m, WithLog env Message m)
  => Component m (ChainSeekClientDependencies m) (STM Bool, STM (ChainEvent r))
chainSeekClient = component "indexer-chain-seek-client" \ChainSeekClientDependencies{..} -> do
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
        runConnector chainSyncConnector $
          client
            ( \mkEvent -> withEvent EmitEvent \ev -> do
                let event = mkEvent (reference ev)
                addField ev event
                atomically $ writeTQueue eventQueue event
            )
            databaseQueries
            pollingInterval
            marloweScriptHashes
            payoutScriptHashes
            chainSyncQueryConnector
            connectedVar
    , (readTVar connectedVar, readTQueue eventQueue)
    )
  where
    -- \| A chain sync client that discovers and follows all Marlowe contracts
    client
      :: ((r -> ChainEvent r) -> m ())
      -> DatabaseQueries m
      -> NominalDiffTime
      -> NESet ScriptHash
      -> NESet ScriptHash
      -> Connector (QueryClient ChainSyncQuery) m
      -> TVar Bool
      -> RuntimeChainSeekClient m ()
    client emit DatabaseQueries{..} pollingInterval marloweScriptHashes payoutScriptHashes chainSyncQueryConnector connectedVar =
      ChainSeekClient $ runConnector chainSyncQueryConnector do
        atomically $ writeTVar connectedVar True
        systemStart <- request GetSystemStart
        securityParameter <- request GetSecurityParameter
        -- Get the intersection points - the most recent block headers stored locally.
        intersectionPoints <- lift getIntersectionPoints
        let -- A client state for handling the intersect response.
            clientNextIntersect =
              ClientStNext
                { -- Rejection of an intersection request implies no intersection was found.
                  -- In this case, we have no choice but to start synchronization from Genesis.
                  recvMsgQueryRejected = \_ tip -> do
                    -- Roll everything back to Genesis.
                    emit $ RollBackward Genesis tip

                    -- Start the main synchronization loop
                    pure $ clientIdle securityParameter systemStart $ MarloweUTxO mempty mempty
                , -- An intersection point was found, resume synchronization from
                  -- that point.
                  recvMsgRollForward = \_ point tip -> do
                    -- Always emit a rollback at the start.
                    emit $ RollBackward point tip

                    -- Load the MarloweUTxO
                    utxo <- withEvent (LoadMarloweUTxO @r) \ev -> case point of
                      -- If the intersection point is at Genesis, return an empty MarloweUTxO.
                      Genesis -> pure $ MarloweUTxO mempty mempty
                      -- Otherwise load it from the database.
                      At block -> do
                        utxo <- getMarloweUTxO block
                        addField ev utxo
                        pure utxo

                    -- Start the main synchronization loop.
                    pure $ clientIdle securityParameter systemStart utxo
                , -- Since the client is at Genesis at the start of this request,
                  -- it will never be rolled back. Handle the perfunctory case by
                  -- looping.
                  recvMsgRollBackward = \_ _ -> pure clientIdleIntersect
                , -- If the client is caught up to the tip, poll for the query results.
                  recvMsgWait = pollWithNext clientNextIntersect
                }

            -- A client state for sending the intersect request.
            clientIdleIntersect = SendMsgQueryNext (Intersect intersectionPoints) clientNextIntersect

        pure case intersectionPoints of
          -- Just start the loop right away with an empty UTxO.
          [] -> clientIdle securityParameter systemStart $ MarloweUTxO mempty mempty
          -- Request an intersection
          _ -> clientIdleIntersect
      where
        allScriptCredentials = NESet.map ScriptCredential $ NESet.union marloweScriptHashes payoutScriptHashes
        -- A helper function to poll pending query results after a set timeout and
        -- continue with the given ClientStNext.
        pollWithNext
          :: ClientStNext Move err res ChainPoint (WithGenesis BlockHeader) m a
          -> m (ClientStPoll Move err res ChainPoint (WithGenesis BlockHeader) m a)
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
          -> MarloweUTxO
          -> ClientStIdle Move ChainPoint (WithGenesis BlockHeader) m a
        clientIdle securityParameter systemStart = SendMsgQueryNext (FindTxsFor allScriptCredentials) . clientNext securityParameter systemStart

        -- Handles responses from the main synchronization loop query.
        clientNext
          :: Int
          -> SystemStart
          -> MarloweUTxO
          -> ClientStNext Move Void (Set Transaction) ChainPoint (WithGenesis BlockHeader) m a
        clientNext securityParameter systemStart utxo =
          ClientStNext
            { -- Fail with an error if marlowe-chain-sync rejects the query. This is safe
              -- from bad user input, because our queries are derived from the ledger
              -- state, and so will only be rejected if the query derivation is
              -- incorrect, or marlowe-chain-sync is corrupt. Because both are unexpected
              -- errors, it is a non-recoverable error state.
              recvMsgQueryRejected = absurd
            , -- Handle the next block by extracting Marlowe transactions into a
              -- MarloweBlock and updating the MarloweUTxO.
              recvMsgRollForward = \txs point tip -> do
                -- Get the current block (not expected ever to be Genesis).
                block <- case point of
                  Genesis -> fail "Rolled forward to Genesis"
                  At block -> pure block

                -- Get the era history
                eraHistory <- runConnector chainSyncQueryConnector $ request GetEraHistory

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
                pure $ clientIdle securityParameter systemStart nextUtxo
            , recvMsgRollBackward = \point tip -> do
                emit $ RollBackward point tip
                nextUtxo <- case point of
                  Genesis -> pure $ MarloweUTxO mempty mempty
                  At block -> getMarloweUTxO block
                pure $ clientIdle securityParameter systemStart nextUtxo
            , recvMsgWait = pollWithNext $ clientNext securityParameter systemStart utxo
            }
