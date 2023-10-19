{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.ChainSeekClient where

import Cardano.Api (SystemStart)
import Colog (Message, WithLog, logInfo)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, TVar, newTQueue, newTVar, readTQueue, readTVar, writeTQueue, writeTVar)
import Control.Monad.Event.Class (MonadInjectEvent, withEvent)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries (..))
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock (..), MarloweUTxO (..), extractMarloweBlock)
import Network.Protocol.ChainSeek.Types (ChainSeek (..), Message (..), SingTag)
import Network.Protocol.Connection (Connector, runConnector)
import qualified Network.Protocol.Peer.Monad as PeerT
import Network.Protocol.Query.Client (QueryClient, request)
import Observe.Event (addField, reference)
import Text.Printf (printf)
import UnliftIO (MonadUnliftIO, atomically, finally)
import UnliftIO.Concurrent (threadDelay)

data ChainSeekClientSelector r f where
  EmitEvent :: ChainSeekClientSelector r (ChainEvent r)
  LoadMarloweUTxO :: ChainSeekClientSelector r MarloweUTxO

-- | Injectable dependencies for the chain sync client
data ChainSeekClientDependencies m = ChainSeekClientDependencies
  { databaseQueries :: DatabaseQueries m
  -- ^ Implementations of the database queries.
  , chainSyncConnector :: Connector (RuntimeChainSeekClientT 'StIdle 'StDone) m
  -- ^ A connector that connects a client of the chain sync protocol.
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  -- ^ A connector that connects a client of the chain sync query protocol.
  , pollingInterval :: NominalDiffTime
  -- ^ How frequently to poll the chain sync server when waiting.
  , marloweScriptHashes :: NESet ScriptHash
  -- ^ The set of known marlowe script hashes.
  , payoutScriptHashes :: NESet ScriptHash
  -- ^ The set of known payout script hashes.
  , indexParties :: m ()
  }

-- | A change to the chain with respect to Marlowe contracts
data ChainEvent r
  = -- | A change in which a new block of Marlowe transactions is added to the chain.
    RollForwardEvent MarloweBlock ChainPoint ChainPoint r
  | -- | A change in which the chain is reverted to a previous point, discarding later blocks.
    RollBackwardEvent ChainPoint ChainPoint r

-- | A component that runs a chain sync client to traverse the blockchain and
-- extract blocks of Marlowe transactions. The sequence of changes to the chain
-- can be read by repeatedly running the resulting STM action.
chainSeekClient
  :: forall r s env m
   . (MonadUnliftIO m, MonadInjectEvent r (ChainSeekClientSelector r) s m, MonadFail m, WithLog env Colog.Message m)
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
                case event of
                  RollBackwardEvent local remote _ -> do
                    logInfo case remote of
                      Genesis -> "Chain-sync rolled back to genesis"
                      At (BlockHeader _ hash no) ->
                        fromString $ printf "Chain sync switched to new tip %s (#%d)" (show hash) (unBlockNo no)
                    let remoteNo = case remote of
                          Genesis -> 0
                          At (BlockHeader _ _ no) -> unBlockNo no
                    logInfo case local of
                      Genesis -> "Rolled back to genesis"
                      At (BlockHeader _ hash (BlockNo no)) ->
                        fromString $
                          printf
                            "Rolled back to block %s (#%d of %d) (%d%%)"
                            (show hash)
                            no
                            remoteNo
                            ((no * 100) `div` remoteNo)
                  RollForwardEvent _ local remote _ -> do
                    let remoteNo = case remote of
                          Genesis -> 0
                          At (BlockHeader _ _ no) -> unBlockNo no
                    logInfo case local of
                      Genesis -> "Rolled forward to genesis"
                      At (BlockHeader _ hash (BlockNo no)) ->
                        fromString $
                          printf
                            "Rolled forward to block %s (#%d of %d) (%d%%)"
                            (show hash)
                            no
                            remoteNo
                            ((no * 100) `div` remoteNo)
            )
            databaseQueries
            pollingInterval
            marloweScriptHashes
            payoutScriptHashes
            chainSyncQueryConnector
            connectedVar
            indexParties
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
      -> m ()
      -> RuntimeChainSeekClientT 'StIdle 'StDone m ()
    client emit DatabaseQueries{..} pollingInterval marloweScriptHashes payoutScriptHashes chainSyncQueryConnector connectedVar indexParties =
      PeerT.do
        ResGetSystemStart systemStart <- lift @(RuntimeChainSeekClientT 'StIdle 'StIdle) $ runConnector chainSyncQueryConnector do
          lift indexParties
          atomically $ writeTVar connectedVar True
          request ReqGetSystemStart
        -- Get the intersection points - the most recent block headers stored locally.
        intersectionPoints <- lift @(RuntimeChainSeekClientT 'StIdle 'StIdle) getIntersectionPoints
        lift @(RuntimeChainSeekClientT 'StIdle 'StIdle) case intersectionPoints of
          ((BlockHeader _ hash no) : _) ->
            logInfo $ fromString $ printf "Tip of local chain: %s (#%d)" (show hash) (unBlockNo no)
          _ -> logInfo "Local chain empty"
        let -- A client state for handling the intersect response.
            clientNextIntersect :: RuntimeChainSeekClientT ('StNext 'Intersect) 'StIdle m MarloweUTxO
            clientNextIntersect = PeerT.await \case
              -- Rejection of an intersection request implies no intersection was found.
              -- In this case, we have no choice but to start synchronization from Genesis.
              RejectQuery _ tip -> do
                lift $ logInfo "No intersection with chain-sync"
                -- Roll everything back to Genesis.
                lift $ emit $ RollBackwardEvent Genesis tip

                -- Start the main synchronization loop
                pure $ MarloweUTxO mempty mempty
              -- An intersection point was found, resume synchronization from
              -- that point.
              RollForward _ point tip -> do
                lift $ logInfo "Intersected with chain-sync"
                -- Always emit a rollback at the start.
                lift $ emit $ RollBackwardEvent point tip

                -- Load the MarloweUTxO
                PeerT.withEvent (LoadMarloweUTxO @r) \ev -> case point of
                  -- If the intersection point is at Genesis, return an empty MarloweUTxO.
                  Genesis -> pure $ MarloweUTxO mempty mempty
                  -- Otherwise load it from the database.
                  At block -> lift do
                    utxo <- getMarloweUTxO block
                    addField ev utxo
                    pure utxo
              -- Since the client is at Genesis at the start of this request,
              -- it will never be rolled back. Handle the perfunctory case by
              -- looping.
              RollBackward _ _ -> clientIdleIntersect
              -- If the client is caught up to the tip, poll for the query results.
              Wait -> pollWithNext clientNextIntersect

            -- A client state for sending the intersect request.
            clientIdleIntersect = PeerT.do
              PeerT.yield $ QueryNext $ MoveIntersect intersectionPoints
              clientNextIntersect

        utxo <- case intersectionPoints of
          -- Just start the loop right away with an empty UTxO.
          [] -> pure $ MarloweUTxO mempty mempty
          -- Request an intersection
          _ -> clientIdleIntersect
        clientIdle systemStart utxo
      where
        allScriptCredentials = NESet.map ScriptCredential $ NESet.union marloweScriptHashes payoutScriptHashes
        -- A helper function to poll pending query results after a set timeout and
        -- continue with the given ClientStNext.
        pollWithNext
          :: (SingTag t)
          => RuntimeChainSeekClientT ('StNext t) st m a
          -> RuntimeChainSeekClientT ('StPoll t) st m a
        pollWithNext next = PeerT.do
          -- Wait for the polling interval to elapse (converted from seconds to
          -- milliseconds).
          lift $ threadDelay $ floor $ 1_000_000 * nominalDiffTimeToSeconds pollingInterval

          -- Poll for results and handle the response with the given ClientStNext.
          PeerT.yield Poll
          next

        -- The client's idle state handler for the main synchronization loop.
        -- Scans the chain for marlowe transactions.
        clientIdle
          :: SystemStart
          -> MarloweUTxO
          -> RuntimeChainSeekClientT 'StIdle 'StDone m a
        clientIdle systemStart utxo = PeerT.do
          let move = MoveFindTxsFor allScriptCredentials
          PeerT.yield $ Scan move
          clientScan move systemStart utxo

        -- The client's scan loop.
        clientScan
          :: Move 'FindTxsFor
          -> SystemStart
          -> MarloweUTxO
          -> RuntimeChainSeekClientT ('StScan 'FindTxsFor) 'StDone m a
        clientScan move systemStart utxo = PeerT.do
          PeerT.yield Collect
          clientCollect move systemStart utxo

        -- Handles responses from the main synchronization loop query.
        clientCollect
          :: Move 'FindTxsFor
          -> SystemStart
          -> MarloweUTxO
          -> RuntimeChainSeekClientT ('StCollect 'FindTxsFor) 'StDone m a
        clientCollect move systemStart utxo = PeerT.await \case
          CollectFailed err _ -> case err of {}
          -- Handle the next block by extracting Marlowe transactions into a
          -- MarloweBlock and updating the MarloweUTxO.
          Collected blocks tip -> PeerT.do
            nextUtxo <- lift $ execStateT (handleRollForward systemStart tip blocks) utxo

            -- Loop back into the main synchronization loop with an updated
            -- rollback state.
            clientScan move systemStart nextUtxo
          CollectRollBackward point tip -> handleRollBackward systemStart point tip
          CollectWait _ -> pollWithNext $ clientNext move systemStart utxo

        handleRollForward :: SystemStart -> ChainPoint -> [(ChainPoint, SeekResult 'FindTxsFor)] -> StateT MarloweUTxO m ()
        handleRollForward systemStart tip blocks = do
          -- Get the era history
          ResGetEraHistory eraHistory <- lift $ runConnector chainSyncQueryConnector $ request ReqGetEraHistory

          for_ blocks \(point, ResFindTxsFor txs) -> do
            -- Get the current block (not expected ever to be Genesis).
            block <- case point of
              Genesis -> fail "Rolled forward to Genesis"
              At block -> pure block

            utxo' <- get

            -- Extract the Marlowe block and compute the next MarloweUTxO.
            for_ (extractMarloweBlock systemStart eraHistory (NESet.toSet marloweScriptHashes) block txs utxo') \(nextUtxo, marloweBlock) -> do
              -- Emit the marlowe block in a roll forward event to a downstream consumer.
              lift $ emit $ RollForwardEvent marloweBlock point tip

              -- Update the MarloweUTxO
              put nextUtxo

        handleRollBackward :: SystemStart -> ChainPoint -> ChainPoint -> RuntimeChainSeekClientT 'StIdle 'StDone m a
        handleRollBackward systemStart point tip = PeerT.do
          lift @(RuntimeChainSeekClientT 'StIdle 'StIdle) $ emit $ RollBackwardEvent point tip
          nextUtxo <- lift case point of
            Genesis -> pure $ MarloweUTxO mempty mempty
            At block -> getMarloweUTxO block
          clientIdle systemStart nextUtxo

        -- Handles responses from the main synchronization loop query.
        clientNext
          :: Move 'FindTxsFor
          -> SystemStart
          -> MarloweUTxO
          -> RuntimeChainSeekClientT ('StNext 'FindTxsFor) 'StDone m a
        clientNext move systemStart utxo = PeerT.await \case
          RejectQuery err _ -> case err of {}
          -- Handle the next block by extracting Marlowe transactions into a
          -- MarloweBlock and updating the MarloweUTxO.
          RollForward res pos tip -> PeerT.do
            nextUtxo <- lift $ execStateT (handleRollForward systemStart tip [(pos, res)]) utxo

            -- Loop back into the main synchronization loop with an updated
            -- rollback state.
            clientIdle systemStart nextUtxo
          RollBackward point tip -> handleRollBackward systemStart point tip
          Wait -> pollWithNext $ clientNext move systemStart utxo
