{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Marlowe.Runtime.App.Channel (
  LastSeen (..),
  RequeueFrequency (..),
  mkDetection,
  mkDiscovery,
  runContractAction,
  runDetection,
  runDiscovery,
  runDiscovery',
  watchContracts,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Monad (forever, join, unless, void)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Time.Units (Second, toMicroseconds)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.App.Stream (
  ContractStream (..),
  EOF (EOF),
  TChanEOF,
  contractFromStream,
  streamAllContractIds,
  streamContractSteps,
  transactionIdFromStream,
 )
import Language.Marlowe.Runtime.App.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag (V1))
import Language.Marlowe.Runtime.History.Api (ContractStep (ApplyTransaction, RedeemPayout), CreateStep)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector (..), DynamicField)
import Observe.Event.Explicit (Event, EventBackend, addField, withEvent)
import Observe.Event.Syntax ((≔))

import qualified Data.Map.Strict as M (Map, adjust, delete, insert, lookup, singleton, unionWith)
import qualified Data.Set as S (Set, insert, member, singleton, union)

-- `mk*` functions are useful if you want to manage the threads yourself.
mkDiscovery
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> FinishOnWait
  -> IO (TChanEOF (Either ChainPoint (BlockHeader, ContractId)), IO ())
mkDiscovery eventBackend config pollingFrequency endOnWait =
  do
    channel <- newTChanIO
    let discovery =
          withEvent (hoistEventBackend liftIO eventBackend) (DynamicEventSelector "DiscoveryProcess") $
            \event ->
              addField event
                . maybe (("success" :: Text) ≔ True) ((("failure" :: Text) ≔) . show)
                =<< runClientWithConfig config (streamAllContractIds eventBackend pollingFrequency endOnWait channel)
    pure (channel, discovery)

runDiscovery
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> FinishOnWait
  -> IO (TChanEOF (Either ChainPoint (BlockHeader, ContractId)))
runDiscovery eventBackend config pollingFrequency endOnWait = do
  (channel, discovery) <- mkDiscovery eventBackend config pollingFrequency endOnWait
  void . forkIO $ discovery
  pure channel

-- | A simplified version of `runDiscovery` that only notifies about new contracts and ignores rollback events.
runDiscovery'
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> FinishOnWait
  -> IO (TChanEOF ContractId)
runDiscovery' eventBackend config pollingFrequency endOnWait = do
  contractIdChannel <- newTChanIO
  runDiscovery eventBackend config pollingFrequency endOnWait >>= \discoveryChannel ->
    void . forkIO . forever $ atomically do
      readTChan discoveryChannel >>= \case
        Left EOF -> writeTChan contractIdChannel (Left EOF)
        Right (Right (_, contractId)) -> writeTChan contractIdChannel (Right contractId)
        Right (Left _) -> pure ()
  pure contractIdChannel

mkDetection
  :: (Either (CreateStep 'V1) (ContractStep 'V1) -> Bool)
  -> EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> FinishOnClose
  -> FinishOnWait
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1), IO ())
mkDetection accept eventBackend config pollingFrequency finishOnClose finishOnWait inChannel =
  do
    outChannel <- newTChanIO
    let threadAction = join
          . withEvent (hoistEventBackend liftIO eventBackend) (DynamicEventSelector "DetectionProcess")
          $ \event ->
            liftIO (atomically $ readTChan inChannel) >>= \case
              Left _ -> do
                liftIO . atomically $ writeTChan outChannel $ Left EOF
                pure $ pure ()
              Right contractId -> do
                addField event $ ("contractId" :: Text) ≔ contractId
                -- FIXME: If there were concurrency combinators for `MarloweSyncClient`, then we
                --        could follow multiple contracts in parallel using the same connection.
                streamContractSteps eventBackend pollingFrequency finishOnClose finishOnWait accept contractId outChannel
                pure threadAction
        detection = runClientWithConfig config threadAction
    pure (outChannel, detection)

runDetection
  :: (Either (CreateStep 'V1) (ContractStep 'V1) -> Bool)
  -> EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> FinishOnClose
  -> FinishOnWait
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection accept eventBackend config pollingFrequency finishOnClose finishOnWait inChannel = do
  (outChannel, detection) <- mkDetection accept eventBackend config pollingFrequency finishOnClose finishOnWait inChannel
  void . forkIO $ detection
  pure outChannel

data LastSeen = LastSeen
  { thisContractId :: ContractId
  -- ^ The ID of the contract.
  , theseSteps :: [ContractStep 'V1]
  -- ^ The contract steps that were applied since the previous report.
  , lastContract :: Contract
  -- ^ The most recent contract body for this ID.
  , lastTxId :: TxId
  -- ^ The most recent transaction ID for this contract.
  , ignoredTxIds :: S.Set TxId
  -- ^ The set of transactions already reported for this contract.
  }
  deriving (Show)

newtype RequeueFrequency = RequeueFrequency Second

-- | Run a function for each open transaction of each contract, repeating periodically. Note that this does not visit every transaction in the contract: instead it only visits the transactions at the tip of the contract.
runContractAction
  :: forall r
   . Text
  -> EventBackend IO r DynamicEventSelector
  -> (Event IO r DynamicField -> LastSeen -> IO ())
  -> RequeueFrequency
  -> FinishOnWait
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runContractAction selectorName eventBackend runInput (RequeueFrequency requeueFrequency) endOnWait inChannel outChannel =
  let -- Nothing needs updating.
      rollback :: ContractStream 'V1 -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
      rollback = const id
      -- Remove the contract from tracking.
      delete :: ContractId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
      delete = M.delete
      -- Update the contract and its latest transaction.
      update :: Event IO r DynamicField -> ContractStream 'V1 -> M.Map ContractId LastSeen -> IO (M.Map ContractId LastSeen)
      update event cs lastSeen =
        let contractId = csContractId cs
         in case (contractId `M.lookup` lastSeen, contractFromStream cs, transactionIdFromStream cs, cs) of
              -- The contract is created, so record its ID, body, and most recent transaction in the map of most-recent information for contracts still open.
              (Nothing, Just contract, Just txId, ContractStreamStart{}) -> pure $ M.insert contractId (LastSeen contractId mempty contract txId mempty) lastSeen
              -- Input was applied to the contract, which is still open, so update its body and most recent transaction in the map of most-recent information for contracts still open.
              (Just seen, Just contract, Just txId, ContractStreamContinued{csContractStep = ApplyTransaction{}}) -> pure $ M.insert contractId (seen{lastContract = contract, lastTxId = txId}) lastSeen
              -- Input was applied to the contract, but it is now closed, so delete it from the map of most-recent information for contracts still open.
              (Just _, Nothing, Just _, ContractStreamContinued{csContractStep = ApplyTransaction{}}) -> pure $ M.delete contractId lastSeen
              -- A payout was redeemed from the contract, so there is no need to update the map of most-recent information for contracts still open.
              (Just _, _, Just _, ContractStreamContinued{csContractStep = RedeemPayout{}}) -> pure lastSeen
              -- FIXME: This should be impossible because a contract must either be created, continuing, closing or redeeming, but diagnose and remedy if this ever occurs.
              (seen, _, _, _) -> do
                addField event $
                  ("invalidContractStream" :: Text)
                    ≔ object
                      [ "lastContract" .= fmap lastContract seen
                      , "lastTxId" .= fmap lastTxId seen
                      , "contractStream" .= cs
                      ]
                pure lastSeen
      -- Ignore the transaction in the future.
      ignore :: TxId -> ContractId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
      ignore txId = M.adjust (\seen -> seen{ignoredTxIds = txId `S.insert` ignoredTxIds seen})
      -- Revisit a contract later.
      revisit :: ContractId -> IO ()
      revisit contractId
        | unFinishOnWait endOnWait = pure ()
        -- FIXME: This is a workaround for contract discovery not tailing past the tip of the blockchain.
        | otherwise =
            void . forkIO $
              threadDelay (fromIntegral . toMicroseconds $ requeueFrequency)
                >> atomically (writeTChan outChannel $ Right contractId)
      go :: M.Map ContractId LastSeen -> IO ()
      go lastSeen =
        do
          -- The `lastSeen` map tracks the most-recent information about contracts that are still open.
          lastSeen' <-
            withEvent eventBackend (DynamicEventSelector selectorName) $
              \event -> runExceptT $
                do
                  cs <- ExceptT . atomically $ readTChan inChannel
                  liftIO . addField event $ ("contractId" :: Text) ≔ csContractId cs
                  liftIO $ case cs of
                    -- Add the contract to `lastSeen` when it is created.
                    ContractStreamStart{} -> do
                      addField event $ ("action" :: Text) ≔ ("start" :: String)
                      update event cs lastSeen
                    -- Update the contract information in `lastSeen` when it is continued by applying input or withdrawing a payout.
                    ContractStreamContinued{} -> do
                      addField event $ ("action" :: Text) ≔ ("continued" :: String)
                      update event cs lastSeen
                    -- Process a rollback, though nothing is required because the rolled-back aspects of the contract will naturally be replayed.
                    ContractStreamRolledBack{} -> do
                      addField event $ ("action" :: Text) ≔ ("rollback" :: String)
                      pure $ rollback cs lastSeen
                    -- The end of the stream for a contract has been reached, but there may be further progression of the contract when it is revisited in future followings.
                    ContractStreamWait{..} -> do
                      addField event $ ("action" :: Text) ≔ ("wait" :: String)
                      case csContractId `M.lookup` lastSeen of
                        -- We can only reach the tip of the contract if the contract was previously seen.
                        Just seen@LastSeen{lastTxId} ->
                          do
                            -- Supply the contract information to the user-defined processing function if we haven't done so already at this tip (transaction).
                            unless (lastTxId `S.member` ignoredTxIds seen) $
                              runInput event seen
                            -- Re-queue the contract ID so it is followed later, since there may then be new transactions beyond the present tip.
                            revisit csContractId
                            -- Remember to not call the user-defined processing function again a this tip (transaction).
                            pure $ ignore lastTxId csContractId lastSeen
                        -- FIXME: Diagnose and remedy situations if this ever occurs.
                        _ ->
                          do
                            addField event $
                              ("invalidContractStream" :: Text)
                                ≔ object ["contractStream" .= cs]
                            pure lastSeen
                    -- The stream of contract information is complete because the contract closed, so we don't need to track it anymore.
                    ContractStreamFinish{..} -> do
                      addField event $ ("action" :: Text) ≔ ("finish" :: String)
                      pure $ delete csContractId lastSeen
          either (const $ pure ()) go lastSeen'
   in go mempty

-- | Call a function exactly once on each transaction of every contract.
watchContracts
  :: forall r
   . Text
  -> EventBackend IO r DynamicEventSelector
  -> (Event IO r DynamicField -> ContractStream 'V1 -> IO ())
  -> RequeueFrequency
  -> FinishOnWait
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
watchContracts selectorName eventBackend runInput (RequeueFrequency requeueFrequency) endOnWait inChannel outChannel =
  let go :: M.Map ContractId (S.Set BlockHeader) -> IO ()
      go seen =
        do
          seen' <-
            withEvent eventBackend (DynamicEventSelector selectorName) $
              \event -> runExceptT $
                do
                  cs <- ExceptT . atomically $ readTChan inChannel
                  liftIO . addField event $ ("contractId" :: Text) ≔ csContractId cs
                  liftIO $ case cs of
                    ContractStreamStart{..} -> do
                      addField event $ ("action" :: Text) ≔ ("start" :: String)
                      unless ((S.member csBlockHeader <$> M.lookup csContractId seen) == Just True) $
                        runInput event cs
                      pure . M.unionWith S.union seen . M.singleton csContractId $ S.singleton csBlockHeader
                    ContractStreamContinued{..} -> do
                      addField event $ ("action" :: Text) ≔ ("continued" :: String)
                      unless ((S.member csBlockHeader <$> M.lookup csContractId seen) == Just True) $
                        runInput event cs
                      pure . M.unionWith S.union seen . M.singleton csContractId $ S.singleton csBlockHeader
                    ContractStreamRolledBack{} -> do
                      addField event $ ("action" :: Text) ≔ ("rollback" :: String)
                      runInput event cs
                      pure seen
                    ContractStreamWait{..} -> do
                      addField event $ ("action" :: Text) ≔ ("wait" :: String)
                      runInput event cs
                      unless (unFinishOnWait endOnWait)
                        . void
                        . forkIO
                        $ threadDelay (fromIntegral . toMicroseconds $ requeueFrequency)
                          >> atomically (writeTChan outChannel $ Right csContractId)
                      pure seen
                    ContractStreamFinish{..} -> do
                      addField event $ ("action" :: Text) ≔ ("finish" :: String)
                      runInput event cs
                      pure $ M.delete csContractId seen
          either (const $ pure ()) go seen'
   in go mempty
