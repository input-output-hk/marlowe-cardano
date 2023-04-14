{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.Runtime.App.Channel
  ( LastSeen(..)
  , RequeueFrequency(..)
  , mkDetection
  , mkDiscovery
  , runContractAction
  , runDetection
  , runDiscovery
  , runDiscovery'
  ) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Monad (forever, join, unless, void)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Map.Strict as M (Map, adjust, delete, insert, lookup)
import qualified Data.Set as S (Set, insert, member)
import Data.Text (Text)
import Data.Time.Units (Second)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.App.Stream
  ( ContractStream(..)
  , EOF(EOF)
  , TChanEOF
  , contractFromStream
  , streamAllContractIds
  , streamContractSteps
  , transactionIdFromStream
  )
import Language.Marlowe.Runtime.App.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector(..), DynamicField)
import Observe.Event.Explicit (Event, EventBackend, addField, withEvent)
import Observe.Event.Syntax ((≔))


-- `mk*` functions are useful if you want to manage the threads yourself.
mkDiscovery
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> Bool
  -> IO (TChanEOF (Either ChainPoint (BlockHeader, ContractId)), IO ())
mkDiscovery eventBackend config pollingFrequency endOnWait =
  do
    channel <- newTChanIO
    let
      discovery =
        withEvent (hoistEventBackend liftIO eventBackend) (DynamicEventSelector "DiscoveryProcess")
        $ \event ->
          addField event
            . maybe (("success" :: Text) ≔ True) ((("failure" :: Text) ≔) . show)
            =<< runClientWithConfig config (streamAllContractIds eventBackend pollingFrequency endOnWait channel)
    pure (channel, discovery)


runDiscovery
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> Bool
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
  -> Bool
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
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1), IO ())
mkDetection accept eventBackend config pollingFrequency inChannel =
  do
    outChannel <- newTChanIO
    let
      threadAction = join
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
          let
            finishOnClose = True
            finishOnWait = True
          streamContractSteps eventBackend pollingFrequency finishOnClose finishOnWait accept contractId outChannel
          pure threadAction
      detection = runClientWithConfig config threadAction
    pure (outChannel, detection)

runDetection
  :: (Either (CreateStep 'V1) (ContractStep 'V1) -> Bool)
  -> EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection accept eventBackend config pollingFrequency inChannel = do
  (outChannel, detection) <- mkDetection accept eventBackend config pollingFrequency inChannel
  void . forkIO $ detection
  pure outChannel

data LastSeen =
  LastSeen
  {
    thisContractId :: ContractId
  , lastContract :: Contract
  , lastTxId :: TxId
  , ignoredTxIds :: S.Set TxId
  }
    deriving (Show)


newtype RequeueFrequency = RequeueFrequency Second


runContractAction
  :: forall r
  .  Text
  -> EventBackend IO r DynamicEventSelector
  -> (Event IO r DynamicField -> LastSeen -> IO ())
  -> RequeueFrequency
  -> Bool
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runContractAction selectorName eventBackend runInput (RequeueFrequency requeueFrequency) endOnWait inChannel outChannel =
  let
    -- Nothing needs updating.
    rollback :: ContractStream 'V1 -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    rollback = const id
    -- Remove the contract from tracking.
    delete :: ContractId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    delete = M.delete
    -- Update the contract and its latest transaction.
    update :: Event IO r DynamicField -> ContractStream 'V1 -> M.Map ContractId LastSeen -> IO (M.Map ContractId LastSeen)
    update event cs lastSeen =
      let
        contractId = csContractId cs
      in
        case (contractId `M.lookup` lastSeen, contractFromStream cs, transactionIdFromStream cs) of
          (Nothing  , Just contract, Just txId) -> pure $ M.insert contractId (LastSeen contractId contract txId mempty) lastSeen
          (Just seen, Just contract, Just txId) -> pure $ M.insert contractId (seen {lastContract = contract, lastTxId = txId}) lastSeen
          (Just _   , Nothing      , Just _   ) -> pure $ M.delete contractId lastSeen
          (seen     , _            , _        ) -> do  -- FIXME: Diagnose and remedy situations if this ever occurs.
                                                     addField event
                                                       $ ("invalidContractStream" :: Text) ≔
                                                         object
                                                             [
                                                               "lastContract" .= fmap lastContract seen
                                                             , "lastTxId" .= fmap lastTxId seen
                                                             , "contractStream" .= cs
                                                             ]
                                                     pure lastSeen
    -- Ignore the transaction in the future.
    ignore :: ContractId -> TxId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    ignore contractId txId lastSeen =
      M.adjust (\seen -> seen {ignoredTxIds = txId `S.insert` ignoredTxIds seen}) contractId lastSeen
    -- Revisit a contract later.
    revisit :: ContractId -> IO ()
    revisit contractId
      | endOnWait = pure ()
      -- FIXME: This is a workaround for contract discovery not tailing past the tip of the blockchain.
      | otherwise = void . forkIO
        $ threadDelay (fromIntegral requeueFrequency)
        >> atomically (writeTChan outChannel $ Right contractId)
    go :: M.Map ContractId LastSeen -> IO ()
    go lastSeen =
      do
        lastSeen' <-
          withEvent eventBackend (DynamicEventSelector selectorName)
            $ \event -> runExceptT $
              do
                cs <- ExceptT . atomically $ readTChan inChannel
                liftIO . addField event $ ("contractId" :: Text) ≔ csContractId cs
                liftIO $ case cs of
                  ContractStreamStart{}      -> do
                                                  addField event $ ("action" :: Text) ≔ ("start" :: String)
                                                  update event cs lastSeen
                  ContractStreamContinued{}  -> do
                                                  addField event $ ("action" :: Text) ≔ ("continued" :: String)
                                                  update event cs lastSeen
                  ContractStreamRolledBack{} -> do
                                                  addField event $ ("action" :: Text) ≔ ("rollback" :: String)
                                                  pure $ rollback cs lastSeen
                  ContractStreamWait{..}     -> do
                                                  addField event $ ("action" :: Text) ≔ ("wait" :: String)
                                                  case csContractId `M.lookup` lastSeen of
                                                    Just seen@LastSeen{lastTxId} ->
                                                      do
                                                        unless (lastTxId `S.member` ignoredTxIds seen)
                                                          $ runInput event seen
                                                        revisit csContractId
                                                        pure $ ignore csContractId lastTxId lastSeen
                                                    _ ->
                                                      do  -- FIXME: Diagnose and remedy situations if this ever occurs.
                                                        addField event
                                                          $ ("invalidContractStream" :: Text) ≔
                                                            object ["contractStream" .= cs]
                                                        pure lastSeen
                  ContractStreamFinish{..}   -> do
                                                  addField event $ ("action" :: Text) ≔ ("finish" :: String)
                                                  pure $ delete csContractId lastSeen
        either (const $ pure ()) go lastSeen'
  in
    go mempty
