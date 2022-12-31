

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Runtime.App.Channel
  ( LastSeen(..)
  , runContractAction
  , runDetection
  , runDiscovery
  ) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.App.Stream
  (ContractStream(..), contractFromStream, streamAllContractIds, streamContractSteps, transactionIdFromStream)
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)
import Observe.Event (Event, EventBackend, addField, withEvent)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector(..), DynamicField)
import Observe.Event.Syntax ((≔))

import qualified Data.Map.Strict as M (Map, adjust, delete, insert, lookup)
import qualified Data.Set as S (Set, insert, member)


runDiscovery
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> Int
  -> IO (TChan ContractId)
runDiscovery eventBackend config pollingFrequency =
  do
    channel <- newTChanIO
    void . forkIO
      . withEvent (hoistEventBackend liftIO eventBackend) (DynamicEventSelector "DiscoveryProcess")
      $ \event ->
        addField event
          . either (("failure" :: Text) ≔) (const $ ("success" :: Text) ≔ True)
          =<< runClientWithConfig config (streamAllContractIds eventBackend pollingFrequency channel)
    pure channel


runDetection
  :: (Either (CreateStep 'V1) (ContractStep 'V1) -> Bool)
  -> EventBackend IO r DynamicEventSelector
  -> Config
  -> Int
  -> TChan ContractId
  -> IO (TChan (ContractStream 'V1))
runDetection accept eventBackend config pollingFrequency inChannel =
  do
    outChannel <- newTChanIO
    void . forkIO
      . forever
      . runClientWithConfig config
      -- FIXME: If `MarloweSyncClient` were a `Monad`, then we could run
      --        multiple actions sequentially in a single connection.
      . withEvent (hoistEventBackend liftIO eventBackend) (DynamicEventSelector "DetectionProcess")
      $ \event ->
        do
          contractId <- liftIO . atomically $ readTChan inChannel
          addField event $ ("contractId" :: Text) ≔ contractId
          -- FIXME: If there were concurrency combinators for `MarloweSyncClient`, then we
          --        could follow multiple contracts in parallel using the same connection.
          let
            finishOnClose = True
            finishOnWait = True
          streamContractSteps eventBackend pollingFrequency finishOnClose finishOnWait accept contractId outChannel
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


runContractAction
  :: EventBackend IO r DynamicEventSelector
  -> (Event IO r DynamicEventSelector DynamicField -> LastSeen -> IO ())
  -> Int
  -> TChan (ContractStream 'V1)
  -> TChan ContractId
  -> IO ()
runContractAction eventBackend runInput pollingFrequency inChannel outChannel =
  let
    -- Nothing needs updating.
    rollback :: ContractStream 'V1 -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    rollback = const id
    -- Remove the contract from tracking.
    delete :: ContractId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    delete = M.delete
    -- Update the contract and its latest transaction.
    update :: ContractStream 'V1 -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    update cs lastSeen =
      let
        contractId = csContractId cs
      in
        flip (M.insert contractId) lastSeen
          $ case (contractId `M.lookup` lastSeen, contractFromStream cs, transactionIdFromStream cs) of
              (Nothing  , Just contract, Just txId) -> LastSeen contractId contract txId mempty
              (Just seen, Just contract, Just txId) -> seen {lastContract = contract, lastTxId = txId}
              _                                     -> error  -- FIXME: Guaranteed to occur.
                                                         $ "Invalid contract stream: seen = "
                                                         <> show (contractId `M.lookup` lastSeen)
                                                         <> ", contract = " <> show (contractFromStream cs)
                                                         <> " txId = " <> show (transactionIdFromStream cs)
                                                         <> "."
    -- Ignore the transaction in the future.
    ignore :: ContractId -> TxId -> M.Map ContractId LastSeen -> M.Map ContractId LastSeen
    ignore contractId txId lastSeen =
      M.adjust (\seen -> seen {ignoredTxIds = txId `S.insert` ignoredTxIds seen}) contractId lastSeen
    -- Revisit a contract later.
    revisit :: ContractId -> IO ()
    revisit contractId =
      -- FIXME: This is a workaround for contract discovery not tailing past the tip of the blockchain.
      void . forkIO
        $ threadDelay pollingFrequency
        >> atomically (writeTChan outChannel contractId)
    go :: M.Map ContractId LastSeen -> IO ()
    go lastSeen =
      do
        lastSeen' <-
          withEvent eventBackend (DynamicEventSelector "OracleProcess")
            $ \event ->
              do
                cs <- liftIO . atomically $ readTChan inChannel
                addField event $ ("contractId" :: Text) ≔ csContractId cs
                case cs of
                  ContractStreamStart{}      -> do
                                                  addField event $ ("action" :: Text) ≔ ("start" :: String)
                                                  pure $ update cs lastSeen
                  ContractStreamContinued{}  -> do
                                                  addField event $ ("action" :: Text) ≔ ("continued" :: String)
                                                  pure $ update cs lastSeen
                  ContractStreamRolledBack{} -> do
                                                  addField event $ ("action" :: Text) ≔ ("rollback" :: String)
                                                  pure $ rollback cs lastSeen
                  ContractStreamWait{..}     -> do
                                                  addField event $ ("action" :: Text) ≔ ("wait" :: String)
                                                  let
                                                    seen@LastSeen{lastTxId} =
                                                      case csContractId `M.lookup` lastSeen of
                                                        Just seen' -> seen'
                                                        _          -> error  -- FIXME: Guaranteed to occur.
                                                                        $ "Invalid contract stream: seen = "
                                                                        <> show (csContractId `M.lookup` lastSeen)
                                                                        <> "."
                                                  unless (lastTxId `S.member` ignoredTxIds seen)
                                                    $ runInput event seen
                                                  revisit csContractId
                                                  pure $ ignore csContractId lastTxId lastSeen
                  ContractStreamFinish{..}   -> do
                                                  addField event $ ("action" :: Text) ≔ ("finish" :: String)
                                                  pure $ delete csContractId lastSeen
        go lastSeen'
  in
    go mempty
