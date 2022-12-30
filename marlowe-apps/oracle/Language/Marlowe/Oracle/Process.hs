

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Oracle.Process
  where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad (forever, unless, void)
import Control.Monad.Except (ExceptT(ExceptT), liftIO, runExceptT)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Language.Marlowe.Core.V1.Semantics.Types
  (ChoiceId(ChoiceId), Contract, Input(NormalInput), InputContent(IChoice), Party)
import Language.Marlowe.Oracle.Detect (containsOracleAction, contractReadyForOracle)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.App.Stream
  ( ContractStream(..)
  , contractFromStep
  , contractFromStream
  , streamAllContractIds
  , streamContractSteps
  , transactionIdFromStream
  )
import Language.Marlowe.Runtime.App.Transact (applyWithEvents)
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.ChainSync.Api (Address, TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Network.Oracle (Oracle, OracleEnv, readOracle, toOracleSymbol)
import Observe.Event (EventBackend, addField, hoistEvent, withEvent)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Syntax ((≔))
import Plutus.V2.Ledger.Api (toBuiltin)

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Data.ByteString.Char8 as BS8 (pack)
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
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> Int
  -> Party
  -> TChan ContractId
  -> IO (TChan (ContractStream 'V1))
runDetection eventBackend config pollingFrequency party inChannel =
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
            accept = maybe False (not . null . containsOracleAction party) . contractFromStep
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


runOracle
  :: EventBackend IO r DynamicEventSelector
  -> OracleEnv
  -> Config
  -> Int
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Party
  -> TChan (ContractStream 'V1)
  -> TChan ContractId
  -> IO ()
runOracle eventBackend oracleEnv config pollingFrequency address key party inChannel outChannel =
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
    -- Find the oracle symbols requested, the ones that can be serviced, and whether to ignore them, respectively.
    lookupSymbols :: LastSeen -> ([String], [Oracle], Bool)
    lookupSymbols LastSeen{..} =
      let
        rawSymbols = contractReadyForOracle party lastContract
        validSymbols = mapMaybe toOracleSymbol rawSymbols
        ignored = lastTxId `S.member` ignoredTxIds
      in
        (rawSymbols, validSymbols, ignored)
    -- Build and submit a transaction to report the oracle's value.
    report event contractId symbol =
      do
        value <- ExceptT $ readOracle eventBackend oracleEnv symbol
        void
          . applyWithEvents (hoistEvent liftIO event) config address key contractId
          . pure . NormalInput
          $ IChoice (ChoiceId (toBuiltin . BS8.pack $ show symbol) party) value
    -- Print the context of the transaction.
    printContext event LastSeen{lastTxId} rawSymbols validSymbols =
      do
        addField event $ ("previousTransactionId" :: Text) ≔ lastTxId
        addField event $ ("readyForOracle" :: Text) ≔ rawSymbols
        addField event $ ("availableForOracle" :: Text) ≔ fmap show validSymbols
    -- Print the result of the transaction.
    printResult event = addField event . (("result" :: Text) ≔)
    -- Process the stream of changes to contracts.
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
                                                    (rawSymbols, validSymbols, ignored) = lookupSymbols seen
                                                  unless ignored
                                                    $ do
                                                      printContext event seen rawSymbols validSymbols
                                                      result <- runExceptT $ mapM_ (report event csContractId) (take 1 validSymbols)
                                                      printResult event
                                                        $ case (result, null validSymbols) of
                                                            (Right ()    , True ) -> "Ignored."
                                                            (Right ()    , False) -> "Confirmed."
                                                            (Left message, _    ) -> "Failed: " <> message
                                                  revisit csContractId
                                                  pure $ ignore csContractId lastTxId lastSeen
                  ContractStreamFinish{..}   -> do
                                                  addField event $ ("action" :: Text) ≔ ("finish" :: String)
                                                  pure $ delete csContractId lastSeen
        go lastSeen'
  in
    go mempty
