

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Oracle.Process
  where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad (forever, unless, void)
import Control.Monad.Except (ExceptT(ExceptT), liftIO, runExceptT)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
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
import Language.Marlowe.Runtime.App.Transact (apply)
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.ChainSync.Api (Address, TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Network.Oracle (Oracle, OracleEnv, readOracle, toOracleSymbol)
import Plutus.V2.Ledger.Api (toBuiltin)
import System.IO (hPutStrLn, stderr)

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Map.Strict as M (Map, adjust, delete, insert, lookup)
import qualified Data.Set as S (Set, insert, member)


runDiscovery
  :: Config
  -> Int
  -> IO (TChan ContractId)
runDiscovery config pollingFrequency =
  do
    channel <- newTChanIO
    void . forkIO
      $ either (hPutStrLn stderr) pure
      =<< runClientWithConfig config (streamAllContractIds pollingFrequency channel)
    pure channel


runDetection
  :: Config
  -> Int
  -> Party
  -> TChan ContractId
  -> IO (TChan (ContractStream 'V1))
runDetection config pollingFrequency party inChannel =
  do
    outChannel <- newTChanIO
    void . forkIO
      . forever
      . runClientWithConfig config
      -- FIXME: If `MarloweSyncClient` were a `Monad`, then we could run
      --        multiple actions sequentially in a single connection.
      $ do
        contractId <- liftIO . atomically $ readTChan inChannel
        -- FIXME: If there were concurrency combinators for `MarloweSyncClient`, then we
        --        could follow multiple contracts in parallel using the same connection.
        let
          finishOnClose = True
          finishOnWait = True
          accept = maybe False (not . null . containsOracleAction party) . contractFromStep
        streamContractSteps pollingFrequency finishOnClose finishOnWait accept contractId outChannel
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
  :: OracleEnv
  -> Config
  -> Int
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Party
  -> TChan (ContractStream 'V1)
  -> TChan ContractId
  -> IO ()
runOracle oracleEnv config pollingFrequency address key party inChannel outChannel =
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
              _                                     -> error
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
    report :: ContractId -> Oracle -> ExceptT String IO ()
    report contractId symbol =
      do
        value <- ExceptT $ readOracle oracleEnv symbol
        void
          . apply config address key contractId
          . pure . NormalInput
          $ IChoice (ChoiceId (toBuiltin . BS8.pack $ show symbol) party) value
        liftIO . hPutStrLn stderr
          $ "  " <> show symbol <> " = " <> show value <> " [scaled integer units]"
    -- Print the context of the transaction.
    printContext :: ContractId -> LastSeen -> [String] -> [Oracle] -> IO ()
    printContext contractId LastSeen{lastTxId} rawSymbols validSymbols =
      do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Contract ID: " <> (init . tail . LBS8.unpack . A.encode) contractId
        hPutStrLn stderr $ "  Previous transaction ID: " <> (init . tail . show) lastTxId
        hPutStrLn stderr $ "  Ready for oracle: " <> intercalate ", " (show <$> rawSymbols)
        hPutStrLn stderr $ "  Available from oracle: " <> intercalate ", " (show <$> validSymbols)
    -- Print the result of the transaction.
    printResult :: String -> IO ()
    printResult = hPutStrLn stderr . ("  " <>)
    -- Process the stream of changes to contracts.
    go :: M.Map ContractId LastSeen -> IO ()
    go lastSeen =
      do
        cs <- liftIO . atomically $ readTChan inChannel
        go =<<
          case cs of
            ContractStreamStart{}      -> pure $ update cs lastSeen
            ContractStreamContinued{}  -> pure $ update cs lastSeen
            ContractStreamRolledBack{} -> pure $ rollback cs lastSeen
            ContractStreamWait{..}     -> do
                                            let
                                              seen@LastSeen{lastTxId} =
                                                case csContractId `M.lookup` lastSeen of
                                                  Just seen' -> seen'
                                                  _          -> error
                                                                  $ "Invalid contract stream: seen = "
                                                                  <> show (csContractId `M.lookup` lastSeen)
                                                                  <> "."
                                              (rawSymbols, validSymbols, ignored) = lookupSymbols seen
                                            unless ignored
                                              $ do
                                                printContext csContractId seen rawSymbols validSymbols
                                                result <- runExceptT $ mapM_ (report csContractId) (take 1 validSymbols)
                                                printResult
                                                  $ case (result, null validSymbols) of
                                                      (Right ()    , True ) -> "Ignored."
                                                      (Right ()    , False) -> "Confirmed."
                                                      (Left message, _    ) -> "Failed: " <> message
                                            revisit csContractId
                                            pure $ ignore csContractId lastTxId lastSeen
            ContractStreamFinish{..}   -> pure $ delete csContractId lastSeen
  in
    go mempty
