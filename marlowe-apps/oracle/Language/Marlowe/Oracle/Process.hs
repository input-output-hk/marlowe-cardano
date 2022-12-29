

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


module Language.Marlowe.Oracle.Process
  where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Monad (forever, void)
import Control.Monad.Except (ExceptT(ExceptT), liftIO, runExceptT)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.String (fromString)
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
import Network.Oracle (OracleEnv, readOracle, toOracleSymbol)
import Plutus.V2.Ledger.Api (toBuiltin)
import System.IO (hPutStrLn, stderr)

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Map.Strict as M (Map, delete, insert, lookup)
import qualified Data.Set as S (Set, delete, insert, member)


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
    go :: M.Map ContractId (TxId, Contract) -> S.Set TxId -> IO ()
    go contracts ignored =
      do
        cs <- liftIO . atomically $ readTChan inChannel
        uncurry go =<<
          case cs of
            ContractStreamFinish{..} -> pure
                                          (
                                            csContractId `M.delete` contracts
                                          , maybe ignored ((`S.delete` ignored) . fst) $ csContractId `M.lookup` contracts
                                          )
            ContractStreamWait{..}   -> do
                                          -- FIXME: Break this into separate functions.
                                          let
                                            maybeContract = csContractId `M.lookup` contracts
                                            rawSymbols = maybe mempty (contractReadyForOracle party . snd) maybeContract
                                            transactionId = maybe (fromString "" :: TxId) fst maybeContract
                                            ignore = transactionId `S.member` ignored
                                            validSymbols = mapMaybe toOracleSymbol rawSymbols
                                            report symbol =
                                              do
                                                value <- ExceptT $ readOracle oracleEnv symbol
                                                void
                                                  . apply config address key csContractId
                                                  . pure . NormalInput
                                                  $ IChoice (ChoiceId (toBuiltin . BS8.pack $ show symbol) party) value
                                                liftIO . hPutStrLn stderr
                                                  $ "  " <> show symbol <> " = " <> show value <> " [scaled integer units]"
                                          -- FIXME: This is a workaround for contract discovery
                                          --        not tailing past the tip of the blockchain.
                                          ignored' <-
                                            if ignore
                                              then pure ignored
                                              else do
                                                hPutStrLn stderr ""
                                                hPutStrLn stderr $ "Contract ID: " <> (init . tail . LBS8.unpack . A.encode) csContractId
                                                hPutStrLn stderr $ "  Transaction ID: " <> (init . tail . show) transactionId
                                                hPutStrLn stderr $ "  Ready for oracle: " <> intercalate ", " (show <$> rawSymbols)
                                                hPutStrLn stderr $ "  Available from oracle: " <> intercalate ", " (show <$> validSymbols)
                                                runExceptT (mapM_ report  $ take 1 validSymbols)
                                                  >>= \result ->
                                                    case (result, null validSymbols) of
                                                      (Right ()    , True ) -> do
                                                                                 hPutStrLn stderr "  Ignored."
                                                                                 pure $ transactionId `S.insert` ignored
                                                      (Right ()    , False) -> do
                                                                                 hPutStrLn stderr "  Confirmed."
                                                                                 pure $ transactionId `S.delete` ignored
                                                      (Left message, _    ) -> do
                                                                                 hPutStrLn stderr $ "  Failed: " <> message
                                                                                 pure $ transactionId `S.insert` ignored
                                          -- FIXME: This is a workaround for contract discovery
                                          --        not tailing past the tip of the blockchain.
                                          void . forkIO
                                            $ threadDelay pollingFrequency
                                            >> atomically (writeTChan outChannel csContractId)
                                          hPutStrLn stderr $ show ignored
                                          pure (contracts, ignored')
            _                        -> let
                                          transactionId = maybe (fromString "" :: TxId) id $ transactionIdFromStream cs
                                        in
                                          pure
                                            (
                                              maybe contracts (flip (M.insert $ csContractId cs) contracts . (transactionId, ))
                                                $ contractFromStream cs
                                            , transactionId `S.delete` ignored
                                            )
  in
    go mempty mempty
