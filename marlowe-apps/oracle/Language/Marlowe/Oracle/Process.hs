

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Oracle.Process
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan)
import Control.Monad (forever, void)
import Control.Monad.Except (ExceptT(ExceptT), liftIO, runExceptT)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.Marlowe.Core.V1.Semantics.Types
  (ChoiceId(ChoiceId), Contract, Input(NormalInput), InputContent(IChoice), Party)
import Language.Marlowe.Oracle.Detect (containsOracleAction, contractReadyForOracle)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.App.Stream
  (ContractStream(..), contractFromStep, contractFromStream, streamAllContractIds, streamContractSteps)
import Language.Marlowe.Runtime.App.Transact (apply)
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.ChainSync.Api (Address)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Network.Oracle (OracleEnv, readOracle, toOracleSymbol)
import Plutus.V2.Ledger.Api (toBuiltin)
import System.IO (hPutStrLn, stderr)

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Map.Strict as M (Map, delete, insert, lookup)


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
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Party
  -> TChan (ContractStream 'V1)
  -> IO ()
runOracle oracleEnv config address key party channel =
  let
    go contracts =
      do
        cs <- liftIO . atomically $ readTChan channel
        go =<<
          case cs of
            ContractStreamFinish{..} -> pure
                                          $ csContractId `M.delete` contracts
            ContractStreamWait{..}   -> do
                                          let
                                            rawSymbols =
                                              fromMaybe mempty
                                                $ contractReadyForOracle party
                                                <$> (csContractId `M.lookup` contracts)
                                            validSymbols = mapMaybe toOracleSymbol rawSymbols
                                            report symbol =
                                              do
                                                value <- ExceptT $ readOracle oracleEnv symbol
                                                void
                                                  . apply config address key csContractId
                                                  . pure . NormalInput
                                                  $ IChoice (ChoiceId (toBuiltin . BS8.pack $ show symbol) party) value
                                                liftIO . hPutStrLn stderr
                                                  $ "  " <> show symbol <> " = " <> show value <> " [NB: scaled integer units]"
                                          hPutStrLn stderr ""
                                          hPutStrLn stderr $ "Contract ID: " <> (init . tail . LBS8.unpack . A.encode) csContractId
                                          hPutStrLn stderr $ "  Ready for oracle: " <> intercalate ", " (show <$> rawSymbols)
                                          hPutStrLn stderr $ "  Available from oracle: " <> intercalate ", " (show <$> validSymbols)
                                          runExceptT (mapM_ report  $ take 1 validSymbols)
                                            >>= \result ->
                                              hPutStrLn stderr
                                                $ case result of
                                                  Right ()     -> (if null validSymbols then "  Ignored." else "  Confirmed.")
                                                  Left message -> "  Failed: " <> message
                                          pure contracts
            _                        -> pure
                                          . maybe contracts (flip (M.insert $ csContractId cs) contracts)
                                          $ contractFromStream cs
  in
    go (mempty :: M.Map ContractId Contract)
