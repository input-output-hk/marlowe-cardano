-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Run Marlowe contracts via the PAB.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.CLI.PAB (
-- * Types
  ApiRunner
, WsRunner
-- * Contracts
, runCompanion
-- * Endpoints
) where


import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, liftEither, liftIO, throwError)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Bifunctor (first)
import Data.ByteString.Builder (toLazyByteString)
import Data.Proxy (Proxy (..))
import Data.Text.Encoding (encodeUtf8Builder)
import Language.Marlowe.CLI.Types (CliError (..))
import Language.Marlowe.Client (CompanionState (..))
import Language.Marlowe.Contract (MarloweContract (..))
import Network.WebSockets (Connection, receiveData)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (PabClient (..))
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..), InstanceStatusToClient (..))
import Servant.Client (ClientM)
import System.IO (hPutStrLn, stderr)
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))


-- | Run in the PAB API.
type ApiRunner m a = ClientM a -> m a


-- | Run in a PAB websocket.
type WsRunner m a = ContractInstanceId -> (Connection -> ExceptT CliError IO a) -> m a


-- | Run the MarloweCompanion contract.
runCompanion :: MonadError CliError m
             => MonadIO m
             => PabClient MarloweContract WalletId  -- ^ The PAB client.
             -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
             -> (forall a. WsRunner m a)            -- ^ The Websockets runner.
             -> WalletId                            -- ^ The wallet ID.
             -> Bool                                -- ^ Whether to forever consume messages.
             -> m ()                                -- ^ Action for running the companion contracct.
runCompanion PabClient{..} runApi runWs walletId loop =
  do
    companionId <-
      runApi
        . activateContract
        $ ContractActivationArgs
          {
            caID     = WalletCompanion
          , caWallet = Just . Wallet $ walletId
          }
    liftIO . print $ unContractInstanceId companionId
    let
      go :: Connection -> ExceptT CliError IO ()
      go connection =
        do
          reportStatus (Proxy :: Proxy CompanionState)
            =<< receiveStatus connection
          go connection
    when loop
      $ runWs companionId go


-- | Report the contract status.
reportStatus :: forall a m
             .  MonadError CliError m
             => MonadIO m
             => FromJSON a
             => Show a
             => Proxy a                 -- ^ Proxy for the type of contract observation.
             -> InstanceStatusToClient  -- ^ The status message.
             -> m ()                    -- ^ Action to report the status.
reportStatus _ (NewObservableState s) =
  do
    state <- liftEither . first CliError $ parseEither parseJSON s
    liftIO . hPutStrLn stderr $ "New observable state: " <> show (state:: a)
reportStatus _ (NewActiveEndpoints aeps) =
  liftIO . hPutStrLn stderr $ "New active endpoints: " <> show aeps
reportStatus _ (NewYieldedExportTxs etxs) =
  liftIO . hPutStrLn stderr $ "Partial transactions that need balancing: " <> show etxs
reportStatus _ (ContractFinished v) =
  liftIO . hPutStrLn stderr $ "Contract finished" <> maybe "." ((": " <>) . show) v


-- | Receive the instance status.
receiveStatus :: Connection                                  -- ^ The websocket connection.
              -> ExceptT CliError IO InstanceStatusToClient  -- ^ The action to with the instance status.
receiveStatus connection =
  do
    message <- liftIO $ receiveData connection
    case eitherDecode . toLazyByteString $ encodeUtf8Builder message of
       Right status -> pure status
       Left  e      -> throwError $ CliError e
