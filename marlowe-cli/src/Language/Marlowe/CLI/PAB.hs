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
{-# LANGUAGE TupleSections       #-}


module Language.Marlowe.CLI.PAB (
-- * Types
  ApiRunner
, WsRunner
-- * Contracts
, runApp
, runFollower
, runCompanion
, stop
-- * Endpoints
, callCreate
, callApplyInputs
, callRedeem
, callFollow
-- * Utilities
, receiveStatus
) where


import Cardano.Api (AddressAny, AddressInEra, ShelleyEra, anyAddressInShelleyBasedEra)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, liftEither, liftIO, throwError)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, toJSON)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Bifunctor (first, second)
import Data.ByteString.Builder (toLazyByteString)
import Data.Proxy (Proxy (..))
import Data.Text.Encoding (encodeUtf8Builder)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.CLI.IO (decodeFileStrict, liftCli, maybeWriteJson)
import Language.Marlowe.CLI.Types (CliError (..))
import Language.Marlowe.Client (ApplyInputsEndpointSchema, CompanionState, CreateEndpointSchema, EndpointResponse (..),
                                FollowerContractState, MarloweClientInput, MarloweContractState,
                                MarloweEndpointResult (CreateResponse), MarloweError, RedeemEndpointSchema)
import Language.Marlowe.Contract (MarloweContract (..))
import Language.Marlowe.Semantics (MarloweParams)
import Language.Marlowe.SemanticsTypes (Contract)
import Network.WebSockets (Connection, receiveData)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (InstanceClient (..), PabClient (..))
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..), InstanceStatusToClient (..))
import Plutus.V1.Ledger.Api (POSIXTime (..), TokenName)
import Servant.Client (ClientM)
import System.IO (hPutStrLn, stderr)
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))

import qualified PlutusTx.AssocMap as AM (Map, fromList)


-- | Run in the PAB API.
type ApiRunner m a = ClientM a -> m a


-- | Run in a PAB websocket.
type WsRunner m a = ContractInstanceId -> (Connection -> ExceptT CliError IO a) -> m a


-- | Run the MarloweApp contract.
runApp :: MonadError CliError m
       => MonadIO m
       => Maybe FilePath                      -- ^ The output file for the Marlowe parameters.
       -> Bool                                -- ^ Whether to consume messages until the contract stops.
       -> PabClient MarloweContract WalletId  -- ^ The PAB client.
       -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
       -> (forall a. WsRunner m a)            -- ^ The Websockets runner.
       -> WalletId                            -- ^ The wallet ID.
       -> Maybe FilePath                      -- ^ The output file for the instance ID.
       -> m ()                                -- ^ Action for running the Marlowe contract.
runApp paramsFile loop =
  runContract
    MarloweApp
    (Proxy :: Proxy MarloweContractState)
    (reportAppStatus paramsFile loop)
    True


-- | Report the Marlowe contract status, writing parameters.
reportAppStatus :: MonadError CliError m
                => MonadIO m
                => Maybe FilePath              -- ^ The output file for the Marlowe parameters.
                -> Bool                        -- ^ Whether to consume messages until the contract stops.
                -> Proxy MarloweContractState  -- ^ Proxy for the type of contract observation.
                -> InstanceStatusToClient      -- ^ The status message.
                -> m Bool                      -- ^ Action to report the status and whether the contract has finished.
reportAppStatus paramsFile loop p s =
  do
    finish <- reportStatus p s
    case s of
      NewObservableState s' -> do
                                 state <- liftCli $ parseEither parseJSON s'
                                 case state :: Maybe (EndpointResponse MarloweEndpointResult MarloweError) of
                                   Just (EndpointSuccess _ (CreateResponse params)) -> do
                                                                                         maybeWriteJson paramsFile params
                                                                                         pure $ not loop
                                   _                     -> pure finish
      _                    -> pure finish


-- | Run the MarloweFollower contract.
runFollower :: MonadError CliError m
            => MonadIO m
            => Bool                                -- ^ Whether to consume messages until the contract stops.
            -> PabClient MarloweContract WalletId  -- ^ The PAB client.
            -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
            -> (forall a. WsRunner m a)            -- ^ The Websockets runner.
            -> WalletId                            -- ^ The wallet ID.
            -> Maybe FilePath                      -- ^ The output file for the instance ID.
            -> m ()                                -- ^ Action for running the follower contract.
runFollower = runContract MarloweFollower (Proxy :: Proxy FollowerContractState) reportStatus


-- | Run the WalletCompanion contract.
runCompanion :: MonadError CliError m
             => MonadIO m
             => Bool                                -- ^ Whether to consume messages until the contract stops.
             -> PabClient MarloweContract WalletId  -- ^ The PAB client.
             -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
             -> (forall a. WsRunner m a)            -- ^ The Websockets runner.
             -> WalletId                            -- ^ The wallet ID.
             -> Maybe FilePath                      -- ^ The output file for the instance ID.
             -> m ()                                -- ^ Action for running the companion contract.
runCompanion = runContract WalletCompanion (Proxy :: Proxy CompanionState) reportStatus


-- | Run a Marlowe contract.
runContract :: MonadError CliError m
            => MonadIO m
            => MarloweContract                     -- ^ The contract.
            -> Proxy a                             -- ^ Proxy for the type of contract observation.
            -> ReportStatus a                      -- ^ The status reporter.
            -> Bool                                -- ^ Whether to consume messages until the contract stops.
            -> PabClient MarloweContract WalletId  -- ^ The PAB client.
            -> (forall b. ApiRunner m b)           -- ^ The HTTP runner.
            -> (forall b. WsRunner m b)            -- ^ The Websockets runner.
            -> WalletId                            -- ^ The wallet ID.
            -> Maybe FilePath                      -- ^ The output file for the instance ID.
            -> m ()                                -- ^ Action for running the contract.
runContract contract proxy reporter loop PabClient{..} runApi runWs walletId outputFile =
  do
    instanceId <-
      runApi
        . activateContract
        $ ContractActivationArgs
          {
            caID     = contract
          , caWallet = Just $ Wallet Nothing walletId
          }
    maybeWriteJson outputFile instanceId
    let
      go :: Connection -> ExceptT CliError IO ()
      go connection =
        do
          status <- receiveStatus connection
          finished <- reporter proxy status
          unless finished
            $ go connection
    when loop
      $ runWs instanceId go


-- | Function for reporting status.
type ReportStatus a =  Proxy a                     -- ^ Proxy for the type of contract observation.
                      -> InstanceStatusToClient    -- ^ The status message.
                      -> ExceptT CliError IO Bool  -- ^ Action to report the status and whether the contract has finished.


-- | Report the contract status.
reportStatus :: forall a m
             .  MonadError CliError m
             => MonadIO m
             => FromJSON a
             => Show a
             => Proxy a                 -- ^ Proxy for the type of contract observation.
             -> InstanceStatusToClient  -- ^ The status message.
             -> m Bool                  -- ^ Action to report the status and whether the contract has finished.
reportStatus _ (NewObservableState s) =
  do
    state <- liftEither . first CliError $ parseEither parseJSON s
    liftIO . hPutStrLn stderr $ "New observable state: " <> show (state:: a)
    pure False
reportStatus _ (NewActiveEndpoints aeps) =
  do
    liftIO . hPutStrLn stderr $ "New active endpoints: " <> show aeps
    pure False
reportStatus _ (NewYieldedExportTxs etxs) =
  do
    liftIO . hPutStrLn stderr $ "Partial transactions that need balancing: " <> show etxs
    pure False
reportStatus _ (ContractFinished v) =
  do
    liftIO . hPutStrLn stderr $ "Contract finished" <> maybe "." ((": " <>) . show) v
    pure True


-- | Receive the instance status.
receiveStatus :: Connection                                  -- ^ The websocket connection.
              -> ExceptT CliError IO InstanceStatusToClient  -- ^ The action to with the instance status.
receiveStatus connection =
  do
    message <- liftIO $ receiveData connection
--  liftIO . putStrLn $ "Received message: " <> show message
    case eitherDecode . toLazyByteString $ encodeUtf8Builder message of
       Right status -> pure status
       Left  e      -> throwError $ CliError e


-- | Stop a contract.
stop :: MonadError CliError m
     => MonadIO m
     => PabClient MarloweContract WalletId  -- ^ The PAB client.
     -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
     -> FilePath                            -- ^ File containing the contract instance ID.
     -> m ()                                -- ^ Action to stop the contract.
stop PabClient{..} runApi instanceFile =
  do
    instanceId <- decodeFileStrict instanceFile
    let
      InstanceClient{..} = instanceClient instanceId
    runApi
      stopInstance


-- | Call the "create" endpoint.
callCreate :: MonadError CliError m
           => MonadIO m
           => PabClient MarloweContract WalletId  -- ^ The PAB client.
           -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
           -> FilePath                            -- ^ File containing the contract instance ID.
           -> FilePath                            -- ^ The JSON file containing the contract.
           -> [(TokenName, AddressAny)]           -- ^ The contract role names and their owners' addresses.
           -> m ()                                -- ^ Action for calling the "create" endpoint.
callCreate pabClient runApi instanceFile contractFile owners =
  do
    contract <- decodeFileStrict contractFile
    call pabClient runApi instanceFile "create"
      ((
      , AM.fromList $ second anyAddressInShelleyBasedEra <$> owners :: AM.Map TokenName (AddressInEra ShelleyEra)
      , contract :: Contract
      ) :: UUID -> CreateEndpointSchema)


-- | Call the "apply-inputs" endpoint.
callApplyInputs :: MonadError CliError m
                => MonadIO m
                => PabClient MarloweContract WalletId  -- ^ The PAB client.
                -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
                -> FilePath                            -- ^ File containing the contract instance ID.
                -> FilePath                            -- ^ The JSON file containing the Marlowe parameters.
                -> [MarloweClientInput]                -- ^ The inputs to the contract.
                -> POSIXTime                           -- ^ The first valid time for the transaction.
                -> POSIXTime                           -- ^ The last valid time for the transaction.
                -> m ()                                -- ^ Action for calling the "apply-inputs" endpoint.
callApplyInputs pabClient runApi instanceFile paramsFile inputs minimumTime maximumTime =
  do
    params <- decodeFileStrict paramsFile
    call pabClient runApi instanceFile "apply-inputs"
      ((
      , params :: MarloweParams
      , Just (minimumTime, maximumTime)
      , inputs
      ) :: UUID -> ApplyInputsEndpointSchema)


-- | Call the "redeem" endpoint.
callRedeem :: MonadError CliError m
           => MonadIO m
           => PabClient MarloweContract WalletId  -- ^ The PAB client.
           -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
           -> FilePath                            -- ^ File containing the contract instance ID.
           -> FilePath                            -- ^ The JSON file containing the Marlowe parameters.
           -> (TokenName, AddressAny)             -- ^ The contract role name and their owner's addresses.
           -> m ()                                -- ^ Action for calling the "redeem" endpoint.
callRedeem pabClient runApi instanceFile paramsFile (ownerName, ownerAddress) =
  do
    params <- decodeFileStrict paramsFile
    call pabClient runApi instanceFile "redeem"
      ((
      , params :: MarloweParams
      , ownerName
      , anyAddressInShelleyBasedEra ownerAddress :: AddressInEra ShelleyEra
      ) :: UUID -> RedeemEndpointSchema)


-- | Call the "follow" endpoint.
callFollow :: MonadError CliError m
           => MonadIO m
           => PabClient MarloweContract WalletId  -- ^ The PAB client.
           -> (forall a. ApiRunner m a)           -- ^ The HTTP runner.
           -> FilePath                            -- ^ File containing the contract instance ID.
           -> FilePath                            -- ^ The JSON file containing the Marlowe parameters.
           -> m ()                                -- ^ Action for calling the "follow" endpoint.
callFollow pabClient runApi instanceFile paramsFile =
  do
    params <- decodeFileStrict paramsFile
    call pabClient runApi instanceFile "follow"
      $ const
      (params :: MarloweParams)


-- | Call the an endpoint.
call :: MonadError CliError m
     => MonadIO m
     => ToJSON a
     => PabClient MarloweContract WalletId  -- ^ The PAB client.
     -> (forall b. ApiRunner m b)           -- ^ The HTTP runner.
     -> FilePath                            -- ^ File containing the contract instance ID.
     -> String                              -- ^ The name of the endpoint.
     -> (UUID -> a)                         -- ^ Function for populating the arguments to the endpoint.
     -> m ()                                -- ^ Action for calling the endpoint.
call PabClient{..} runApi instanceFile endpoint arguments =
  do
    instanceId <- decodeFileStrict instanceFile
    uuid <- liftIO nextRandom
    let
      InstanceClient{..} = instanceClient instanceId
    runApi
      . callInstanceEndpoint endpoint
      . toJSON
      $ arguments uuid
