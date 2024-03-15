{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Runtime.App.Transact (
  App,
  apply,
  applyWithEvents,
  create,
  createWithEvents,
  handleWithEvents,
  run,
  runWithEvents,
  transact,
  transactWithEvents,
  transact',
  transactWithEvents',
) where

import Control.Concurrent (threadDelay)
import Control.Monad (join, when)
import Control.Monad.Except (ExceptT (..), catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input)
import Language.Marlowe.Runtime.App.Types (
  App,
  Config (Config, buildSeconds, confirmSeconds, retryLimit, retrySeconds),
  MarloweRequest (..),
  MarloweResponse (..),
  TxBodyInEraWithReferenceScripts (TxBodyInEraWithReferenceScripts),
  TxInEraWithReferenceScripts (TxInEraWithReferenceScripts),
 )
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag (V1))
import Observe.Event.Dynamic (DynamicEventSelector (..))
import Observe.Event.Explicit (EventBackend, addField, idInjectSelector, subEventBackend, unitEventBackend, withEvent)
import Observe.Event.Syntax ((≔))
import System.Random (randomRIO)

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Text as T (Text)
import qualified Language.Marlowe.Runtime.App as App (handle)

run
  :: Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [[Input]]
  -> Maybe Lovelace
  -> App ContractId
run = runWithEvents unitEventBackend

runWithEvents
  :: EventBackend App r DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [[Input]]
  -> Maybe Lovelace
  -> App ContractId
runWithEvents backend config address key contract inputs minUtxo =
  do
    let transact'' = transactWithEvents' backend config key
    contractId <- transact'' $ Create contract mempty minUtxo mempty mempty address mempty
    mapM_ (\input -> transact'' $ Apply contractId input Nothing Nothing mempty mempty address mempty) inputs
    pure contractId

create
  :: Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> Maybe Lovelace
  -> App ContractId
create = createWithEvents unitEventBackend

createWithEvents
  :: EventBackend App r DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> Maybe Lovelace
  -> App ContractId
createWithEvents backend config address key contract minUtxo =
  transactWithEvents' backend config key $
    Create contract mempty minUtxo mempty mempty address mempty

apply
  :: Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> ContractId
  -> [Input]
  -> App ContractId
apply = applyWithEvents unitEventBackend

applyWithEvents
  :: EventBackend App r DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> ContractId
  -> [Input]
  -> App ContractId
applyWithEvents backend config address key contractId input =
  transactWithEvents' backend config key $
    Apply contractId input Nothing Nothing mempty mempty address mempty

transact'
  :: Config
  -> C.SigningKey C.PaymentExtendedKey
  -> MarloweRequest 'V1
  -> App ContractId
transact' = transactWithEvents' unitEventBackend

transact
  :: Config
  -> C.SigningKey C.PaymentExtendedKey
  -> MarloweRequest 'V1
  -> App (Maybe ContractId)
transact = transactWithEvents unitEventBackend

transactWithEvents'
  :: EventBackend App r DynamicEventSelector
  -> Config
  -> C.SigningKey C.PaymentExtendedKey
  -> MarloweRequest 'V1
  -> App ContractId
transactWithEvents' backend config key request = do
  mContractId <- transactWithEvents backend config key request
  maybe (fail "Contract ID expected") pure mContractId

transactWithEvents
  :: EventBackend App r DynamicEventSelector
  -> Config
  -> C.SigningKey C.PaymentExtendedKey
  -> MarloweRequest 'V1
  -> App (Maybe ContractId)
transactWithEvents backend config@Config{buildSeconds, confirmSeconds, retryLimit, retrySeconds} key request =
  let show' = LBS8.unpack . A.encode
      unexpected response = throwError $ "Unexpected response: " <> show' response
   in retry "Transact" backend [retrySeconds * 2 ^ (i - 1) | retrySeconds > 0, retryLimit > 0, i <- [1 .. retryLimit]] $
        \subBackend ->
          do
            when (buildSeconds > 0)
              . withEvent subBackend (DynamicEventSelector "WaitBeforeBuild")
              . const
              $ liftIO . threadDelay . (buildSeconds *)
                =<< randomRIO (1_000_000, 2_000_000)
            (contractId, TxBodyInEraWithReferenceScripts era body) <-
              handleWithEvents subBackend "Build" config request $
                \case
                  Body{..} -> pure (resContractId, TxBodyInEraWithReferenceScripts resTxEra resTxBody)
                  response -> unexpected response
            TxInEraWithReferenceScripts era' tx <-
              handleWithEvents subBackend "Sign" config (Sign era body [] [key]) $
                \case
                  Tx{..} -> pure $ TxInEraWithReferenceScripts resTxEra resTx
                  response -> unexpected response
            _txId' <-
              handleWithEvents subBackend "Submit" config (Submit era' tx 1) $
                \case
                  TxId{..} -> pure resTxId
                  response -> unexpected response
            {-
                      handleWithEvents subBackend "Confirm" config (Wait txId' 1)
                        $ \case
                          TxInfo{} -> pure ()
                          response -> unexpected response
            -}
            when (confirmSeconds > 0)
              . withEvent subBackend (DynamicEventSelector "WaitAfterConfirm")
              . const
              $ liftIO . threadDelay . (confirmSeconds *)
                =<< randomRIO (1_000_000, 2_000_000)
            pure contractId

retry
  :: T.Text
  -> EventBackend App r DynamicEventSelector
  -> [Int]
  -> (EventBackend App r DynamicEventSelector -> App a)
  -> App a
retry name backend [] action = withEvent backend (DynamicEventSelector name) \ev ->
  action $ subEventBackend idInjectSelector ev backend
retry name backend (delay : delays) action =
  join $ withEvent backend (DynamicEventSelector name) \ev ->
    let subBackend = subEventBackend idInjectSelector ev backend
     in (pure <$> action subBackend) `catchError` \message -> do
          addField ev $ ("failedAttempt" :: T.Text) ≔ message
          addField ev $ ("waitForRetry" :: T.Text) ≔ delay
          pure do
            liftIO . threadDelay $ delay * 1_000_000
            retry name backend delays action

handleWithEvents
  :: EventBackend App r DynamicEventSelector
  -> T.Text
  -> Config
  -> MarloweRequest 'V1
  -> (MarloweResponse 'V1 -> App a)
  -> App a
handleWithEvents backend name config request extract =
  withEvent backend (DynamicEventSelector name) $
    \ev ->
      do
        addField ev $ ("request" :: T.Text) ≔ request
        response <- ExceptT $ App.handle config request
        addField ev $ ("response" :: T.Text) ≔ response
        extract response
