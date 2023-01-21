

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Runtime.App.Transact
  ( App
  , apply
  , applyWithEvents
  , create
  , createWithEvents
  , handleWithEvents
  , run
  , runWithEvents
  , transact
  , transactWithEvents
  ) where


import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Except (ExceptT(..), liftIO, runExceptT, throwError)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input)
import Language.Marlowe.Runtime.App.Types
  (App, Config(Config, buildSeconds, confirmSeconds, retryLimit, retrySeconds), MarloweRequest(..), MarloweResponse(..))
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Observe.Event (Event, addField, newEvent, withSubEvent)
import Observe.Event.Backend (unitEventBackend)
import Observe.Event.Dynamic (DynamicEvent, DynamicEventSelector(..), DynamicField)
import Observe.Event.Syntax ((≔))
import System.Random (randomRIO)

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Text as T (Text)
import qualified Language.Marlowe.Runtime.App as App (handle)


unitEvent :: App (DynamicEvent App ())
unitEvent = newEvent unitEventBackend $ DynamicEventSelector "unit"


run
  :: Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [[Input]]
  -> Lovelace
  -> App ContractId
run config address key contract inputs minUtxo =
  do
    event <- unitEvent
    runWithEvents event config address key contract inputs minUtxo


runWithEvents
  :: Event App r DynamicEventSelector f
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [[Input]]
  -> Lovelace
  -> App ContractId
runWithEvents event config address key contract inputs minUtxo =
  do
    let
      transact' = transactWithEvents event config key
    contractId <- transact' $ Create contract mempty minUtxo mempty mempty address mempty
    mapM_ (\input -> transact' $ Apply contractId input Nothing Nothing mempty mempty address mempty) inputs
    pure contractId


create
  :: Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> Lovelace
  -> App ContractId
create config address key contract minUtxo =
  do
    event <- unitEvent
    createWithEvents event config address key contract minUtxo


createWithEvents
  :: Event App r DynamicEventSelector f
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> Lovelace
  -> App ContractId
createWithEvents event config address key contract minUtxo =
  transactWithEvents event config key
    $ Create contract mempty minUtxo mempty mempty address mempty


apply
  :: Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> ContractId
  -> [Input]
  -> App ContractId
apply config address key contractId input =
  do
    event <- unitEvent
    applyWithEvents event config address key contractId input


applyWithEvents
  :: Event App r DynamicEventSelector f
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> ContractId
  -> [Input]
  -> App ContractId
applyWithEvents event config address key contractId input =
  transactWithEvents event config key
    $ Apply contractId input Nothing Nothing mempty mempty address mempty


transact
  :: Config
  -> C.SigningKey C.PaymentExtendedKey
  -> MarloweRequest 'V1
  -> App ContractId
transact config key request =
  do
    event <- unitEvent
    transactWithEvents event config key request


transactWithEvents
  :: Event App r DynamicEventSelector f
  -> Config
  -> C.SigningKey C.PaymentExtendedKey
  -> MarloweRequest 'V1
  -> App ContractId
transactWithEvents event config@Config{buildSeconds, confirmSeconds, retryLimit, retrySeconds} key request =
  let
    show' = LBS8.unpack . A.encode
    unexpected response = throwError $ "Unexpected response: " <> show' response
  in
    retry "Transact" event [retrySeconds * 2^(i-1) | i <- [1..retryLimit]]
      $ \subEvent ->
        do
          when (buildSeconds > 0)
            . withSubEvent subEvent (DynamicEventSelector "WaitBeforeBuild")
            . const
            $ liftIO . threadDelay . (buildSeconds *)
            =<< randomRIO (500_000, 1_500_000)
          (contractId, body) <-
            handleWithEvents subEvent "Build" config request
              $ \case
                Body{..} -> pure (resContractId, resTxBody)
                response -> unexpected response
          tx <-
            handleWithEvents subEvent "Sign" config (Sign body [] [key])
              $ \case
                Tx{..}   -> pure resTx
                response -> unexpected response
          txId' <-
            handleWithEvents subEvent "Submit" config (Submit tx)
              $ \case
                TxId{..} -> pure resTxId
                response -> unexpected response
          handleWithEvents subEvent "Confirm" config (Wait txId' 1)
            $ \case
              TxInfo{} -> pure ()
              response -> unexpected response
          when (confirmSeconds > 0)
            . withSubEvent subEvent (DynamicEventSelector "WaitAfterConfirm")
            . const
            $ liftIO . threadDelay . (confirmSeconds *)
            =<< randomRIO (500_000, 1_500_000)
          pure contractId


retry
  :: T.Text
  -> Event App r DynamicEventSelector f
  -> [Int]
  -> (Event App r DynamicEventSelector DynamicField -> App a)
  -> App a
retry name event [] action = withSubEvent event (DynamicEventSelector name) action
retry name event (delay : delays) action =
  withSubEvent event (DynamicEventSelector name)
    $ \subEvent ->
      ExceptT
        $ runExceptT (action subEvent)
        >>= \case
          Right result -> pure $ Right result
          Left message -> runExceptT
                            $ do
                              addField subEvent $ ("failure" :: T.Text) ≔ message
                              addField subEvent $ ("waitForRetry" :: T.Text) ≔ delay
                              liftIO . threadDelay $ delay * 1_000_000
                              retry name event delays action


handleWithEvents
  :: Event App r DynamicEventSelector f
  -> T.Text
  -> Config
  -> MarloweRequest 'V1
  -> (MarloweResponse 'V1 -> App a)
  -> App a
handleWithEvents event name config request extract =
  withSubEvent event (DynamicEventSelector name)
    $ \subEvent ->
      do
        addField subEvent $ ("request" :: T.Text) ≔ request
        response <- ExceptT $ App.handle config request
        addField subEvent $ ("response" :: T.Text) ≔ response
        extract response
