

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main
  ) where


import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM_, void)
import Control.Monad.Except (MonadIO, liftIO, runExceptT, throwError)
import Data.List.Split (chunksOf)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(..)
  , Bound(Bound)
  , Case(..)
  , ChoiceId(..)
  , Contract(..)
  , Input(NormalInput)
  , InputContent(..)
  , Observation(TrueObs)
  , Party(Address)
  , Token(..)
  , Value(ChoiceValue)
  )
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddress)
import Language.Marlowe.Runtime.ChainSync.Api (Address(unAddress), TxId, fromBech32)
import Language.Marlowe.Runtime.Client (App, handleWithEvents)
import Language.Marlowe.Runtime.Client.Types (Config, MarloweRequest(..), MarloweResponse(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Observe.Event (EventBackend, addField, hoistEventBackend, withEvent)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(defaultRenderSelectorJSON))
import Observe.Event.Render.JSON.Handle (JSONRef, simpleJsonStderrBackend)
import Observe.Event.Syntax ((≔))
import Plutus.V2.Ledger.Api (POSIXTime(..))
import System.Environment (getArgs)
import System.Random (randomRIO)

import qualified Cardano.Api as C
import qualified Data.Aeson as A (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Text as T (Text, pack)
import qualified Data.Time.Clock.POSIX as P (getPOSIXTime)


makeContract
  :: POSIXTime
  -> Party
  -> Contract
makeContract timeout party =
  When
    [
      Case (Choice (ChoiceId "Amount" party) [Bound 1 2000000])
        $ When
        [
           Case (Deposit party party (Token "" "") (ChoiceValue $ ChoiceId "Amount" party))
             $ When
             [
               Case (Notify TrueObs)
                 Close
             ]
             timeout
             Close
        ]
        timeout
        Close
    ]
    timeout
    Close


makeInputs
  :: Party
  -> Integer
  -> [InputContent]
makeInputs party amount =
  [
    IChoice (ChoiceId "Amount" party) amount
  , IDeposit party party (Token "" "") amount
  , INotify
  ]


randomInputs
  :: MonadIO m
  => Party
  -> m [InputContent]
randomInputs = (<$> randomRIO (1_000_000, 2_000_000)) . makeInputs


runScenario
  :: EventBackend App JSONRef DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [InputContent]
  -> App ContractId
runScenario eventBackend config address key contract inputs =
  do
    let
      unexpected response = throwError $ "Unexpected response: " <> show' response
      transact request =
        withEvent eventBackend (DynamicEventSelector "Transact")
          $ \event ->
            do
              threadId <- liftIO myThreadId
              addField event $ ("threadId" :: T.Text) ≔ show threadId
              (contractId, body) <-
                handle' eventBackend config request
                  $ \case
                    Body{..} -> pure (resContractId, resTransactionBody)
                    response -> unexpected response
              tx <-
                handle' eventBackend config (Sign body [] [key])
                  $ \case
                    Tx{..}   -> pure resTransaction
                    response -> unexpected response
              txId' <-
                handle' eventBackend config (Submit tx)
                  $ \case
                    TxId{..} -> pure resTransactionId
                    response -> unexpected response
              waitForConfirmation eventBackend config txId'
              pure contractId
    contractId <- transact $ Create contract mempty 1_500_000 mempty address mempty
    mapM_ (\input -> transact $ Apply contractId [NormalInput input] Nothing Nothing mempty address mempty) inputs
    pure contractId


show' :: A.ToJSON a => a -> String
show' = LBS8.unpack . A.encode


handle'
  :: EventBackend App JSONRef DynamicEventSelector
  -> Config
  -> MarloweRequest 'V1
  -> (MarloweResponse 'V1 -> App a)
  -> App a
handle' eventBackend config request extract =
  extract =<< handleWithEvents eventBackend config request



waitForConfirmation
  :: EventBackend App JSONRef DynamicEventSelector
  -> Config
  -> TxId
  -> App ()
waitForConfirmation eventBackend config txId =
  handle' eventBackend config (Wait txId)
    $ \case
      TxId{} -> pure ()
      response -> throwError $ "Unexpected response: " <> show' response


currentTime :: MonadIO m => m POSIXTime
currentTime = POSIXTime . floor . (* 1000) . nominalDiffTimeToSeconds <$> liftIO P.getPOSIXTime


runOne
  :: EventBackend App JSONRef DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> App ContractId
runOne eventBackend config address key =
  do
    now <- currentTime
    party <-
      case deserialiseAddress $ unAddress address of
        Just address' -> pure $ uncurry Address address'
        Nothing       -> throwError "Address conversion failed."
    let
      contract = makeContract (now + 15 * 60 * 1000) party
    inputs <- randomInputs party
    runScenario eventBackend config address key contract inputs


main :: IO ()
main =
   do
     eventBackend <- hoistEventBackend liftIO <$> simpleJsonStderrBackend defaultRenderSelectorJSON
     configFilename : countText : addressKeyEntries <- getArgs
     config <- read <$> readFile configFilename
     let
       count = read countText
     addressKeys <-
       sequence
         [
           do
             Just address <- pure . fromBech32 $ T.pack addressBech32
             Right key <- C.readFileTextEnvelope (C.AsSigningKey C.AsPaymentExtendedKey) keyFilename
             pure (address, key)
         |
           [addressBech32, keyFilename] <- chunksOf 2 addressKeyEntries
         ]
     void
       $ mapConcurrently
         (\(address, key) -> replicateM_ count $ print =<< runExceptT (runOne eventBackend config address key))
         addressKeys
