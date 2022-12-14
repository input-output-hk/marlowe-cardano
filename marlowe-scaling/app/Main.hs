

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


import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM_, void)
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT, throwError)
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
import Language.Marlowe.Runtime.ChainSync.Api (Address(unAddress), TxId, TxIx(..), TxOutRef(..), fromBech32)
import Language.Marlowe.Runtime.Client (handle)
import Language.Marlowe.Runtime.Client.Types (Config, MarloweRequest(..), MarloweResponse(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(unContractId), MarloweVersionTag(V1))
import Plutus.V2.Ledger.Api (POSIXTime(..))
import System.Environment (getArgs)
import System.Random (randomRIO)

import qualified Cardano.Api as C
import qualified Data.Aeson as A (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Data.Text as T (pack)
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
  :: MonadError String m
  => MonadIO m
  => Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [InputContent]
  -> m ContractId
runScenario config address key contract inputs =
  do
    let
      unexpected response = throwError $ "Unexpected response: " <> show' response
      transact request =
        do
          (contractId, body) <-
            handle' config request
              $ \case
                Body{..} -> pure (resContractId, resTransactionBody)
                response -> unexpected response
          tx <-
            handle' config (Sign body [] [key])
              $ \case
                Tx{..}   -> pure resTransaction
                response -> unexpected response
          txId' <-
            handle' config (Submit tx)
              $ \case
                TxId{..} -> pure resTransactionId
                response -> unexpected response
          liftIO $ putStrLn $ "ContractId = " <> (init . tail . show) (txId $ unContractId contractId) <> "#" <> show (unTxIx . txIx $ unContractId contractId) <> " > TxId = " <> (init . tail . show) txId'
          waitForConfirmation config txId'
          pure contractId
    contractId <- transact $ Create contract mempty 1_500_000 mempty address mempty
    mapM_ (\input -> transact $ Apply contractId [NormalInput input] Nothing Nothing mempty address mempty) inputs
    pure contractId


show' :: A.ToJSON a => a -> String
show' = LBS8.unpack . A.encode


handle'
  :: MonadError String m
  => MonadIO m
  => Config
  -> MarloweRequest 'V1
  -> (MarloweResponse 'V1 -> m a)
  -> m a
handle' config request extract =
  liftIO (handle request config)
    >>= \case
      Right response -> extract response
      Left message   -> throwError message


waitForConfirmation
  :: MonadError String m
  => MonadIO m
  => Config
  -> TxId
  -> m ()
waitForConfirmation config txId =
  handle' config (Wait txId)
    $ \case
      TxId{} -> pure ()
      response -> throwError $ "Unexpected response: " <> show' response


currentTime :: MonadIO m => m POSIXTime
currentTime = POSIXTime . floor . (* 1000) . nominalDiffTimeToSeconds <$> liftIO P.getPOSIXTime


runOne
  :: MonadError String m
  => MonadIO m
  => Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> m ContractId
runOne config address key =
  do
    now <- currentTime
    party <-
      case deserialiseAddress $ unAddress address of
        Just address' -> pure $ uncurry Address address'
        Nothing       -> throwError "Address conversion failed."
    let
      contract = makeContract (now + 1 * 15 * 60 * 1000) party
    inputs <- randomInputs party
    runScenario config address key contract inputs


main :: IO ()
main =
   do
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
         (\(address, key) -> replicateM_ count $ print =<< runExceptT (runOne config address key))
         addressKeys
