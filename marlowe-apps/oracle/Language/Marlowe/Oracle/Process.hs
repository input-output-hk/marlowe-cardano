{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Oracle.Process where

import Control.Monad (void)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Language.Marlowe.Core.V1.Semantics.Types (
  ChoiceId (ChoiceId),
  Input (NormalInput),
  InputContent (IChoice),
  Party,
 )
import Language.Marlowe.Oracle.Detect (containsOracleAction, contractReadyForOracle)
import Language.Marlowe.Oracle.Types (choiceName, choiceName')
import Language.Marlowe.Runtime.App.Channel (RequeueFrequency)
import Language.Marlowe.Runtime.App.Stream (ContractStream (..), TChanEOF, contractFromStep)
import Language.Marlowe.Runtime.App.Transact (applyWithEvents)
import Language.Marlowe.Runtime.App.Types (
  Config,
  FinishOnClose (FinishOnClose),
  FinishOnWait (FinishOnWait),
  PollingFrequency,
 )
import Language.Marlowe.Runtime.ChainSync.Api (Address)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag (V1))
import Network.Oracle (OracleEnv, readOracle, toOracleSymbol)
import Observe.Event.Dynamic (DynamicEventSelector (..))
import Observe.Event.Explicit (EventBackend, addField, hoistEvent, hoistEventBackend, idInjectSelector, subEventBackend)
import Observe.Event.Syntax ((≔))

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Language.Marlowe.Runtime.App.Channel as App (LastSeen (..), runContractAction, runDetection)

runDetection
  :: Party
  -> EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection party eventBackend config pollingFrequency =
  App.runDetection
    (maybe False (not . null . containsOracleAction party) . contractFromStep)
    eventBackend
    config
    pollingFrequency
    (FinishOnClose True)
    (FinishOnWait True)

runOracle
  :: Either String OracleEnv
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Party
  -> EventBackend IO r DynamicEventSelector
  -> RequeueFrequency
  -> FinishOnWait
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runOracle oracleEnv config address key party eventBackend =
  App.runContractAction "OracleProcess" eventBackend $
    \event App.LastSeen{..} ->
      do
        let -- Find the oracle symbols requested and the ones that can be serviced, respectively.
            rawRequests = contractReadyForOracle party lastContract
            validRequests =
              case oracleEnv of
                Right _ -> mapMaybe (\oracleRequest -> fmap (const oracleRequest) . toOracleSymbol $ choiceName' oracleRequest) rawRequests
                Left _ -> rawRequests
            -- Build and submit a transaction to report the oracle's value.
            report contractId request =
              do
                let symbol = choiceName request
                    event' = hoistEvent liftIO event
                    subBackend = hoistEventBackend liftIO $ subEventBackend idInjectSelector event eventBackend
                addField event' $ ("request" :: Text) ≔ request
                value <- ExceptT $ readOracle eventBackend oracleEnv request
                addField event' $ ("value" :: Text) ≔ value
                void
                  . applyWithEvents subBackend config address key contractId
                  . pure
                  . NormalInput
                  $ IChoice (ChoiceId symbol party) value
        -- Print the context of the transaction.
        addField event $ ("lastContract" :: Text) ≔ lastContract
        addField event $ ("previousTransactionId" :: Text) ≔ lastTxId
        addField event $ ("readyForOracle" :: Text) ≔ rawRequests
        addField event $ ("availableForOracle" :: Text) ≔ validRequests
        -- Execute the transaction, if there is a valid symbol.
        result <- runExceptT $ mapM_ (report thisContractId) (take 1 validRequests)
        -- Print the result of the transaction.
        addField event
          . (("result" :: Text) ≔)
          $ case (result, null validRequests) of
            (Right (), True) -> "Ignored."
            (Right (), False) -> "Confirmed."
            (Left message, _) -> "Failed: " <> message
