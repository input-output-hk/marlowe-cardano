{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Oracle.Process
  where


import Control.Monad (void)
import Control.Monad.Except (ExceptT(ExceptT), liftIO, runExceptT)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Language.Marlowe.Core.V1.Semantics.Types (ChoiceId(ChoiceId), Input(NormalInput), InputContent(IChoice), Party)
import Language.Marlowe.Oracle.Detect (containsOracleAction, contractReadyForOracle)
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), TChanEOF, contractFromStep)
import Language.Marlowe.Runtime.App.Transact (applyWithEvents)
import Language.Marlowe.Runtime.App.Types (Config, PollingFrequency)
import Language.Marlowe.Runtime.ChainSync.Api (Address)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Network.Oracle (OracleEnv, readOracle, toOracleSymbol)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Explicit (EventBackend, addField, hoistEvent, hoistEventBackend, idInjectSelector, subEventBackend)
import Observe.Event.Syntax ((≔))
import Plutus.V2.Ledger.Api (toBuiltin)

import qualified Cardano.Api as C (PaymentExtendedKey, SigningKey)
import qualified Data.ByteString.Char8 as BS8 (pack)
import Language.Marlowe.Runtime.App.Channel (RequeueFrequency)
import qualified Language.Marlowe.Runtime.App.Channel as App (LastSeen(..), runContractAction, runDetection)


runDetection
  :: Party
  -> EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection party =
  App.runDetection
    $ maybe False (not . null . containsOracleAction party) . contractFromStep


runOracle
  :: OracleEnv
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Party
  -> EventBackend IO r DynamicEventSelector
  -> RequeueFrequency
  -> Bool
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runOracle oracleEnv config address key party eventBackend =
  App.runContractAction "OracleProcess" eventBackend
    $ \event App.LastSeen{..} ->
      do
        let
          -- Find the oracle symbols requested and the ones that can be serviced, respectively.
          rawSymbols = contractReadyForOracle party lastContract
          validSymbols = mapMaybe toOracleSymbol rawSymbols
          -- Build and submit a transaction to report the oracle's value.
          report contractId symbol =
            do
              let
                event' = hoistEvent liftIO event
                subBackend = hoistEventBackend liftIO $ subEventBackend idInjectSelector event eventBackend
              addField event' $ ("symbol" :: Text) ≔ show symbol
              value <- ExceptT $ readOracle eventBackend oracleEnv symbol
              addField event' $ ("value" :: Text) ≔ value
              void
                . applyWithEvents subBackend config address key contractId
                . pure . NormalInput
                $ IChoice (ChoiceId (toBuiltin . BS8.pack $ show symbol) party) value
        -- Print the context of the transaction.
        addField event $ ("previousTransactionId" :: Text) ≔ lastTxId
        addField event $ ("readyForOracle" :: Text) ≔ rawSymbols
        addField event $ ("availableForOracle" :: Text) ≔ fmap show validSymbols
        -- Execute the transaction, if there is a valid symbol.
        result <- runExceptT $ mapM_ (report thisContractId) (take 1 validSymbols)
          -- Print the result of the transaction.
        addField event
          . (("result" :: Text) ≔)
          $ case (result, null validSymbols) of
              (Right ()    , True ) -> "Ignored."
              (Right ()    , False) -> "Confirmed."
              (Left message, _    ) -> "Failed: " <> message
