{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  ( RootSelector(..)
  , renderRootSelectorOTel
  ) where

import qualified Cardano.Api as C
import Control.Monad.Event.Class (Inject(..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.String (fromString)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek, UTxOs(unUTxOs), renderTxOutRef, toBech32, unInterpreter)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), renderContractId)
import Language.Marlowe.Runtime.Core.ScriptRegistry (ReferenceScriptUtxo(..))
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext(..), WalletContext(..))
import qualified Language.Marlowe.Runtime.Transaction.Query as Q
import Language.Marlowe.Runtime.Transaction.Server
import Network.Protocol.Driver.Trace
  (TcpClientSelector, TcpServerSelector, renderTcpClientSelectorOTel, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace (SpanKind(Client, Internal), toAttribute)
import qualified OpenTelemetry.Trace as OTel

data RootSelector f where
  ChainSeekClient :: TcpClientSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  ChainSyncJobClient :: TcpClientSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  ChainSyncQueryClient :: TcpClientSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  Server :: TcpServerSelector (Handshake (Job MarloweTxCommand)) f -> RootSelector f
  App :: TransactionServerSelector f -> RootSelector f
  LoadWalletContext :: Q.LoadWalletContextSelector f -> RootSelector f
  LoadMarloweContext :: Q.LoadMarloweContextSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject Q.LoadWalletContextSelector RootSelector where
  inject = injectSelector LoadWalletContext

instance Inject Q.LoadMarloweContextSelector RootSelector where
  inject = injectSelector LoadMarloweContext

instance Inject TransactionServerSelector RootSelector where
  inject = injectSelector App

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  ChainSeekClient sel -> renderTcpClientSelectorOTel sel
  ChainSyncJobClient sel -> renderTcpClientSelectorOTel sel
  ChainSyncQueryClient sel -> renderTcpClientSelectorOTel sel
  Server sel -> renderTcpServerSelectorOTel sel
  App sel -> renderAppSelectorOTel sel
  LoadWalletContext sel -> renderLoadWalletContextSelectorOTel sel
  LoadMarloweContext sel -> renderLoadMarloweContextSelectorOTel sel

renderAppSelectorOTel :: RenderSelectorOTel TransactionServerSelector
renderAppSelectorOTel = \case
  Exec -> OTelRendered
    { eventName = "marlowe_tx/exec"
    , eventKind = OTel.Server
    , renderField = \case
        SystemStart (C.SystemStart start) -> [("cardano.system_start", fromString $ show start)]
        EraHistory (C.EraHistory C.CardanoMode interpreter) ->
          [("cardano.era_history", fromString $ show $ unInterpreter interpreter)]
        ProtocolParameters pp -> [("cardano.protocol_parameters", fromString $ show pp)]
        NetworkId networkId -> [("cardano.network_id", fromString $ show networkId)]
    }
  ExecCreate -> OTelRendered
    { eventName = "marlowe_tx/exec/create"
    , eventKind = OTel.Server
    , renderField = \case
        Constraints MarloweV1 constraints -> [("marlowe.tx.constraints", fromString $ show constraints)]
        ResultingTxBody txBody -> [("cardano.tx_body.babbage", fromString $ show txBody)]
    }
  ExecApplyInputs -> OTelRendered
    { eventName = "marlowe_tx/exec/apply_inputs"
    , eventKind = OTel.Server
    , renderField = \case
        Constraints MarloweV1 constraints -> [("marlowe.tx.constraints", fromString $ show constraints)]
        ResultingTxBody txBody -> [("cardano.tx_body.babbage", fromString $ show txBody)]
    }
  ExecWithdraw -> OTelRendered
    { eventName = "marlowe_tx/exec/withdraw"
    , eventKind = OTel.Server
    , renderField = \case
        Constraints MarloweV1 constraints -> [("marlowe.tx.constraints", fromString $ show constraints)]
        ResultingTxBody txBody -> [("cardano.tx_body.babbage", fromString $ show txBody)]
    }

renderLoadWalletContextSelectorOTel :: RenderSelectorOTel Q.LoadWalletContextSelector
renderLoadWalletContextSelectorOTel = \case
  Q.LoadWalletContext -> OTelRendered
    { eventName = "marlowe_tx/load_wallet_context"
    , eventKind = Client
    , renderField = \case
        Q.ForAddresses addresses -> [("marlowe.tx.wallet_addresses", toAttribute $ mapMaybe toBech32 $ Set.toList addresses)]
        Q.WalletContextLoaded WalletContext{..} -> catMaybes
          [ Just
              ( "marlowe.tx.wallet_utxo"
              , toAttribute
                  $ fmap renderTxOutRef
                  $ Map.keys
                  $ unUTxOs availableUtxos
              )
          , Just
              ( "marlowe.tx.wallet_collateral_utxo"
              , toAttribute
                  $ fmap renderTxOutRef
                  $ Set.toList collateralUtxos
              )
          , ("marlowe.tx.wallet_change_address",) . toAttribute <$> toBech32 changeAddress
          ]
    }

renderLoadMarloweContextSelectorOTel :: RenderSelectorOTel Q.LoadMarloweContextSelector
renderLoadMarloweContextSelectorOTel = \case
  Q.LoadMarloweContext -> OTelRendered
    { eventName = "marlowe_tx/load_marlowe_context"
    , eventKind = Internal
    , renderField = \case
        Q.DesiredVersion version -> [("marlowe.contract_version", fromString $ show version)]
        Q.Contract contractId -> [("marlowe.contract_version", toAttribute $ renderContractId contractId)]
    }
  Q.ExtractCreationFailed -> OTelRendered
    { eventName = "marlowe_tx/load_marlowe_context/extract_creation_failed"
    , eventKind = Internal
    , renderField = pure . ("marlowe.extract_creation_error",) . fromString . show
    }
  Q.ExtractMarloweTransactionFailed -> OTelRendered
    { eventName = "marlowe_tx/load_marlowe_context/extract_marlowe_transaction_failed"
    , eventKind = Internal
    , renderField = pure . ("marlowe.extract_marlowe_transaction_error",) . fromString . show
    }
  Q.ContractNotFound -> OTelRendered
    { eventName = "marlowe_tx/load_marlowe_context/contract_not_found"
    , eventKind = Internal
    , renderField = \case
    }
  Q.ContractFound -> OTelRendered
    { eventName = "marlowe_tx/load_marlowe_context/contract_found"
    , eventKind = Internal
    , renderField = \case
        Q.ActualVersion version -> [("marlowe.contract_version", fromString $ show version)]
        Q.MarloweScriptAddress address ->maybeToList $ ("marlowe.marlowe_script_address",) . toAttribute <$> toBech32 address
        Q.PayoutScriptHash hash -> [("marlowe.payout_script_hash", fromString $ show hash)]
    }
  Q.ContractTipFound MarloweV1 -> OTelRendered
    { eventName = "marlowe_tx/load_marlowe_context/contract_tip_found"
    , eventKind = Internal
    , renderField = \MarloweContext{..} -> catMaybes
        [ ("marlowe.contract_utxo",) . fromString . show <$> scriptOutput
        , Just
            ( "marlowe.contract_payout_utxo"
            , toAttribute
                $ fmap renderTxOutRef
                $ Map.keys payoutOutputs
            )
        , ("marlowe.marlowe_script_address",) . toAttribute <$> toBech32 marloweAddress
        , ("marlowe.payout_script_address",) . toAttribute <$> toBech32 payoutAddress
        , Just case marloweScriptUTxO of
            ReferenceScriptUtxo{..} -> ("marlowe.marlowe_reference_script_output", toAttribute $ renderTxOutRef txOutRef)
        , Just case payoutScriptUTxO of
            ReferenceScriptUtxo{..} -> ("marlowe.payout_reference_script_output", toAttribute $ renderTxOutRef txOutRef)
        , Just ("marlowe.marlowe_script_hash", fromString $ show marloweScriptHash)
        , Just ("marlowe.payout_script_hash", fromString $ show payoutScriptHash)
        ]
    }
