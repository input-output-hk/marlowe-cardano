{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction where

import Cardano.Api (AlonzoEraOnwards, Tx)
import qualified Cardano.Api as C
import Colog (Message, WithLog)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Event.Class (MonadInjectEvent)
import Data.Aeson (KeyValue (..), encode, object)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient)
import Language.Marlowe.Runtime.ChainSync.Api (
  ChainSyncQuery,
  PlutusScript (..),
  RuntimeChainSeekClient,
  TokenName (..),
  TxOutRef (..),
  UTxOs (..),
  renderTxOutRef,
  toBech32,
  unInterpreter,
 )
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), renderContractId)
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts, ReferenceScriptUtxo (..))
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.BuildConstraints (MkRoleTokenMintingPolicy)
import Language.Marlowe.Runtime.Transaction.Chain
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext (..), PayoutContext (..), WalletContext (..))
import Language.Marlowe.Runtime.Transaction.Query (
  LoadMarloweContext,
  LoadPayoutContext,
  LoadWalletContext,
 )
import qualified Language.Marlowe.Runtime.Transaction.Query as Q
import Language.Marlowe.Runtime.Transaction.Query.Helper (LoadHelpersContext)
import qualified Language.Marlowe.Runtime.Transaction.Query.Helper as QH
import Language.Marlowe.Runtime.Transaction.Server
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob)
import Network.Protocol.Connection (Connector, ServerSource)
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Query.Client (QueryClient)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Trace.Core (SpanKind (Client, Internal), toAttribute)
import qualified OpenTelemetry.Trace.Core as OTel
import System.Exit (ExitCode (..))
import System.IO (hGetContents)
import UnliftIO (MonadUnliftIO, bracket, hFlush, liftIO)
import UnliftIO.Process (CreateProcess (..), StdStream (..), createProcess, terminateProcess, waitForProcess)
import qualified UnliftIO.Process as P

data TransactionDependencies m = TransactionDependencies
  { chainSyncConnector :: Connector RuntimeChainSeekClient m
  , mkSubmitJob :: forall era. AlonzoEraOnwards era -> Tx era -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadPayoutContext :: LoadPayoutContext m
  , loadMarloweContext :: LoadMarloweContext m
  , loadHelpersContext :: LoadHelpersContext m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , contractQueryConnector :: Connector (QueryClient ContractRequest) m
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  , analysisTimeout :: NominalDiffTime
  , mkRoleTokenMintingPolicy :: MkRoleTokenMintingPolicy m
  , marloweQueryConnector :: Connector MarloweQueryClient m
  }

data MarloweTx m = MarloweTx
  { probes :: Probes
  , serverSource :: ServerSource (JobServer MarloweTxCommand) m ()
  }

transaction
  :: (MonadUnliftIO m, MonadInjectEvent r TransactionServerSelector s m, WithLog env Message m)
  => Component m (TransactionDependencies m) (MarloweTx m)
transaction = proc TransactionDependencies{..} -> do
  (connected, getTip) <- transactionChainClient -< TransactionChainClientDependencies{..}
  serverSource <- transactionServer -< TransactionServerDependencies{..}
  returnA
    -<
      MarloweTx
        { serverSource
        , probes =
            Probes
              { startup = pure True
              , liveness = atomically connected
              , readiness = atomically connected
              }
        }

renderTransactionServerSelectorOTel :: RenderSelectorOTel TransactionServerSelector
renderTransactionServerSelectorOTel = \case
  Exec ->
    OTelRendered
      { eventName = "marlowe_tx/exec"
      , eventKind = OTel.Server
      , renderField = \case
          SystemStart (C.SystemStart start) -> [("cardano.system_start", fromString $ show start)]
          EraHistory (C.EraHistory interpreter) ->
            [("cardano.era_history", fromString $ show $ unInterpreter interpreter)]
          ProtocolParameters pp -> [("cardano.protocol_parameters", fromString $ show pp)]
          NetworkId networkId -> [("cardano.network_id", fromString $ show networkId)]
          Era era -> [("cardano.era", fromString $ show era)]
      }
  ExecCreate ->
    OTelRendered
      { eventName = "marlowe_tx/exec/create"
      , eventKind = OTel.Server
      , renderField = \case
          Constraints MarloweV1 constraints -> [("marlowe.tx.constraints", fromString $ show constraints)]
          ResultingTxBody txBody -> [("cardano.tx_body.babbage", fromString $ show txBody)]
      }
  ExecApplyInputs ->
    OTelRendered
      { eventName = "marlowe_tx/exec/apply_inputs"
      , eventKind = OTel.Server
      , renderField = \case
          Constraints MarloweV1 constraints -> [("marlowe.tx.constraints", fromString $ show constraints)]
          ResultingTxBody txBody -> [("cardano.tx_body.babbage", fromString $ show txBody)]
      }
  ExecWithdraw ->
    OTelRendered
      { eventName = "marlowe_tx/exec/withdraw"
      , eventKind = OTel.Server
      , renderField = \case
          Constraints MarloweV1 constraints -> [("marlowe.tx.constraints", fromString $ show constraints)]
          ResultingTxBody txBody -> [("cardano.tx_body.babbage", fromString $ show txBody)]
      }

renderLoadWalletContextSelectorOTel :: RenderSelectorOTel Q.LoadWalletContextSelector
renderLoadWalletContextSelectorOTel = \case
  Q.LoadWalletContext ->
    OTelRendered
      { eventName = "marlowe_tx/load_wallet_context"
      , eventKind = Client
      , renderField = \case
          Q.ForAddresses addresses -> [("marlowe.tx.wallet_addresses", toAttribute $ mapMaybe toBech32 $ Set.toList addresses)]
          Q.WalletContextLoaded WalletContext{..} ->
            catMaybes
              [ Just
                  ( "marlowe.tx.wallet_utxo"
                  , toAttribute $
                      fmap renderTxOutRef $
                        Map.keys $
                          unUTxOs availableUtxos
                  )
              , Just
                  ( "marlowe.tx.wallet_collateral_utxo"
                  , toAttribute $
                      fmap renderTxOutRef $
                        Set.toList collateralUtxos
                  )
              , ("marlowe.tx.wallet_change_address",) . toAttribute <$> toBech32 changeAddress
              ]
      }

renderLoadPayoutContextSelectorOTel :: RenderSelectorOTel Q.LoadPayoutContextSelector
renderLoadPayoutContextSelectorOTel = \case
  Q.LoadPayoutContext ->
    OTelRendered
      { eventName = "marlowe_tx/load_wallet_context"
      , eventKind = Client
      , renderField = \case
          Q.ForPayouts payouts -> [("marlowe.tx.payouts", toAttribute $ renderTxOutRef <$> Set.toList payouts)]
          Q.PayoutContextLoaded PayoutContext{..} ->
            catMaybes
              [ Just
                  ( "marlowe.contract_payout_utxo"
                  , toAttribute $
                      fmap renderTxOutRef $
                        Map.keys payoutOutputs
                  )
              , Just
                  ( "marlowe.payout_reference_script_outputs"
                  , toAttribute $ renderTxOutRef . txOutRef <$> Map.elems payoutScriptOutputs
                  )
              ]
      }

renderLoadMarloweContextSelectorOTel :: RenderSelectorOTel Q.LoadMarloweContextSelector
renderLoadMarloweContextSelectorOTel = \case
  Q.LoadMarloweContext ->
    OTelRendered
      { eventName = "marlowe_tx/load_marlowe_context"
      , eventKind = Internal
      , renderField = \case
          Q.DesiredVersion version -> [("marlowe.contract_version", fromString $ show version)]
          Q.Contract contractId -> [("marlowe.contract_version", toAttribute $ renderContractId contractId)]
      }
  Q.ExtractCreationFailed ->
    OTelRendered
      { eventName = "marlowe_tx/load_marlowe_context/extract_creation_failed"
      , eventKind = Internal
      , renderField = pure . ("marlowe.extract_creation_error",) . fromString . show
      }
  Q.ExtractMarloweTransactionFailed ->
    OTelRendered
      { eventName = "marlowe_tx/load_marlowe_context/extract_marlowe_transaction_failed"
      , eventKind = Internal
      , renderField = pure . ("marlowe.extract_marlowe_transaction_error",) . fromString . show
      }
  Q.ContractNotFound ->
    OTelRendered
      { eventName = "marlowe_tx/load_marlowe_context/contract_not_found"
      , eventKind = Internal
      , renderField = \case {}
      }
  Q.ContractFound ->
    OTelRendered
      { eventName = "marlowe_tx/load_marlowe_context/contract_found"
      , eventKind = Internal
      , renderField = \case
          Q.ActualVersion version -> [("marlowe.contract_version", fromString $ show version)]
          Q.MarloweScriptAddress address -> maybeToList $ ("marlowe.marlowe_script_address",) . toAttribute <$> toBech32 address
          Q.PayoutScriptHash hash -> [("marlowe.payout_script_hash", fromString $ show hash)]
      }
  Q.ContractTipFound MarloweV1 ->
    OTelRendered
      { eventName = "marlowe_tx/load_marlowe_context/contract_tip_found"
      , eventKind = Internal
      , renderField = \MarloweContext{..} ->
          catMaybes
            [ ("marlowe.contract_utxo",) . fromString . show <$> scriptOutput
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

renderLoadHelpersContextSelectorOTel :: RenderSelectorOTel QH.LoadHelpersContextSelector
renderLoadHelpersContextSelectorOTel = \case
  QH.LoadHelpersContext ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/load_helpers_context"
      , eventKind = Internal
      , renderField = \contractId -> [("marlowe.contract_id", fromString . show $ contractId)]
      }
  QH.ContractNotFound ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/contract_not_found"
      , eventKind = Internal
      , renderField = mempty
      }
  QH.ContractNotExtracted ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/contract_not_extracted"
      , eventKind = Internal
      , renderField = \e -> [("marlowe.extract_creation_error", fromString . show $ e)]
      }
  QH.ContractFound ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/contract_found"
      , eventKind = Internal
      , renderField = \contractId -> [("marlowe.contract_id", fromString . show $ contractId)]
      }
  QH.RollForwardToGenesis ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/roll_forward_to_genesis"
      , eventKind = Internal
      , renderField = mempty
      }
  QH.TxOutRefConsumed ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/tx_out_ref_consumed"
      , eventKind = Internal
      , renderField = \txOutRef -> [("marlowe.tx_out_ref", fromString . show $ txOutRef)]
      }
  QH.TxOutRefNotConsumed ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/tx_out_ref_not_consumed"
      , eventKind = Internal
      , renderField = \txOutRef -> [("marlowe.tx_out_ref", fromString . show $ txOutRef)]
      }
  QH.LoadHelpersContextTxOutRefNotFound ->
    OTelRendered
      { eventName = "marlowe_tx/load_helpers_context/load_helpers_context_tx_out_ref_not_found"
      , eventKind = Internal
      , renderField = \txOutRef -> [("marlowe.tx_out_ref", fromString . show $ txOutRef)]
      }

mkCommandLineRoleTokenMintingPolicy :: (MonadUnliftIO m, MonadFail m) => String -> MkRoleTokenMintingPolicy m
mkCommandLineRoleTokenMintingPolicy cmd TxOutRef{..} roleTokens = bracket
  (createProcess (P.proc cmd []){std_out = CreatePipe, std_err = CreatePipe, std_in = CreatePipe})
  (\(_, _, _, h) -> terminateProcess h)
  \handles -> do
    (Just stdIn, Just stdOut, Just stdErr, handle) <- pure handles
    liftIO $
      BS8.hPutStrLn stdIn $
        LBS.toStrict $
          encode $
            object
              [ "seedInputId" .= txId
              , "seedInputIx" .= txIx
              , "roleTokens" .= Map.mapKeys (encodeBase16 . unTokenName) roleTokens
              ]
    hFlush stdIn
    exitCode <- waitForProcess handle
    liftIO case exitCode of
      ExitSuccess -> PlutusScript <$> BS.hGetContents stdOut
      ExitFailure _ -> fail . ("mkRoleTokenMintingPolicy: " <>) =<< hGetContents stdErr
