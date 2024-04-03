{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts/:contractId/transactions REST API.
module Language.Marlowe.Runtime.Web.Contract.Transaction.Server (server) where

import Cardano.Api (BabbageEra, BabbageEraOnwards (..), ConwayEra, TxBody, getTxId, makeSignedTransaction)
import qualified Cardano.Api as Cardano
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Data.Aeson (Value (Null))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (
  MarloweTransactionMetadata (MarloweTransactionMetadata),
  MarloweVersion (..),
  SomeMarloweVersion (..),
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Transaction.Api (InputsApplied (..), InputsAppliedInEra (..), WalletAddresses (..))
import Language.Marlowe.Runtime.Web.Adapter.CommaList (
  CommaList (unCommaList),
 )
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink (..))
import Language.Marlowe.Runtime.Web.Adapter.Pagination (
  PaginatedResponse,
 )
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.ApiError (
  ApiError (ApiError),
  badRequest',
  notFound',
  rangeNotSatisfiable',
  throwDTOError,
 )
import qualified Language.Marlowe.Runtime.Web.Adapter.Server.ApiError as ApiError
import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (
  FromDTO (fromDTO),
  ShelleyTxWitness (..),
  ToDTO (toDTO),
  fromDTOThrow,
  fromPaginationRange,
 )
import Language.Marlowe.Runtime.Web.Adapter.Server.Monad (
  ServerM,
  applyInputs,
  loadTransaction,
  loadTransactions,
  submitTransaction,
 )
import Language.Marlowe.Runtime.Web.Adapter.Server.SyncClient (LoadTxError (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.TxClient (TempTx (TempTx), TempTxStatus (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.Util (makeSignedTxWithWitnessKeys)
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (
  GetTransactionResponse,
  GetTransactionsAPI,
  GetTransactionsResponse,
  PostTransactionsAPI,
  PostTransactionsResponse,
  TransactionAPI,
  TransactionsAPI,
 )
import Language.Marlowe.Runtime.Web.Core.Address (Address)
import Language.Marlowe.Runtime.Web.Core.Tx (TextEnvelope (..), TxBodyInAnyEra (..), TxId, TxOutRef)

import Language.Marlowe.Runtime.Web.Tx.API (
  ApplyInputsTxEnvelope (ApplyInputsTxEnvelope),
  CardanoTx,
  CardanoTxBody,
  TxHeader,
 )
import Language.Marlowe.Runtime.Web.Withdrawal.API (
  PostTransactionsRequest (..),
 )
import Servant (
  HasServer (ServerT),
  NoContent (..),
  Proxy (Proxy),
  addHeader,
  throwError,
  type (:<|>) ((:<|>)),
 )
import Servant.Pagination (
  ExtractRange (extractRange),
  HasPagination (getDefaultRange),
  Range,
  Ranges,
  returnRange,
 )

server :: TxOutRef -> ServerT TransactionsAPI ServerM
server contractId =
  getTransactionsAPI contractId
    :<|> postTransactionsAPI
    :<|> transactionAPI contractId
  where
    getTransactionsAPI :: TxOutRef -> ServerT GetTransactionsAPI ServerM
    getTransactionsAPI = getTransactionsByContractId

    postTransactionsAPI :: ServerT PostTransactionsAPI ServerM
    postTransactionsAPI = buildCreateContractTxBody contractId :<|> buildCreateContractTx contractId

    transactionAPI :: TxOutRef -> TxId -> ServerT TransactionAPI ServerM
    transactionAPI contractId' txId = getTransaction contractId' txId :<|> submitCreateContractTx contractId txId

getTransactionsByContractId
  :: TxOutRef
  -> Maybe (Ranges '["transactionId"] GetTransactionsResponse)
  -> ServerM (PaginatedResponse '["transactionId"] GetTransactionsResponse)
getTransactionsByContractId contractId ranges = do
  let range :: Range "transactionId" TxId
      range = fromMaybe (getDefaultRange (Proxy @TxHeader)) $ extractRange =<< ranges
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  range' <- maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  loadTransactions contractId' range' >>= \case
    Left ContractNotFound -> throwError $ notFound' "Contract not found"
    Left TxNotFound -> throwError $ rangeNotSatisfiable' "Starting transaction not found"
    Right Page{..} -> do
      let headers' = toDTO items
      addHeader totalCount . fmap ListObject <$> returnRange range (IncludeLink (Proxy @"transaction") <$> headers')

buildCreateContractTxBody
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostTransactionsResponse CardanoTxBody)
buildCreateContractTxBody contractId req changeAddressDTO mAddresses mCollateralUtxos = do
  TxBodyInAnyEra txBody <- buildCreateContractTxBody' contractId req changeAddressDTO mAddresses mCollateralUtxos
  pure $
    IncludeLink (Proxy @"transaction") $
      ApplyInputsTxEnvelope
        contractId
        (toDTO $ fromCardanoTxId $ getTxId txBody)
        (toDTO txBody)

buildCreateContractTx
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostTransactionsResponse CardanoTx)
buildCreateContractTx contractId req changeAddressDTO mAddresses mCollateralUtxos = do
  TxBodyInAnyEra txBody <- buildCreateContractTxBody' contractId req changeAddressDTO mAddresses mCollateralUtxos
  pure $
    IncludeLink (Proxy @"transaction") $
      ApplyInputsTxEnvelope
        contractId
        (toDTO $ fromCardanoTxId $ getTxId txBody)
        (toDTO $ makeSignedTransaction [] txBody)

buildCreateContractTxBody'
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM TxBodyInAnyEra
buildCreateContractTxBody' contractId PostTransactionsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
  SomeMarloweVersion v@MarloweV1 <- fromDTOThrow (badRequest' "Invalid Marlowe version") version
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <-
    Set.fromList
      <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  transactionMetadata <- fromDTOThrow (badRequest' "Invalid metadata value") metadata
  marloweMetadata <-
    fromDTOThrow
      (badRequest' "Invalid tags value")
      if Map.null tags then Nothing else Just tags
  applyInputs v WalletAddresses{..} contractId' MarloweTransactionMetadata{..} invalidBefore invalidHereafter inputs >>= \case
    Left err -> throwDTOError err
    Right (InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{txBody}) -> pure $ TxBodyInAnyEra txBody
    Right (InputsApplied BabbageEraOnwardsConway InputsAppliedInEra{txBody}) -> pure $ TxBodyInAnyEra txBody

getTransaction :: TxOutRef -> TxId -> ServerM GetTransactionResponse
getTransaction contractId txId = do
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  txId' <- fromDTOThrow (badRequest' "Invalid transaction id value") txId
  loadTransaction contractId' txId' >>= \case
    Nothing -> throwError $ notFound' "Transaction not found"
    Just result -> do
      let contractState = either toDTO toDTO result
      pure $
        IncludeLink (Proxy @"previous") $
          IncludeLink (Proxy @"next") contractState

submitCreateContractTx :: TxOutRef -> TxId -> TextEnvelope -> ServerM NoContent
submitCreateContractTx contractId txId body = do
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  txId' <- fromDTOThrow (badRequest' "Invalid transaction id value") txId
  loadTransaction contractId' txId' >>= \case
    Nothing -> throwError $ notFound' "Transaction not found"
    Just (Left (TempTx era _ Unsigned InputsAppliedInEra{txBody})) -> handleLoaded contractId' txId' era txBody
    Just _ ->
      throwError $
        ApiError.toServerError $
          ApiError "Transaction already submitted" "ContractAlreadySubmitted" Null 409
  where
    handleLoaded
      :: Core.ContractId -> Chain.TxId -> BabbageEraOnwards era -> TxBody era -> ServerM NoContent
    handleLoaded contractId' txId' BabbageEraOnwardsBabbage txBody = do
      (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
        "Tx BabbageEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitTransaction contractId' txId' BabbageEraOnwardsBabbage tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    handleLoaded contractId' txId' BabbageEraOnwardsConway txBody = do
      (req :: Maybe (Either (Cardano.Tx ConwayEra) (ShelleyTxWitness ConwayEra))) <- case teType body of
        "Tx ConwayEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness ConwayEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx ConwayEra\", \"ShelleyTxWitness ConwayEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitTransaction contractId' txId' BabbageEraOnwardsConway tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
