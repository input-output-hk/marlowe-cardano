{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts/:contractId/transactions REST API.
module Language.Marlowe.Runtime.Web.Server.REST.Transactions where

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
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM,
  applyInputs,
  loadTransaction,
  loadTransactions,
  submitTransaction,
 )
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (
  ApiError (ApiError),
  badRequest',
  notFound',
  rangeNotSatisfiable',
  throwDTOError,
 )
import qualified Language.Marlowe.Runtime.Web.Server.REST.ApiError as ApiError
import Language.Marlowe.Runtime.Web.Server.REST.Withdrawals (TxBodyInAnyEra (..))
import Language.Marlowe.Runtime.Web.Server.SyncClient (LoadTxError (..))
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx (TempTx), TempTxStatus (..))
import Language.Marlowe.Runtime.Web.Server.Util (makeSignedTxWithWitnessKeys)
import Servant
import Servant.Pagination

server :: TxOutRef -> ServerT TransactionsAPI ServerM
server contractId =
  get contractId
    :<|> (postCreateTxBodyResponse contractId :<|> postCreateTxResponse contractId)
    :<|> transactionServer contractId

get
  :: TxOutRef
  -> Maybe (Ranges '["transactionId"] GetTransactionsResponse)
  -> ServerM (PaginatedResponse '["transactionId"] GetTransactionsResponse)
get contractId ranges = do
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

postCreateTxBody
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM TxBodyInAnyEra
postCreateTxBody contractId PostTransactionsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
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
      if Map.null tags then Nothing else Just (tags, Nothing)
  applyInputs v WalletAddresses{..} contractId' MarloweTransactionMetadata{..} invalidBefore invalidHereafter inputs >>= \case
    Left err -> throwDTOError err
    Right (InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{txBody}) -> pure $ TxBodyInAnyEra txBody
    Right (InputsApplied BabbageEraOnwardsConway InputsAppliedInEra{txBody}) -> pure $ TxBodyInAnyEra txBody

postCreateTxBodyResponse
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostTransactionsResponse CardanoTxBody)
postCreateTxBodyResponse contractId req changeAddressDTO mAddresses mCollateralUtxos = do
  TxBodyInAnyEra txBody <- postCreateTxBody contractId req changeAddressDTO mAddresses mCollateralUtxos
  let txBody' = toDTO txBody
  let txId = toDTO $ fromCardanoTxId $ getTxId txBody
  let body = ApplyInputsTxEnvelope contractId txId txBody'
  pure $ IncludeLink (Proxy @"transaction") body

postCreateTxResponse
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostTransactionsResponse CardanoTx)
postCreateTxResponse contractId req changeAddressDTO mAddresses mCollateralUtxos = do
  TxBodyInAnyEra txBody <- postCreateTxBody contractId req changeAddressDTO mAddresses mCollateralUtxos
  let txId = toDTO $ fromCardanoTxId $ getTxId txBody
  let tx = makeSignedTransaction [] txBody
  let tx' = toDTO tx
  let body = ApplyInputsTxEnvelope contractId txId tx'
  pure $ IncludeLink (Proxy @"transaction") body

transactionServer :: TxOutRef -> TxId -> ServerT TransactionAPI ServerM
transactionServer contractId txId =
  getOne contractId txId
    :<|> put contractId txId

getOne :: TxOutRef -> TxId -> ServerM GetTransactionResponse
getOne contractId txId = do
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  txId' <- fromDTOThrow (badRequest' "Invalid transaction id value") txId
  loadTransaction contractId' txId' >>= \case
    Nothing -> throwError $ notFound' "Transaction not found"
    Just result -> do
      let contractState = either toDTO toDTO result
      pure $
        IncludeLink (Proxy @"previous") $
          IncludeLink (Proxy @"next") contractState

put :: TxOutRef -> TxId -> TextEnvelope -> ServerM NoContent
put contractId txId body = do
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
