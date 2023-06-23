{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module defines a server for the /withdrawals REST API.
module Language.Marlowe.Runtime.Web.Server.REST.Withdrawals where

import Cardano.Api (BabbageEra, TxBody, getTxId, makeSignedTransaction)
import qualified Cardano.Api as Cardano
import Cardano.Ledger.Alonzo.TxWitness
import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page (..), WithdrawalFilter (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, loadWithdrawal, loadWithdrawals, submitWithdrawal, withdraw)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (
  ApiError (ApiError),
  badRequest',
  notFound',
  rangeNotSatisfiable',
  throwDTOError,
 )
import qualified Language.Marlowe.Runtime.Web.Server.REST.ApiError as ApiError
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx (..), TempTxStatus (..), Withdrawn (..))
import Language.Marlowe.Runtime.Web.Server.Util
import Servant
import Servant.Pagination

server :: ServerT WithdrawalsAPI ServerM
server =
  get
    :<|> (postCreateTxBodyResponse :<|> postCreateTxResponse)
    :<|> withdrawalServer

postCreateTxBody
  :: PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (TxBody BabbageEra)
postCreateTxBody PostWithdrawalsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <-
    Set.fromList
      <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  role' <- fromDTOThrow (badRequest' "Invalid role") role
  contractId' <- fromDTOThrow (badRequest' "Invalid contract ID") contractId
  withdraw MarloweV1 WalletAddresses{..} contractId' role' >>= \case
    Left err -> throwDTOError err
    Right txBody -> pure txBody

postCreateTxBodyResponse
  :: PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostWithdrawalsResponse CardanoTxBody)
postCreateTxBodyResponse req changeAddressDTO mAddresses mCollateralUtxos = do
  txBody <- postCreateTxBody req changeAddressDTO mAddresses mCollateralUtxos
  let (withdrawalId, txBody') = toDTO (fromCardanoTxId $ getTxId txBody, txBody)
  let body = WithdrawTxEnvelope withdrawalId txBody'
  pure $ IncludeLink (Proxy @"withdrawal") body

postCreateTxResponse
  :: PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostWithdrawalsResponse CardanoTx)
postCreateTxResponse req changeAddressDTO mAddresses mCollateralUtxos = do
  txBody <- postCreateTxBody req changeAddressDTO mAddresses mCollateralUtxos
  let tx = makeSignedTransaction [] txBody
  let (withdrawalId, tx') = toDTO (fromCardanoTxId $ getTxId txBody, tx)
  let body = WithdrawTxEnvelope withdrawalId tx'
  pure $ IncludeLink (Proxy @"withdrawal") body

get
  :: [PolicyId]
  -> Maybe (Ranges '["withdrawalId"] GetWithdrawalsResponse)
  -> ServerM (PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse)
get roleCurrencies ranges = do
  let range :: Range "withdrawalId" TxId
      range = fromMaybe (getDefaultRange (Proxy @WithdrawalHeader)) $ extractRange =<< ranges
  range' <- maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  roleCurrencies' <-
    traverse
      (\role -> maybe (throwError $ badRequest' $ "Invalid role value " <> show role) pure $ fromDTO role)
      roleCurrencies
  let wFilter = WithdrawalFilter $ Set.fromList roleCurrencies'
  loadWithdrawals wFilter range' >>= \case
    Nothing -> throwError $ rangeNotSatisfiable' "Initial withdrawal ID not found"
    Just Page{..} -> do
      let headers' = toWithdrawalHeader <$> toDTO items
      let response = IncludeLink (Proxy @"withdrawal") <$> headers'
      addHeader totalCount . fmap ListObject <$> returnRange range response

toWithdrawalHeader :: Withdrawal -> WithdrawalHeader
toWithdrawalHeader Withdrawal{..} = WithdrawalHeader{..}

withdrawalServer :: TxId -> ServerT WithdrawalAPI ServerM
withdrawalServer withdrawalId = getOne withdrawalId :<|> put withdrawalId

getOne :: TxId -> ServerM Withdrawal
getOne withdrawalId = do
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") withdrawalId
  loadWithdrawal contractId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just result -> pure $ either toDTO toDTO result

put :: TxId -> TextEnvelope -> ServerM NoContent
put contractId body = do
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  loadWithdrawal contractId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just (Left (TempTx _ Unsigned (Withdrawn txBody))) -> do
      (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
        "Tx BabbageEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        Just (Right (ShelleyTxWitness (TxWitness wtKeys _ _ _ _))) -> do
          case makeSignedTxWithWitnessKeys txBody wtKeys of
            Just tx -> pure tx
            Nothing -> throwError $ badRequest' "Invalid witness keys"
      submitWithdrawal contractId' tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    Just _ ->
      throwError $
        ApiError.toServerError $
          ApiError "Withdrawal already submitted" "WithdrawalAlreadySubmitted" Null 409
