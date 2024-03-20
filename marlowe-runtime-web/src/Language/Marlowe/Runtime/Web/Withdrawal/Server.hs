{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module defines a server for the /withdrawals REST API.
module Language.Marlowe.Runtime.Web.Withdrawal.Server where

import Cardano.Api (
  BabbageEra,
  BabbageEraOnwards (..),
  ConwayEra,
  IsShelleyBasedEra,
  TxBody,
  getTxId,
  makeSignedTransaction,
 )
import qualified Cardano.Api as Cardano
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page (..), WithdrawalFilter (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..), WithdrawTx (..), WithdrawTxInEra (..))
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
  loadWithdrawal,
  loadWithdrawals,
  submitWithdrawal,
  withdraw,
 )
import Language.Marlowe.Runtime.Web.Adapter.Server.TxClient (TempTx (..), TempTxStatus (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.Util (
  makeSignedTxWithWitnessKeys,
 )

import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList (..))
import Language.Marlowe.Runtime.Web.Core.Address (Address)
import Language.Marlowe.Runtime.Web.Core.Asset (PolicyId)

import Language.Marlowe.Runtime.Web.Core.Tx (TextEnvelope (..), TxId, TxOutRef)
import Language.Marlowe.Runtime.Web.Tx.API (CardanoTx, CardanoTxBody, WithdrawTxEnvelope (..))
import Language.Marlowe.Runtime.Web.Withdrawal.API (
  GetWithdrawalsResponse,
  PostWithdrawalsRequest (..),
  PostWithdrawalsResponse,
  Withdrawal (..),
  WithdrawalAPI,
  WithdrawalHeader (..),
  WithdrawalsAPI,
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

server :: ServerT WithdrawalsAPI ServerM
server =
  get
    :<|> (postCreateTxBodyResponse :<|> postCreateTxResponse)
    :<|> withdrawalServer

data TxBodyInAnyEra where
  TxBodyInAnyEra :: (IsShelleyBasedEra era) => TxBody era -> TxBodyInAnyEra

postCreateTxBody
  :: PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM TxBodyInAnyEra
postCreateTxBody PostWithdrawalsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <-
    Set.fromList
      <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  payouts' <- fromDTOThrow (badRequest' "Invalid payouts") payouts
  withdraw MarloweV1 WalletAddresses{..} payouts' >>= \case
    Left err -> throwDTOError err
    Right (WithdrawTx BabbageEraOnwardsBabbage WithdrawTxInEra{txBody}) -> pure $ TxBodyInAnyEra txBody
    Right (WithdrawTx BabbageEraOnwardsConway WithdrawTxInEra{txBody}) -> pure $ TxBodyInAnyEra txBody

postCreateTxBodyResponse
  :: PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostWithdrawalsResponse CardanoTxBody)
postCreateTxBodyResponse req changeAddressDTO mAddresses mCollateralUtxos = do
  TxBodyInAnyEra txBody <- postCreateTxBody req changeAddressDTO mAddresses mCollateralUtxos
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
  TxBodyInAnyEra txBody <- postCreateTxBody req changeAddressDTO mAddresses mCollateralUtxos
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
  withdrawalId' <- fromDTOThrow (badRequest' "Invalid withdrawal id value") withdrawalId
  loadWithdrawal withdrawalId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just result -> pure $ either toDTO toDTO result

put :: TxId -> TextEnvelope -> ServerM NoContent
put withdrawalId body = do
  withdrawalId' <- fromDTOThrow (badRequest' "Invalid withdrawal id value") withdrawalId
  loadWithdrawal withdrawalId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just (Left (TempTx era _ Unsigned WithdrawTxInEra{txBody})) -> handleLoaded withdrawalId' era txBody
    Just _ ->
      throwError $
        ApiError.toServerError $
          ApiError "Withdrawal already submitted" "WithdrawalAlreadySubmitted" Null 409
  where
    handleLoaded :: Chain.TxId -> BabbageEraOnwards era -> TxBody era -> ServerM NoContent
    handleLoaded withdrawalId' BabbageEraOnwardsBabbage txBody = do
      (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
        "Tx BabbageEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitWithdrawal withdrawalId' BabbageEraOnwardsBabbage tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    handleLoaded withdrawalId' BabbageEraOnwardsConway txBody = do
      (req :: Maybe (Either (Cardano.Tx ConwayEra) (ShelleyTxWitness ConwayEra))) <- case teType body of
        "Tx ConwayEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness ConwayEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx ConwayEra\", \"ShelleyTxWitness ConwayEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitWithdrawal withdrawalId' BabbageEraOnwardsConway tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
