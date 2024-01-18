{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

-- | This module defines a server for the /withdrawals REST API.
module Language.Marlowe.Runtime.Web.Server.REST.Withdrawals where

import Cardano.Api (IsCardanoEra, TxBody, getTxId, makeSignedTransaction)
import Cardano.Api.Shelley (ReferenceTxInsScriptsInlineDatumsSupportedInEra (..))
import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page (..), WithdrawalFilter (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Cardano.Feature (ShelleyFeature (..), withShelleyBasedEra)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..), WithdrawTx (..), WithdrawTxInEra (..))
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
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx (..), TempTxStatus (..))
import Language.Marlowe.Runtime.Web.Server.Util (makeSignedTxWithWitnessKeys)
import Servant
import Servant.Pagination

server :: ServerT WithdrawalsAPI ServerM
server =
  get
    :<|> post
    :<|> withdrawalServer

data TxBodyInAnyEra where
  TxBodyInAnyEra :: (IsCardanoEra era) => TxBody era -> TxBodyInAnyEra

post
  :: PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM PostWithdrawalsResponse
post PostWithdrawalsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <-
    Set.fromList
      <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  payouts' <- fromDTOThrow (badRequest' "Invalid payouts") payouts
  TxBodyInAnyEra txBody <-
    withdraw MarloweV1 WalletAddresses{..} payouts' >>= \case
      Left err -> throwDTOError err
      Right (WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra WithdrawTxInEra{txBody}) -> pure $ TxBodyInAnyEra txBody
      Right (WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra WithdrawTxInEra{txBody}) -> pure $ TxBodyInAnyEra txBody
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

put :: TxId -> TxWitness -> ServerM NoContent
put withdrawalId txWitness = do
  withdrawalId' <- fromDTOThrow (badRequest' "Invalid withdrawal id value") withdrawalId
  loadWithdrawal withdrawalId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just (Left (TempTx era _ Unsigned WithdrawTxInEra{txBody})) -> handleLoaded withdrawalId' era txBody
    Just _ ->
      throwError $
        ApiError.toServerError $
          ApiError "Withdrawal already submitted" "WithdrawalAlreadySubmitted" Null 409
  where
    handleLoaded :: Chain.TxId -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> TxBody era -> ServerM NoContent
    handleLoaded withdrawalId' era txBody = withShelleyBasedEra (shelleyBasedEraOfFeature era) do
      tx <- case era of
        ReferenceTxInsScriptsInlineDatumsInBabbageEra -> do
          txWitness' <- fromDTOThrow (badRequest' "Invalid tx witness") txWitness
          pure $ makeSignedTxWithWitnessKeys txBody txWitness'
        ReferenceTxInsScriptsInlineDatumsInConwayEra -> do
          txWitness' <- fromDTOThrow (badRequest' "Invalid tx witness") txWitness
          pure $ makeSignedTxWithWitnessKeys txBody txWitness'
      submitWithdrawal withdrawalId' era tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
