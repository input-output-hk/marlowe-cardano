{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Role.Server (
  server,
) where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Set as Set
import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.ApiError (
  ApiError (..),
  badRequest',
  constraintErrorToApiError,
  throwDTOError,
 )
import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (
  FromDTO (..),
  HasDTO (..),
  ShelleyTxWitness (..),
  ToDTO (..),
  fromDTOThrow,
 )
import Language.Marlowe.Runtime.Web.Core.Address (Address)

import Language.Marlowe.Runtime.Web.Core.Tx (
  TextEnvelope (..),
  TxId,
  TxOutRef,
 )
import Language.Marlowe.Runtime.Web.Role.API (BurnRoleTokensTxEnvelope (..), RoleAPI)
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
  CardanoTxBody,
 )
import Servant (
  NoContent (..),
  type (:<|>) ((:<|>)),
 )

import Cardano.Api (BabbageEra, BabbageEraOnwards (..), ConwayEra, TxBody, getTxId, makeSignedTransaction)
import Data.Aeson (ToJSON (toJSON), Value (Null))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Transaction.Api (
  BurnRoleTokensError (..),
  BurnRoleTokensTx (BurnRoleTokensTx),
  BurnRoleTokensTxInEra (..),
  WalletAddresses (..),
 )
import Language.Marlowe.Runtime.Web.Adapter.Server.Monad (
  ServerM,
  burnRoleTokens,
  loadTmpBurnRoleTokensTx,
  submitBurnRoleTokensTx,
 )
import Language.Marlowe.Runtime.Web.Role.TokenFilter (RoleTokenFilter)
import Servant.Server (HasServer (ServerT))

import qualified Cardano.Api as Cardano
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Web.Adapter.Server.ApiError as ApiError
import Language.Marlowe.Runtime.Web.Adapter.Server.TxClient (TempTx (..), TempTxStatus (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.Util (makeSignedTxWithWitnessKeys)

server :: ServerT Language.Marlowe.Runtime.Web.Role.API.RoleAPI ServerM
server = (buildTxBody :<|> buildTx) :<|> submitTx

buildTx
  :: RoleTokenFilter
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (BurnRoleTokensTxEnvelope CardanoTx)
buildTx roleTokenFilterDTO changeAddressDTO usedAddressesDTO collateralsDTO = do
  walletAddresses <- toWalletAddress (changeAddressDTO, usedAddressesDTO, collateralsDTO)
  roleTokenFilter <- fromDTOThrow (badRequest' "Invalid Role Token Filter") roleTokenFilterDTO
  burnRoleTokens MarloweV1 walletAddresses roleTokenFilter >>= \case
    Left err -> throwDTOError err
    Right (BurnRoleTokensTx BabbageEraOnwardsBabbage BurnRoleTokensTxInEra{txBody}) -> do
      let (txId, txBody') = toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
      let body = Language.Marlowe.Runtime.Web.Role.API.BurnRoleTokensTxEnvelope txId txBody'
      pure body
    Right (BurnRoleTokensTx BabbageEraOnwardsConway BurnRoleTokensTxInEra{txBody}) -> do
      let (txId, txBody') = toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
      let body = Language.Marlowe.Runtime.Web.Role.API.BurnRoleTokensTxEnvelope txId txBody'
      pure body

buildTxBody
  :: RoleTokenFilter
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (BurnRoleTokensTxEnvelope CardanoTxBody)
buildTxBody roleTokenFilterDTO changeAddressDTO usedAddressesDTO collateralsDTO = do
  walletAddresses <- toWalletAddress (changeAddressDTO, usedAddressesDTO, collateralsDTO)
  roleTokenFilter <- fromDTOThrow (badRequest' "Invalid Role Token Filter") roleTokenFilterDTO
  burnRoleTokens MarloweV1 walletAddresses roleTokenFilter >>= \case
    Left err -> throwDTOError err
    Right (BurnRoleTokensTx BabbageEraOnwardsBabbage BurnRoleTokensTxInEra{txBody}) -> do
      let (txId, txBody') = toDTO (fromCardanoTxId $ getTxId txBody, txBody)
      let body = BurnRoleTokensTxEnvelope txId txBody'
      pure body
    Right (BurnRoleTokensTx BabbageEraOnwardsConway BurnRoleTokensTxInEra{txBody}) -> do
      let (txId, txBody') = toDTO (fromCardanoTxId $ getTxId txBody, txBody)
      let body = BurnRoleTokensTxEnvelope txId txBody'
      pure body

submitTx :: TxId -> TextEnvelope -> ServerM NoContent
submitTx txIdTDO body = do
  txId <- fromDTOThrow (badRequest' "Invalid transaction id value") txIdTDO
  loadTmpBurnRoleTokensTx txId >>= \case
    Nothing -> throwError $ badRequest' "Transaction not found"
    Just ((TempTx _ _ Submitted _)) ->
      throwError $ ApiError.toServerError $ ApiError "Tx already submitted" "TxAlreadySubmitted" Null 409
    Just ((TempTx era _ Unsigned BurnRoleTokensTxInEra{txBody})) -> submitTx' txId era txBody
  where
    submitTx' :: Chain.TxId -> BabbageEraOnwards era -> TxBody era -> ServerM NoContent
    submitTx' txId' BabbageEraOnwardsBabbage txBody = do
      (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
        "Tx BabbageEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitBurnRoleTokensTx txId' BabbageEraOnwardsBabbage tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    submitTx' txId' BabbageEraOnwardsConway txBody = do
      (req :: Maybe (Either (Cardano.Tx ConwayEra) (ShelleyTxWitness ConwayEra))) <- case teType body of
        "Tx ConwayEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness ConwayEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx ConwayEra\", \"ShelleyTxWitness ConwayEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitBurnRoleTokensTx txId' BabbageEraOnwardsConway tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403

type WalletAddressesDTO = (Address, Maybe (CommaList Address), Maybe (CommaList TxOutRef))

toWalletAddress :: WalletAddressesDTO -> ServerM WalletAddresses
toWalletAddress (changeAddressDTO, usedAddressesDTO, collateralsDTO) = do
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList usedAddressesDTO)
  collateralUtxos <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList collateralsDTO)
  pure WalletAddresses{..}

instance HasDTO BurnRoleTokensError where
  type DTO BurnRoleTokensError = ApiError

instance ToDTO BurnRoleTokensError where
  toDTO = \case
    BurnEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "BurnEraUnsupported" Null 503
    BurnRolesActive roles -> ApiError "Active roles detected, refusing to burn" "BurnRolesActive" (toJSON roles) 400
    BurnInvalidPolicyId policyIds -> ApiError "Invalid policies" "BurnInvalidPolicyId" (toJSON policyIds) 400
    BurnNoTokens -> ApiError "No tokens to burn" "BurnNoTokensToBurn" Null 400
    BurnFromCardanoError -> ApiError "Internal error" "BurnFromCardanoError" Null 400
    BurnConstraintError err -> constraintErrorToApiError err
