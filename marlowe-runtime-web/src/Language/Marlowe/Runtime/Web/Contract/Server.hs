{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts REST API.
module Language.Marlowe.Runtime.Web.Contract.Server (server) where

import Cardano.Api (BabbageEra, BabbageEraOnwards (..), ConwayEra, TxBody, makeSignedTransaction)
import qualified Cardano.Api as Cardano
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Data.Aeson (Value (Null))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import Language.Marlowe.Protocol.Query.Types (ContractFilter (..), Page (..))
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..), Lovelace (..))
import Language.Marlowe.Runtime.Core.Api as Core (
  ContractId,
  MarloweMetadataTag (..),
  MarloweTransactionMetadata (..),
  MarloweVersion (..),
  SomeMarloweVersion (..),
 )
import qualified Language.Marlowe.Runtime.Discovery.Api as Core
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated (..), ContractCreatedInEra (..), WalletAddresses (..))
import Language.Marlowe.Runtime.Web.Adapter.CommaList (
  CommaList (unCommaList),
 )
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink (..))
import Language.Marlowe.Runtime.Web.Adapter.Pagination (
  PaginatedResponse,
 )
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject (..))

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
  createContract,
  loadContract,
  loadContractHeaders,
  submitContract,
 )
import Language.Marlowe.Runtime.Web.Adapter.Server.TxClient (TempTx (TempTx), TempTxStatus (Unsigned))
import Language.Marlowe.Runtime.Web.Adapter.Server.Util (makeSignedTxWithWitnessKeys)
import Language.Marlowe.Runtime.Web.Contract.API as Web (
  ContractAPI,
  ContractHeader,
  ContractId,
  ContractOrSourceId (..),
  ContractsAPI,
  GetContractResponse,
  GetContractsResponse,
  PostContractsRequest (..),
  PostContractsResponse,
  unContractSourceId,
 )
import qualified Language.Marlowe.Runtime.Web.Contract.Next.Server as Next
import qualified Language.Marlowe.Runtime.Web.Contract.Source.Server as ContractSources
import qualified Language.Marlowe.Runtime.Web.Contract.Transaction.Server as Transactions
import Language.Marlowe.Runtime.Web.Core.Address (
  Address,
  StakeAddress,
 )
import Language.Marlowe.Runtime.Web.Core.Asset (
  AssetId,
  PolicyId,
 )
import Language.Marlowe.Runtime.Web.Core.Tx (
  TextEnvelope (..),
  TxBodyInAnyEra (..),
  TxOutRef,
 )

import Control.Monad.Except (MonadError, join)
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Runtime.Web.Adapter.Server.ApiError (
  ApiError (ApiError),
  badRequest',
  notFound',
  rangeNotSatisfiable',
  throwDTOError,
 )
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
  CardanoTxBody,
  CreateTxEnvelope (CreateTxEnvelope),
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
  HasPagination (..),
  Range,
  Ranges,
  returnRange,
 )

server :: ServerT ContractsAPI ServerM
server =
  getContractHeaders
    :<|> (\stakeAddress -> postCreateTxBodyResponse stakeAddress :<|> postCreateTxResponse stakeAddress)
    :<|> contractServer
    :<|> ContractSources.server

postCreateTxBody
  :: PostContractsRequest
  -> Maybe StakeAddress
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (Core.ContractId, TxBodyInAnyEra, [SafetyError])
postCreateTxBody PostContractsRequest{..} stakeAddressDTO changeAddressDTO mAddresses mCollateralUtxos = do
  SomeMarloweVersion v@MarloweV1 <- fromDTOThrow (badRequest' "Unsupported Marlowe version") version
  stakeAddress <- fromDTOThrow (badRequest' "Invalid stake address value") stakeAddressDTO
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <-
    Set.fromList
      <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  threadTokenName' <- fromDTOThrow (badRequest' "Invalid thread token name") threadTokenName
  roles' <- fromDTOThrow (badRequest' "Invalid roles value") roles
  accounts' <- fromDTOThrow (badRequest' "Invalid initial accounts") accounts
  transactionMetadata <- fromDTOThrow (badRequest' "Invalid metadata value") metadata
  marloweMetadata <-
    fromDTOThrow
      (badRequest' "Invalid tags value")
      if Map.null tags then Nothing else Just tags
  let ContractOrSourceId contract' = contract
  createContract
    stakeAddress
    v
    WalletAddresses{..}
    threadTokenName'
    roles'
    MarloweTransactionMetadata{..}
    (Lovelace <$> minUTxODeposit)
    accounts'
    (DatumHash . unContractSourceId <$> contract')
    >>= \case
      Left err -> throwDTOError err
      Right
        (ContractCreated BabbageEraOnwardsBabbage ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)
      Right
        (ContractCreated BabbageEraOnwardsConway ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)

postCreateTxBodyResponse
  :: Maybe StakeAddress
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostContractsResponse CardanoTxBody)
postCreateTxBodyResponse stakeAddressDTO req changeAddressDTO mAddresses mCollateralUtxos = do
  (contractId, TxBodyInAnyEra txBody, safetyErrors) <-
    postCreateTxBody req stakeAddressDTO changeAddressDTO mAddresses mCollateralUtxos
  let (contractId', txBody') = toDTO (contractId, txBody)
  let body = CreateTxEnvelope contractId' txBody' safetyErrors
  pure $ IncludeLink (Proxy @"contract") body

postCreateTxResponse
  :: Maybe StakeAddress
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (PostContractsResponse CardanoTx)
postCreateTxResponse stakeAddressDTO req changeAddressDTO mAddresses mCollateralUtxos = do
  (contractId, TxBodyInAnyEra txBody, safetyErrors) <-
    postCreateTxBody req stakeAddressDTO changeAddressDTO mAddresses mCollateralUtxos
  let tx = makeSignedTransaction [] txBody
  let (contractId', tx') = toDTO (contractId, tx)
  let body = CreateTxEnvelope contractId' tx' safetyErrors
  pure $ IncludeLink (Proxy @"contract") body

getContractHeaders
  :: [PolicyId]
  -> [Text]
  -> [Address]
  -> [AssetId]
  -> Maybe (Ranges '["contractId"] GetContractsResponse)
  -> ServerM (PaginatedResponse '["contractId"] GetContractsResponse)
getContractHeaders roleCurrencies tags partyAddresses partyRoles ranges = do
  join
    ( loadContractHeaders
        <$> toContractFilter roleCurrencies tags partyAddresses partyRoles
        <*> toCoreRange ranges
    )
    >>= whenNothingThrow (rangeNotSatisfiable' "Initial contract ID not found")
    >>= toWebContractHeaders (servantContractHeaderRange ranges)

toWebContractHeaders
  :: Range "contractId" Web.ContractId
  -> Query.Page Core.ContractId Core.ContractHeader
  -> ServerM (PaginatedResponse '["contractId"] GetContractsResponse)
toWebContractHeaders range Page{items, totalCount} = do
  let response = IncludeLink (Proxy @"transactions") . IncludeLink (Proxy @"contract") <$> toDTO items
  addHeader totalCount
    . fmap ListObject
    <$> returnRange range response

servantContractHeaderRange :: Maybe (Ranges '["contractId"] GetContractsResponse) -> Range "contractId" Web.ContractId
servantContractHeaderRange ranges = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges

toCoreRange :: Maybe (Ranges '["contractId"] GetContractsResponse) -> ServerM (Query.Range Core.ContractId)
toCoreRange ranges =
  whenNothingThrow
    (rangeNotSatisfiable' "Invalid range value")
    (fromPaginationRange . servantContractHeaderRange $ ranges)

toContractFilter :: [PolicyId] -> [Text] -> [Address] -> [AssetId] -> ServerM ContractFilter
toContractFilter roleCurrencies tags partyAddresses partyRoles = do
  ContractFilter (Set.fromList $ MarloweMetadataTag <$> tags)
    <$> whenNothingThrow (badRequest' "Invalid role currency") (Set.fromList <$> fromDTO roleCurrencies)
    <*> whenNothingThrow (badRequest' "Invalid role token") (Set.fromList <$> fromDTO partyRoles)
    <*> whenNothingThrow (badRequest' "Invalid address") (Set.fromList <$> fromDTO partyAddresses)

whenNothingThrow :: (MonadError e m) => e -> Maybe a -> m a
whenNothingThrow err = maybe (throwError err) pure

contractServer :: TxOutRef -> ServerT ContractAPI ServerM
contractServer contractId =
  getOne contractId
    :<|> put contractId
    :<|> Next.server contractId
    :<|> Transactions.server contractId

getOne :: TxOutRef -> ServerM GetContractResponse
getOne contractId = do
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  loadContract contractId' >>= \case
    Nothing -> throwError $ notFound' "Contract not found"
    Just result -> do
      let contractState = either toDTO toDTO result
      pure $ IncludeLink (Proxy @"transactions") contractState

put :: TxOutRef -> TextEnvelope -> ServerM NoContent
put contractId body = do
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId

  loadContract contractId' >>= \case
    Nothing -> throwError $ notFound' "Contract not found"
    Just (Left (TempTx era _ Unsigned ContractCreatedInEra{txBody})) -> handleLoaded contractId' era txBody
    Just _ ->
      throwError $
        ApiError.toServerError $
          ApiError "Contract already submitted" "ContractAlreadySubmitted" Null 409
  where
    handleLoaded
      :: Core.ContractId -> BabbageEraOnwards era -> TxBody era -> ServerM NoContent
    handleLoaded contractId' BabbageEraOnwardsBabbage txBody = do
      (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
        "Tx BabbageEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        -- It seems that wallets provide nearly empty `TxWitness` back. Here is a quote from `CIP-30` docs:
        -- > Only the portion of the witness set that were signed as a result of this call are returned to
        -- > encourage dApps to verify the contents returned by this endpoint while building the final transaction.
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> do pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitContract contractId' BabbageEraOnwardsBabbage tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    handleLoaded contractId' BabbageEraOnwardsConway txBody = do
      (req :: Maybe (Either (Cardano.Tx ConwayEra) (ShelleyTxWitness ConwayEra))) <- case teType body of
        "Tx ConwayEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness ConwayEra" -> pure $ Right <$> fromDTO body
        _ ->
          throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx ConwayEra\", \"ShelleyTxWitness ConwayEra\""

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> pure tx
        -- It seems that wallets provide nearly empty `TxWitness` back. Here is a quote from `CIP-30` docs:
        -- > Only the portion of the witness set that were signed as a result of this call are returned to
        -- > encourage dApps to verify the contents returned by this endpoint while building the final transaction.
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
      submitContract contractId' BabbageEraOnwardsConway tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
