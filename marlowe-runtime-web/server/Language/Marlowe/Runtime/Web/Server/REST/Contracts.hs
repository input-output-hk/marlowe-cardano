{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts REST API.
module Language.Marlowe.Runtime.Web.Server.REST.Contracts where

import Cardano.Api (TxBody, makeSignedTransaction)
import Cardano.Api.Shelley (ReferenceTxInsScriptsInlineDatumsSupportedInEra (..))
import Data.Aeson (Value (Null))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import Language.Marlowe.Protocol.Query.Types (ContractFilter (..), Page (..))
import Language.Marlowe.Runtime.Cardano.Feature (ShelleyFeature (..), withShelleyBasedEra)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..), Lovelace (..))
import Language.Marlowe.Runtime.Core.Api (
  ContractId,
  MarloweMetadataTag (..),
  MarloweTransactionMetadata (..),
  MarloweVersion (..),
  SomeMarloweVersion (..),
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated (..), ContractCreatedInEra (..), WalletAddresses (..))
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM,
  createContract,
  loadContract,
  loadContractHeaders,
  submitContract,
 )
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (
  ApiError (ApiError),
  badRequest',
  notFound',
  rangeNotSatisfiable',
  throwDTOError,
 )
import qualified Language.Marlowe.Runtime.Web.Server.REST.ApiError as ApiError
import qualified Language.Marlowe.Runtime.Web.Server.REST.ContractSources as ContractSources
import qualified Language.Marlowe.Runtime.Web.Server.REST.Contracts.Next as Next
import qualified Language.Marlowe.Runtime.Web.Server.REST.Transactions as Transactions
import Language.Marlowe.Runtime.Web.Server.REST.Withdrawals (TxBodyInAnyEra (..))
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx (TempTx), TempTxStatus (Unsigned))
import Language.Marlowe.Runtime.Web.Server.Util (makeSignedTxWithWitnessKeys)
import Servant
import Servant.Pagination

server :: ServerT ContractsAPI ServerM
server =
  get
    :<|> post
    :<|> contractServer
    :<|> ContractSources.server

postCreateTxBody
  :: PostContractsRequest
  -> Maybe StakeAddress
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (ContractId, TxBodyInAnyEra, [SafetyError])
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
      if Map.null tags then Nothing else Just (tags, Nothing)
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
        (ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)
      Right
        (ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)

post
  :: Maybe StakeAddress
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM PostContractsResponse
post stakeAddressDTO req changeAddressDTO mAddresses mCollateralUtxos = do
  (contractId, TxBodyInAnyEra txBody, safetyErrors) <-
    postCreateTxBody req stakeAddressDTO changeAddressDTO mAddresses mCollateralUtxos
  let tx = makeSignedTransaction [] txBody
  let (contractId', tx') = toDTO (contractId, tx)
  let body = CreateTxEnvelope contractId' tx' safetyErrors
  pure $ IncludeLink (Proxy @"contract") body

get
  :: [PolicyId]
  -> [Text]
  -> [Address]
  -> [AssetId]
  -> Maybe (Ranges '["contractId"] GetContractsResponse)
  -> ServerM (PaginatedResponse '["contractId"] GetContractsResponse)
get roleCurrencies' tags' partyAddresses' partyRoles' ranges = do
  let range :: Range "contractId" TxOutRef
      range = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
  range' <- maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  roleCurrencies <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid role currency") roleCurrencies'
  let tags = Set.fromList $ MarloweMetadataTag <$> tags'
  partyAddresses <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid address") partyAddresses'
  partyRoles <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid role token") partyRoles'
  loadContractHeaders ContractFilter{..} range' >>= \case
    Nothing -> throwError $ rangeNotSatisfiable' "Initial contract ID not found"
    Just Page{..} -> do
      let headers' = toDTO items
      let response = IncludeLink (Proxy @"transactions") . IncludeLink (Proxy @"contract") <$> headers'
      addHeader totalCount . fmap ListObject <$> returnRange range response

toContractHeader :: ContractState -> ContractHeader
toContractHeader ContractState{..} = ContractHeader{..}

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

put :: TxOutRef -> TxWitness -> ServerM NoContent
put contractId txWitness = do
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
      :: Core.ContractId -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> TxBody era -> ServerM NoContent
    handleLoaded contractId' era txBody = withShelleyBasedEra (shelleyBasedEraOfFeature era) do
      tx <- case era of
        ReferenceTxInsScriptsInlineDatumsInBabbageEra -> do
          txWitness' <- fromDTOThrow (badRequest' "Invalid tx witness") txWitness
          pure $ makeSignedTxWithWitnessKeys txBody txWitness'
        ReferenceTxInsScriptsInlineDatumsInConwayEra -> do
          txWitness' <- fromDTOThrow (badRequest' "Invalid tx witness") txWitness
          pure $ makeSignedTxWithWitnessKeys txBody txWitness'
      submitContract contractId' era tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
