{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts REST API.
module Language.Marlowe.Runtime.Web.Server.REST.Contracts where

import Cardano.Api (BabbageEra, ConwayEra, TxBody, makeSignedTransaction)
import qualified Cardano.Api as Cardano
import Cardano.Api.Shelley (ReferenceTxInsScriptsInlineDatumsSupportedInEra (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Data.Aeson (Value (Null))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import Language.Marlowe.Protocol.Query.Types (ContractFilter (..), Page (..))
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
    :<|> (\stakeAddress -> postCreateTxBodyResponse stakeAddress :<|> postCreateTxResponse stakeAddress)
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
    state
    (DatumHash . unContractSourceId <$> contract')
    >>= \case
      Left err -> throwDTOError err
      Right
        (ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)
      Right
        (ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)

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
      :: Core.ContractId -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> TxBody era -> ServerM NoContent
    handleLoaded contractId' ReferenceTxInsScriptsInlineDatumsInBabbageEra txBody = do
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
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> do
          case makeSignedTxWithWitnessKeys txBody wtKeys of
            Just tx -> pure tx
            Nothing -> throwError $ badRequest' "Invalid witness keys"
      submitContract contractId' ReferenceTxInsScriptsInlineDatumsInBabbageEra tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    handleLoaded contractId' ReferenceTxInsScriptsInlineDatumsInConwayEra txBody = do
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
        Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> do
          case makeSignedTxWithWitnessKeys txBody wtKeys of
            Just tx -> pure tx
            Nothing -> throwError $ badRequest' "Invalid witness keys"
      submitContract contractId' ReferenceTxInsScriptsInlineDatumsInConwayEra tx >>= \case
        Nothing -> pure NoContent
        Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
