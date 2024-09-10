{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Contract.API (
  ContractId,
  ContractHeader (..),
  ContractState (..),
  ContractsAPI,
  ContractAPI,
  GetContractAPI,
  GetContractResponse,
  ContractSourcesAPI,
  ContractSourceAPI,
  ContractSourceId (..),
  GetContractsAPI,
  GetContractsResponse,
  PostContractsRequest (..),
  PostContractsResponse,
  PostContractSourceResponse (..),
  ContractOrSourceId (..),
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  withText,
 )
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Object.Types (Label, ObjectBundle)
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedGet)
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject, OperationId, RenameResponseSchema)
import Language.Marlowe.Runtime.Web.Contract.Next.API (NextAPI)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (TransactionsAPI)
import Language.Marlowe.Runtime.Web.Core.Address (
  Address,
  StakeAddress,
 )
import Language.Marlowe.Runtime.Web.Core.Asset (
  AssetId,
  Assets,
  PolicyId,
 )
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (
  MarloweVersion,
 )
import Language.Marlowe.Runtime.Web.Core.Party (Party)
import Language.Marlowe.Runtime.Web.Core.Tx (
  TextEnvelope,
  TxOutRef,
  TxStatus,
 )
import Language.Marlowe.Runtime.Web.Payout.API (Payout)

import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )
import Language.Marlowe.Runtime.Web.Core.Metadata (Metadata)
import Language.Marlowe.Runtime.Web.Core.Roles (RolesConfig)
import Pipes (Producer)
import Servant (
  Capture,
  Description,
  FromHttpApiData,
  Get,
  Header',
  JSON,
  NewlineFraming,
  Optional,
  Post,
  PostCreated,
  Proxy (..),
  QueryFlag,
  QueryParam',
  QueryParams,
  ReqBody,
  Required,
  StreamBody,
  Strict,
  Summary,
  ToHttpApiData,
  type (:<|>),
  type (:>),
 )
import Servant.API (FromHttpApiData (..))
import Servant.Pagination (
  HasPagination (RangeType, getFieldValue),
 )

import Control.DeepSeq (NFData)
import Control.Lens ((&), (?~))
import Control.Monad ((<=<))
import Data.Aeson.Types (parseFail)
import Data.ByteString (ByteString)
import Data.OpenApi (
  HasOneOf (oneOf),
  HasPattern (pattern),
  HasType (type_),
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiString),
  ToParamSchema (..),
  ToSchema (..),
  declareSchemaRef,
 )
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import Language.Marlowe.Runtime.Web.Adapter.ByteString (hasLength)
import Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..))
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
  CardanoTxBody,
  ContractTx,
  CreateTxEnvelope,
  PostTxAPI,
  PutSignedTxAPI,
  TxJSON,
 )

type ContractId = TxOutRef

type ContractsAPI =
  GetContractsAPI
    :<|> PostContractsAPI
    :<|> Capture "contractId" TxOutRef :> ContractAPI
    :<|> "sources" :> ContractSourcesAPI

-- | GET /contracts sub-API
type GetContractsAPI =
  Summary "Get contracts"
    :> Description
        "Get contracts published on chain. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getContracts"
    :> QueryParams "roleCurrency" PolicyId
    :> QueryParams "tag" Text
    :> QueryParams "partyAddress" Address
    :> QueryParams "partyRole" AssetId
    :> RenameResponseSchema "GetContractsResponse"
    :> PaginatedGet '["contractId"] GetContractsResponse

type GetContractsResponse = WithLink "transactions" (WithLink "contract" ContractHeader)

-- | POST /contracts sub-API
type PostContractsAPI =
  Summary "Create a new contract"
    :> Description
        "Build an unsigned (Cardano) transaction body which opens a new Marlowe contract. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the PUT /contracts/{contractId} endpoint."
    :> OperationId "createContract"
    :> RenameResponseSchema "CreateContractResponse"
    :> Header'
        '[Optional, Strict, Description "Where to send staking rewards for the Marlowe script outputs of this contract."]
        "X-Stake-Address"
        StakeAddress
    :> ( ReqBody '[JSON] PostContractsRequest :> PostTxAPI (PostCreated '[JSON] (PostContractsResponse CardanoTxBody))
          :<|> ReqBody '[JSON] PostContractsRequest :> PostTxAPI (PostCreated '[TxJSON ContractTx] (PostContractsResponse CardanoTx))
       )

-- | /contracts/:contractId sub-API
type ContractAPI =
  GetContractAPI
    :<|> Summary "Submit contract to chain"
      :> Description
          "Submit a signed (Cardano) transaction that opens a new Marlowe contract. \
          \The transaction must have originally been created by the POST /contracts endpoint. \
          \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
          \it will not wait for the transaction to be published in a block. \
          \Use the GET /contracts/{contractId} endpoint to poll the on-chain status."
      :> OperationId "submitContract"
      :> PutSignedTxAPI
    :<|> "next" :> NextAPI
    :<|> "transactions" :> TransactionsAPI

type GetContractAPI =
  Summary "Get contract by ID"
    :> OperationId "getContractById"
    :> RenameResponseSchema "GetContractResponse"
    :> Get '[JSON] GetContractResponse

type GetContractResponse = WithLink "transactions" ContractState

-- | /contracts/sources sub-API
type ContractSourcesAPI =
  PostContractSourcesAPI
    :<|> Capture "contractSourceId" ContractSourceId :> ContractSourceAPI

-- | /contracts/sources/:contractSourceId sub-API
type ContractSourceAPI =
  GetContractSourceAPI
    :<|> "adjacency"
      :> Summary "Get adjacent contract source IDs by ID"
      :> Description
          "Get the contract source IDs which are adjacent to a contract source (they appear directly in the contract source)."
      :> OperationId "getContractSourceAdjacency"
      :> GetContractSourceIdsAPI
    :<|> "closure"
      :> Summary "Get contract source closure by ID"
      :> Description
          "Get the contract source IDs which appear in the full hierarchy of a contract source (including the ID of the contract source its self)."
      :> OperationId "getContractSourceClosure"
      :> GetContractSourceIdsAPI

type PostContractSourcesAPI =
  Summary "Upload contract sources"
    :> Description
        "Upload a bundle of marlowe objects as contract sources. This API supports request body streaming, with newline \
        \framing between request bundles."
    :> OperationId "createContractSources"
    :> QueryParam' '[Required, Description "The label of the top-level contract object in the bundle(s)."] "main" Label
    :> StreamBody NewlineFraming JSON (Producer ObjectBundle IO ())
    :> Post '[JSON] PostContractSourceResponse

type GetContractSourceAPI =
  Summary "Get contract source by ID"
    :> OperationId "getContractSourceById"
    :> QueryFlag "expand"
    :> Get '[JSON] Contract

type GetContractSourceIdsAPI = RenameResponseSchema "ContractSourceIds" :> Get '[JSON] (ListObject ContractSourceId)

type PostContractsResponse tx = WithLink "contract" (CreateTxEnvelope tx)

data ContractState = ContractState
  { contractId :: ContractId
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , initialContract :: Semantics.Contract
  , initialState :: Semantics.State
  , currentContract :: Maybe Semantics.Contract
  , state :: Maybe Semantics.State
  , utxo :: Maybe TxOutRef
  , assets :: Assets
  , txBody :: Maybe TextEnvelope
  , unclaimedPayouts :: [Payout]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data ContractHeader = ContractHeader
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance NFData ContractHeader

data PostContractSourceResponse = PostContractSourceResponse
  { contractSourceId :: ContractSourceId
  , intermediateIds :: Map Label ContractSourceId
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data PostContractsRequest = PostContractsRequest
  { tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , version :: MarloweVersion
  , roles :: Maybe RolesConfig
  , threadTokenName :: Maybe Text
  , contract :: ContractOrSourceId
  , accounts :: Map Party Assets
  , minUTxODeposit :: Maybe Integer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

newtype ContractSourceId = ContractSourceId {unContractSourceId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON) via Base16

instance FromHttpApiData ContractSourceId where
  parseUrlPiece = fmap ContractSourceId . (hasLength 32 . unBase16 <=< parseUrlPiece)

instance FromJSON ContractSourceId where
  parseJSON =
    withText "ContractSourceId" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToSchema ContractSourceId where
  declareNamedSchema = pure . NamedSchema (Just "ContractSourceId") . toParamSchema

instance ToParamSchema ContractSourceId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded identifier of a Marlowe contract source"
      & pattern ?~ "^[a-fA-F0-9]{64}$"

newtype ContractOrSourceId = ContractOrSourceId (Either Semantics.Contract ContractSourceId)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContractOrSourceId where
  parseJSON =
    fmap ContractOrSourceId . \case
      String "close" -> pure $ Left Semantics.Close
      String s -> Right <$> parseJSON (String s)
      j -> Left <$> parseJSON j

instance ToJSON ContractOrSourceId where
  toJSON = \case
    ContractOrSourceId (Left contract) -> toJSON contract
    ContractOrSourceId (Right hash) -> toJSON hash

instance ToSchema ContractOrSourceId where
  declareNamedSchema _ = do
    contractSchema <- declareSchemaRef $ Proxy @Semantics.Contract
    contractSourceIdSchema <- declareSchemaRef $ Proxy @ContractSourceId
    pure $
      NamedSchema Nothing $
        mempty
          & oneOf ?~ [contractSchema, contractSourceIdSchema]

instance HasPagination ContractHeader "contractId" where
  type RangeType ContractHeader "contractId" = ContractId
  getFieldValue _ ContractHeader{..} = contractId
