{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.
module Language.Marlowe.Runtime.Web.API where

import Control.Lens hiding ((.=))
import Control.Monad (guard, replicateM, unless, (<=<))
import Data.Aeson
import Data.Aeson.Types (parseFail)
import qualified Data.Aeson.Types as A
import Data.Bits (Bits (shiftL), (.|.))
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Functor (void, ($>))
import qualified Data.Map as Map
import Data.OpenApi (
  Definitions,
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  Schema,
  ToSchema,
  allOperations,
  declareNamedSchema,
  declareSchemaRef,
  operationId,
  properties,
  required,
  type_,
 )
import Data.OpenApi.Declare (Declare)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Data.Word (Word8)
import GHC.Base (Symbol)
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Core.V1.Next
import Language.Marlowe.Runtime.Web.Next.Schema ()

import Data.Kind (Type)
import Data.Text.Encoding (encodeUtf8)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Object.Types (Label, ObjectBundle)
import Language.Marlowe.Runtime.Web.Types
import Network.HTTP.Media ((//))
import Network.Wai (mapResponseHeaders)
import Pipes (Producer)
import Servant
import Servant.Client (HasClient (..))
import Servant.Client.Core (RunClient)
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Pagination
import Servant.Server.Internal.RouteResult (RouteResult (..))
import Servant.Server.Internal.RoutingApplication (RoutingApplication)
import Text.Parsec (char, digit, eof, hexDigit, many1, runParser, string)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

api :: Proxy API
api = Proxy

data WithRuntimeStatus api

data OperationId (name :: Symbol)

data RenameResponseSchema (name :: Symbol)

data RenameSchema (name :: Symbol) a

instance (KnownSymbol name, ToSchema a) => ToSchema (RenameSchema name a) where
  declareNamedSchema _ = do
    NamedSchema _ schema <- declareNamedSchema $ Proxy @a
    pure $ NamedSchema (Just $ T.pack $ symbolVal $ Proxy @name) schema

instance (HasServer sub ctx) => HasServer (OperationId name :> sub) ctx where
  type ServerT (OperationId name :> sub) m = ServerT sub m
  route _ = route $ Proxy @sub
  hoistServerWithContext _ = hoistServerWithContext $ Proxy @sub

instance (HasClient m api) => HasClient m (OperationId name :> api) where
  type Client m (OperationId name :> api) = Client m api
  clientWithRoute m _ = clientWithRoute m $ Proxy @api
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @api

instance (KnownSymbol name, HasOpenApi api) => HasOpenApi (OperationId name :> api) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & allOperations . operationId ?~ T.pack (symbolVal $ Proxy @name)

instance (HasServer sub ctx) => HasServer (RenameResponseSchema name :> sub) ctx where
  type ServerT (RenameResponseSchema name :> sub) m = ServerT sub m
  route _ = route $ Proxy @sub
  hoistServerWithContext _ = hoistServerWithContext $ Proxy @sub

instance (HasClient m api) => HasClient m (RenameResponseSchema name :> api) where
  type Client m (RenameResponseSchema name :> api) = Client m api
  clientWithRoute m _ = clientWithRoute m $ Proxy @api
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @api

instance (KnownSymbol name, HasOpenApi (AddRenameSchema name api)) => HasOpenApi (RenameResponseSchema name :> api) where
  toOpenApi _ = toOpenApi $ Proxy @(AddRenameSchema name api)

type instance IsElem' e (WithRuntimeStatus api) = IsElem e api

instance (HasServer api ctx) => HasServer (WithRuntimeStatus api) (IO RuntimeStatus ': ctx) where
  type ServerT (WithRuntimeStatus api) m = ServerT api m
  route _ (getStatus :. ctx) =
    fmap addStatusHeaders . route (Proxy @api) ctx
    where
      addStatusHeaders :: RoutingApplication -> RoutingApplication
      addStatusHeaders app req sendRes = app req \case
        Fail err -> sendRes $ Fail err
        FailFatal err -> sendRes $ FailFatal err
        Route res -> do
          status <- getStatus
          sendRes $ Route $ mapResponseHeaders (<> statusHeaders status) res
      statusHeaders RuntimeStatus{..} =
        [ ("X-Node-Tip", encodeUtf8 $ toUrlPiece nodeTip)
        , ("X-Runtime-Chain-Tip", encodeUtf8 $ toUrlPiece runtimeChainTip)
        , ("X-Runtime-Tip", encodeUtf8 $ toUrlPiece runtimeTip)
        , ("X-Runtime-Version", encodeUtf8 $ toUrlPiece runtimeVersion)
        , ("X-Network-Id", encodeUtf8 $ toUrlPiece networkId)
        ]
  hoistServerWithContext _ _ = hoistServerWithContext (Proxy @api) (Proxy @ctx)

type StatusHeaders =
  '[ Header "X-Node-Tip" ChainTip
   , Header "X-Runtime-Chain-Tip" ChainTip
   , Header "X-Runtime-Tip" ChainTip
   , Header "X-Runtime-Version" Version
   , Header "X-Network-Id" NetworkId
   ]

type family AppendStatusHeaders hs where
  AppendStatusHeaders '[] = StatusHeaders
  AppendStatusHeaders (h ': hs) = h ': AppendStatusHeaders hs

type family AddStatusHeaders api where
  AddStatusHeaders (path :> api) = path :> AddStatusHeaders api
  AddStatusHeaders (a :<|> b) = AddStatusHeaders a :<|> AddStatusHeaders b
  AddStatusHeaders (Verb method cTypes status (Headers hs a)) =
    Verb method cTypes status (Headers (AppendStatusHeaders hs) a)
  AddStatusHeaders (Verb method cTypes status a) = Verb method cTypes status (Headers StatusHeaders a)
  AddStatusHeaders (Stream method status framing ct (Headers hs a)) =
    Stream method status framing ct (Headers (AppendStatusHeaders hs) a)
  AddStatusHeaders (Stream cTypes status framing ct a) = Stream cTypes status framing ct (Headers StatusHeaders a)

type family AddRenameSchema name api where
  AddRenameSchema name (path :> api) = path :> AddRenameSchema name api
  AddRenameSchema name (a :<|> b) = AddRenameSchema name a :<|> AddRenameSchema name b
  AddRenameSchema name (Verb method cTypes status (Headers hs a)) =
    Verb method cTypes status (Headers hs (RenameSchema name a))
  AddRenameSchema name (Verb method cTypes status a) = Verb method cTypes status (RenameSchema name a)
  AddRenameSchema name (Stream method status framing ct (Headers hs a)) =
    Stream method status framing ct (Headers hs (RenameSchema name a))
  AddRenameSchema name (Stream cTypes status framing ct a) = Stream cTypes status framing ct (RenameSchema name a)

instance (RunClient m, HasClient m (AddStatusHeaders api)) => HasClient m (WithRuntimeStatus api) where
  type Client m (WithRuntimeStatus api) = Client m (AddStatusHeaders api)
  clientWithRoute m _ = clientWithRoute m $ Proxy @(AddStatusHeaders api)
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @(AddStatusHeaders api)

instance (HasOpenApi (AddStatusHeaders api)) => HasOpenApi (WithRuntimeStatus api) where
  toOpenApi _ = toOpenApi $ Proxy @(AddStatusHeaders api)

-- | The REST API of the Marlowe Runtime
type API =
  WithRuntimeStatus
    ( "contracts" :> ContractsAPI
        :<|> "withdrawals" :> WithdrawalsAPI
        :<|> "payouts" :> PayoutsAPI
        :<|> "healthcheck"
          :> ( Summary "Test server status"
                :> Description "Check if the server is running and ready to respond to requests."
                :> OperationId "healthcheck"
                :> Get '[JSON] NoContent
             )
    )

-- | /contracts sub-API
type ContractsAPI =
  GetContractsAPI
    :<|> PostContractsAPI
    :<|> Capture "contractId" TxOutRef :> ContractAPI
    :<|> "sources" :> ContractSourcesAPI

-- | /withdrawals sub-API
type WithdrawalsAPI =
  GetWithdrawalsAPI
    :<|> PostWithdrawalsAPI
    :<|> Capture "withdrawalId" TxId :> WithdrawalAPI

-- | /payouts sub-API
type PayoutsAPI =
  GetPayoutsAPI
    :<|> Capture "payoutId" TxOutRef :> GetPayoutAPI

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

instance HasNamedLink ContractHeader API "contract" where
  type
    Endpoint ContractHeader API "contract" =
      "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink ContractHeader{..} = Just $ mkLink contractId

instance HasNamedLink ContractHeader API "transactions" where
  type
    Endpoint ContractHeader API "transactions" =
      "contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI
  namedLink _ _ mkLink ContractHeader{..} = guard (status == Confirmed) $> mkLink contractId

type PostContractsResponse tx = WithLink "contract" (CreateTxEnvelope tx)

data TxJSON a

data ContractTx

instance Accept (TxJSON ContractTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.contract-tx-json"

instance MimeRender (TxJSON ContractTx) (PostContractsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON ContractTx) (PostContractsResponse CardanoTx) where
  mimeUnrender _ = eitherDecode

instance HasNamedLink (CreateTxEnvelope tx) API "contract" where
  type
    Endpoint (CreateTxEnvelope tx) API "contract" =
      "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink CreateTxEnvelope{..} = Just $ mkLink contractId

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

-- | GET /contracts/:contractId sub-API
type GetContractAPI =
  Summary "Get contract by ID"
    :> OperationId "getContractById"
    :> RenameResponseSchema "GetContractResponse"
    :> Get '[JSON] GetContractResponse

type GetContractResponse = WithLink "transactions" ContractState

instance HasNamedLink ContractState API "transactions" where
  type
    Endpoint ContractState API "transactions" =
      "contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI
  namedLink _ _ mkLink ContractState{..} = guard (status == Confirmed) $> mkLink contractId

type NextAPI = GETNextContinuationAPI

-- | GET /contracts/:contractId/next/continuation sub-API
type GETNextContinuationAPI =
  Summary "Get next contract steps"
    :> Description "Get inputs which could be performed on a contract withing a time range by the requested parties."
    :> OperationId "getNextStepsForContract"
    :> QueryParam' '[Required, Description "The beginning of the validity range."] "validityStart" UTCTime
    :> QueryParam' '[Required, Description "The end of the validity range."] "validityEnd" UTCTime
    :> QueryParams "party" Party
    :> Get '[JSON] Next

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

-- | /contracts/:contractId/transactions sub-API
type TransactionsAPI =
  GetTransactionsAPI
    :<|> PostTransactionsAPI
    :<|> Capture "transactionId" TxId :> TransactionAPI

data ApplyInputsTx

instance Accept (TxJSON ApplyInputsTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.apply-inputs-tx-json"

instance MimeRender (TxJSON ApplyInputsTx) (PostTransactionsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON ApplyInputsTx) (PostTransactionsResponse CardanoTx) where
  mimeUnrender _ = eitherDecode

-- | POST /contracts/:contractId/transactions sub-API
type PostTransactionsAPI =
  Summary "Apply inputs to contract"
    :> Description
        "Build an unsigned (Cardano) transaction body which applies inputs to an open Marlowe contract. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the PUT /contracts/{contractId}/transactions/{transactionId} endpoint."
    :> OperationId "applyInputsToContract"
    :> RenameResponseSchema "ApplyInputsResponse"
    :> ( ReqBody '[JSON] PostTransactionsRequest :> PostTxAPI (PostCreated '[JSON] (PostTransactionsResponse CardanoTxBody))
          :<|> ReqBody '[JSON] PostTransactionsRequest
            :> PostTxAPI (PostCreated '[TxJSON ApplyInputsTx] (PostTransactionsResponse CardanoTx))
       )

type PostTransactionsResponse tx = WithLink "transaction" (ApplyInputsTxEnvelope tx)

instance HasNamedLink (ApplyInputsTxEnvelope tx) API "transaction" where
  type
    Endpoint (ApplyInputsTxEnvelope tx) API "transaction" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink ApplyInputsTxEnvelope{..} = Just $ mkLink contractId transactionId

-- | GET /contracts/:contractId/transactions sub-API
type GetTransactionsAPI =
  Summary "Get transactions for contract"
    :> Description
        "Get published transactions for a contract. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getTransactionsForContract"
    :> RenameResponseSchema "GetTransactionsResponse"
    :> PaginatedGet '["transactionId"] GetTransactionsResponse

type GetTransactionsResponse = WithLink "transaction" TxHeader

instance HasNamedLink TxHeader API "transaction" where
  type
    Endpoint TxHeader API "transaction" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink TxHeader{..} = Just $ mkLink contractId transactionId

-- | /contracts/:contractId/transactions/:transactionId sub-API
type TransactionAPI =
  GetTransactionAPI
    :<|> Summary "Submit contract input application"
      :> Description
          "Submit a signed (Cardano) transaction that applies inputs to an open Marlowe contract. \
          \The transaction must have originally been created by the POST /contracts/{contractId}/transactions endpoint. \
          \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
          \it will not wait for the transaction to be published in a block. \
          \Use the GET /contracts/{contractId}/transactions/{transactionId} endpoint to poll the on-chain status."
      :> OperationId "submitContractTransaction"
      :> PutSignedTxAPI

-- | GET /contracts/:contractId/transactions/:transactionId sub-API
type GetTransactionAPI =
  Summary "Get contract transaction by ID"
    :> OperationId "getContractTransactionById"
    :> RenameResponseSchema "GetTransactionResponse"
    :> Get '[JSON] GetTransactionResponse

type GetTransactionResponse = WithLink "previous" (WithLink "next" Tx)

type PutSignedTxAPI = ReqBody '[JSON] TextEnvelope :> PutAccepted '[JSON] NoContent

instance HasNamedLink Tx API "previous" where
  type
    Endpoint Tx API "previous" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink Tx{..} = guard (inputUtxo /= contractId) $> mkLink contractId (txId inputUtxo)

instance HasNamedLink Tx API "next" where
  type
    Endpoint Tx API "next" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink Tx{..} = mkLink contractId <$> consumingTx

-- | GET /contracts/:contractId/withdrawals sub-API
type GetWithdrawalsAPI =
  Summary "Get withdrawals"
    :> Description
        "Get published withdrawal transactions. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getWithdrawals"
    :> QueryParams "roleCurrency" PolicyId
    :> RenameResponseSchema "GetWithdrawalsResponse"
    :> PaginatedGet '["withdrawalId"] GetWithdrawalsResponse

type GetWithdrawalsResponse = WithLink "withdrawal" WithdrawalHeader

instance HasNamedLink WithdrawalHeader API "withdrawal" where
  type
    Endpoint WithdrawalHeader API "withdrawal" =
      "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink WithdrawalHeader{..} = Just $ mkLink withdrawalId

-- | GET /payouts sub-API
type GetPayoutsAPI =
  Summary "Get role payouts"
    :> Description
        "Get payouts to parties from role-based contracts. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getPayouts"
    :> QueryParams "contractId" TxOutRef
    :> QueryParams "roleToken" AssetId
    :> QueryParam'
        '[Optional, Description "Whether to include available or withdrawn payouts in the results."]
        "status"
        PayoutStatus
    :> RenameResponseSchema "GetPayoutsResponse"
    :> PaginatedGet '["payoutId"] GetPayoutsResponse

type GetPayoutsResponse = WithLink "payout" PayoutHeader

instance HasNamedLink PayoutHeader API "payout" where
  type
    Endpoint PayoutHeader API "payout" =
      "payouts" :> Capture "payoutId" TxOutRef :> GetPayoutAPI
  namedLink _ _ mkLink PayoutHeader{..} = Just $ mkLink payoutId

type GetPayoutAPI =
  Summary "Get payout by ID"
    :> OperationId "getPayoutById"
    :> RenameResponseSchema "GetPayoutResponse"
    :> Get '[JSON] GetPayoutResponse

type GetPayoutResponse = WithLink "contract" (WithLink "transaction" (WithLink "withdrawal" PayoutState))

instance HasNamedLink PayoutState API "contract" where
  type
    Endpoint PayoutState API "contract" =
      "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink PayoutState{..} = Just $ mkLink contractId

instance HasNamedLink PayoutState API "transaction" where
  type
    Endpoint PayoutState API "transaction" =
      "contracts"
        :> Capture "contractId" TxOutRef
        :> "transactions"
        :> Capture "transactionId" TxId
        :> GetTransactionAPI
  namedLink _ _ mkLink PayoutState{..} = Just $ mkLink contractId $ txId payoutId

instance HasNamedLink PayoutState API "withdrawal" where
  type
    Endpoint PayoutState API "withdrawal" =
      "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink PayoutState{..} = mkLink <$> withdrawalId

-- | POST /contracts sub-API
type PostWithdrawalsAPI =
  Summary "Withdraw payouts"
    :> Description
        "Build an unsigned (Cardano) transaction body which withdraws available payouts from a role payout validator. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the PUT /withdrawals/{withdrawalId} endpoint."
    :> OperationId "withdrawPayouts"
    :> RenameResponseSchema "WithdrawPayoutsResponse"
    :> ( ReqBody '[JSON] PostWithdrawalsRequest :> PostTxAPI (PostCreated '[JSON] (PostWithdrawalsResponse CardanoTxBody))
          :<|> ReqBody '[JSON] PostWithdrawalsRequest
            :> PostTxAPI (PostCreated '[TxJSON WithdrawTx] (PostWithdrawalsResponse CardanoTx))
       )

type PostWithdrawalsResponse tx = WithLink "withdrawal" (WithdrawTxEnvelope tx)

data WithdrawTx

instance Accept (TxJSON WithdrawTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.withdraw-tx-json"

instance MimeRender (TxJSON WithdrawTx) (PostWithdrawalsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON WithdrawTx) (PostWithdrawalsResponse CardanoTx) where
  mimeUnrender _ = eitherDecode

instance HasNamedLink (WithdrawTxEnvelope tx) API "withdrawal" where
  type
    Endpoint (WithdrawTxEnvelope tx) API "withdrawal" =
      "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink WithdrawTxEnvelope{..} = Just $ mkLink withdrawalId

-- | /contracts/:contractId/withdrawals/:withdrawalId sub-API
type WithdrawalAPI =
  GetWithdrawalAPI
    :<|> Summary "Submit payout withdrawal"
      :> Description
          "Submit a signed (Cardano) transaction that withdraws available payouts from a role payout validator. \
          \The transaction must have originally been created by the POST /withdrawals endpoint. \
          \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
          \it will not wait for the transaction to be published in a block. \
          \Use the GET /withdrawals/{withdrawalId} endpoint to poll the on-chain status."
      :> OperationId "submitWithdrawal"
      :> PutSignedTxAPI

-- | GET /contracts/:contractId/withdrawals/:withdrawalId sub-API
type GetWithdrawalAPI =
  Summary "Get withdrawal by ID"
    :> OperationId "getWithdrawalById"
    :> Get '[JSON] Withdrawal

-- | Helper type for defining generic paginated GET endpoints
type PaginatedGet rangeFields resource =
  Header "Range" (Ranges rangeFields resource)
    :> RenameResponseSchema "GetContractsResponse"
    :> GetPartialContent '[JSON] (PaginatedResponse rangeFields resource)

-- | Helper type for describing the response type of generic paginated APIs
type PaginatedResponse fields resource =
  Headers (Header "Total-Count" Int ': PageHeaders fields resource) (ListObject resource)

newtype ListObject a = ListObject {results :: [a]}
  deriving (Eq, Show, Ord, Functor, Generic)

instance (ToJSON a) => ToJSON (ListObject a)
instance (FromJSON a) => FromJSON (ListObject a)
instance (ToSchema a) => ToSchema (ListObject a)

type PostTxAPI api =
  Header' '[Required, Strict] "X-Change-Address" Address
    :> Header "X-Address" (CommaList Address)
    :> Header "X-Collateral-UTxO" (CommaList TxOutRef)
    :> api

class ParseHttpApiData a where
  urlPieceParser :: Parser a

instance ParseHttpApiData TxOutRef where
  urlPieceParser =
    TxOutRef <$> urlPieceParser <*> do
      _ <- string "%23"
      digits <- many1 digit
      case readMaybe digits of
        Just txIx -> pure txIx
        Nothing -> fail "txIx too large"

instance ParseHttpApiData TxId where
  urlPieceParser = do
    let octet :: Parser Word8
        octet = do
          gb <- hexDigit
          lb <- hexDigit
          let gbi = fromIntegral $ digitToInt gb
          let lbi = fromIntegral $ digitToInt lb
          pure $ shiftL gbi 4 .|. lbi
    octets <- replicateM 32 octet
    pure $ TxId $ BS.pack octets

class (HasLink endpoint) => HasLinkParser endpoint where
  linkParser :: Bool -> Proxy endpoint -> Parser (MkLink endpoint a -> a)

instance (KnownSymbol seg, HasLinkParser endpoint) => HasLinkParser (seg :> endpoint) where
  linkParser isStart _ = do
    unless isStart $ void $ char '/'
    string (symbolVal $ Proxy @seg) *> linkParser False (Proxy @endpoint)

instance HasLinkParser (Verb m s ct a) where
  linkParser _ _ = eof $> id

instance (HasLinkParser sub) => HasLinkParser (Header' mods sym a :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (Summary summary :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (Description desc :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLink sub) => HasLink (OperationId name :> sub) where
  type MkLink (OperationId name :> sub) link = MkLink sub link
  toLink f _ = toLink f $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (OperationId name :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance (HasLink sub) => HasLink (RenameResponseSchema name :> sub) where
  type MkLink (RenameResponseSchema name :> sub) link = MkLink sub link
  toLink f _ = toLink f $ Proxy @sub

instance (HasLinkParser sub) => HasLinkParser (RenameResponseSchema name :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance
  ( ParseHttpApiData a
  , ToHttpApiData a
  , HasLinkParser sub
  )
  => HasLinkParser (Capture' mods name a :> sub)
  where
  linkParser isStart _ = do
    unless isStart $ void $ char '/'
    a <- urlPieceParser
    withSubMkLink <- linkParser False $ Proxy @sub
    pure \mkLink -> withSubMkLink $ mkLink a

class (IsElem (Endpoint a api name) api, HasLink (Endpoint a api name)) => HasNamedLink a api (name :: Symbol) where
  type Endpoint a api name :: Type
  namedLink :: Proxy api -> Proxy name -> MkLink (Endpoint a api name) Link -> a -> Maybe Link

instance (HasNamedLink a api name) => HasNamedLink (WithLink name' a) api name where
  type Endpoint (WithLink name' a) api name = Endpoint a api name
  namedLink api' name mkLink = \case
    IncludeLink _ a -> namedLink api' name mkLink a
    OmitLink a -> namedLink api' name mkLink a

data WithLink (name :: Symbol) a where
  IncludeLink :: Proxy name -> a -> WithLink name a
  OmitLink :: a -> WithLink name a

retractLink :: WithLink name a -> a
retractLink (IncludeLink _ a) = a
retractLink (OmitLink a) = a

deriving instance Typeable (WithLink name a)

instance (Show a, KnownSymbol name) => Show (WithLink name a) where
  showsPrec p (IncludeLink name a) =
    showParen
      (p >= 11)
      ( showString "IncludeLink (Proxy @"
          . showSpace
          . showsPrec 11 (symbolVal name)
          . showString ")"
          . showSpace
          . showsPrec 11 a
      )
  showsPrec p (OmitLink a) =
    showParen
      (p >= 11)
      ( showString "OmitLink"
          . showSpace
          . showsPrec 11 a
      )

class ToJSONWithLinks a where
  toJSONWithLinks :: a -> ([(String, Link)], Value)

class FromJSONWithLinks a where
  fromJSONWithLinks :: ([(String, String)], Value) -> A.Parser a

instance
  {-# OVERLAPPING #-}
  ( HasNamedLink a API name
  , ToJSONWithLinks a
  , KnownSymbol name
  )
  => ToJSONWithLinks (WithLink name a)
  where
  toJSONWithLinks (IncludeLink name a) = (maybe links (: links) link, value)
    where
      (links, value) = toJSONWithLinks a
      link = (symbolVal name,) <$> namedLink api name (safeLink api $ Proxy @(Endpoint a API name)) a
  toJSONWithLinks (OmitLink a) = toJSONWithLinks a

instance {-# OVERLAPPING #-} (ToJSON a) => ToJSONWithLinks a where
  toJSONWithLinks a = ([], toJSON a)

instance
  {-# OVERLAPPING #-}
  ( HasLinkParser (Endpoint a API name)
  , FromJSONWithLinks a
  , KnownSymbol name
  )
  => FromJSONWithLinks (WithLink name a)
  where
  fromJSONWithLinks (links, value) = do
    let mUri = lookup (symbolVal $ Proxy @name) links
    case mUri of
      Nothing -> OmitLink <$> fromJSONWithLinks (links, value)
      Just uri -> case runParser (linkParser True (Proxy @(Endpoint a API name))) () "" uri of
        Right _ -> IncludeLink (Proxy @name) <$> fromJSONWithLinks (links, value)
        Left err -> parseFail $ show err

instance {-# OVERLAPPING #-} (FromJSON a) => FromJSONWithLinks a where
  fromJSONWithLinks = parseJSON . snd

instance
  ( HasNamedLink a API name
  , ToJSONWithLinks a
  , KnownSymbol name
  )
  => ToJSON (WithLink name a)
  where
  toJSON = toJSON' . toJSONWithLinks
    where
      toJSON' (links, value) =
        object
          [ "resource" .= value
          , "links" .= object (bimap fromString (toJSON . show . linkURI) <$> links)
          ]

instance
  ( HasLinkParser (Endpoint a API name)
  , FromJSONWithLinks a
  , KnownSymbol name
  )
  => FromJSON (WithLink name a)
  where
  parseJSON = fromJSONWithLinks <=< parseJSON'
    where
      parseJSON' = withObject "WithLink" \obj -> do
        value <- obj .: "resource"
        links <- Map.toList <$> obj .: "links"
        pure (links, value)

instance (HasPagination resource field) => HasPagination (WithLink name resource) field where
  type RangeType (WithLink name resource) field = RangeType resource field
  getFieldValue p (IncludeLink _ resource) = getFieldValue p resource
  getFieldValue p (OmitLink resource) = getFieldValue p resource

class ToSchemaWithLinks a where
  declareNamedSchemaWithLinks :: Proxy a -> Declare (Definitions Schema) ([String], Referenced Schema)

instance
  {-# OVERLAPPING #-}
  ( ToSchemaWithLinks a
  , KnownSymbol name
  )
  => ToSchemaWithLinks (WithLink name a)
  where
  declareNamedSchemaWithLinks _ = do
    (links, namedSchema) <- declareNamedSchemaWithLinks (Proxy @a)
    pure (symbolVal (Proxy @name) : links, namedSchema)

instance {-# OVERLAPPING #-} (ToSchema a) => ToSchemaWithLinks a where
  declareNamedSchemaWithLinks p = ([],) <$> declareSchemaRef p

instance
  ( Typeable a
  , ToSchemaWithLinks a
  , KnownSymbol name
  )
  => ToSchema (WithLink name a)
  where
  declareNamedSchema _ = do
    (links, schema) <- declareNamedSchemaWithLinks (Proxy @(WithLink name a))
    stringSchema <- declareSchemaRef (Proxy @String)
    pure $
      NamedSchema Nothing $
        mempty
          & type_ ?~ OpenApiObject
          & required .~ ["resource", "links"]
          & properties
            .~ [ ("resource", schema)
               ,
                 ( "links"
                 , Inline $
                    mempty
                      & type_ ?~ OpenApiObject
                      & properties .~ fromList ((,stringSchema) . fromString <$> links)
                 )
               ]

class ContentRangeFromHttpApiData fields resource where
  contentRangeFromHttpApiData :: Text -> Text -> Text -> Either Text (ContentRange fields resource)

instance ContentRangeFromHttpApiData '[] resource where
  contentRangeFromHttpApiData _ _ _ = Left "Invalid content range"

instance
  ( KnownSymbol field
  , ToHttpApiData (RangeType resource field)
  , FromHttpApiData (RangeType resource field)
  , ContentRangeFromHttpApiData fields resource
  )
  => ContentRangeFromHttpApiData (field ': fields) resource
  where
  contentRangeFromHttpApiData field start end
    | field == T.pack (symbolVal $ Proxy @field) =
        ContentRange
          <$> parseUrlPiece start
          <*> parseUrlPiece end
          <*> pure (Proxy @field)
    | otherwise = do
        ContentRange start' end' field' <-
          contentRangeFromHttpApiData @fields @resource field start end
        pure $ ContentRange start' end' field'

instance
  (ContentRangeFromHttpApiData fields resource)
  => FromHttpApiData (ContentRange fields resource)
  where
  parseUrlPiece text = case T.splitOn " " text of
    [field, suffix] -> case T.splitOn ".." suffix of
      [start, end] -> contentRangeFromHttpApiData field start end
      _ -> Left "Invalid content range"
    _ -> Left "Invalid content range"

instance FromHttpApiData (AcceptRanges fields) where
  parseUrlPiece = const $ Right AcceptRanges
