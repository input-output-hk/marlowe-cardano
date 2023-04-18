{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.

module Language.Marlowe.Runtime.Web.API
  where

import Control.Lens hiding ((.=))
import Control.Monad (guard, replicateM, unless, (<=<))
import Data.Aeson
import Data.Aeson.Types (parseFail)
import qualified Data.Aeson.Types as A
import Data.Bits (Bits(shiftL), (.|.))
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Functor (void, ($>))
import qualified Data.Map as Map
import Data.OpenApi
  ( Definitions
  , NamedSchema(..)
  , OpenApiType(..)
  , Referenced(..)
  , Schema
  , ToSchema
  , declareNamedSchema
  , declareSchemaRef
  , properties
  , required
  , type_
  )
import Data.OpenApi.Declare (Declare)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Base (Symbol)
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Runtime.Web.Types
import Network.HTTP.Media ((//))
import Servant
import Servant.Pagination
import Text.Parsec (char, digit, eof, hexDigit, many1, runParser, string)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

api :: Proxy API
api = Proxy

-- | The REST API of the Marlowe Runtime
type API = "contracts" :> ContractsAPI
      :<|> "withdrawals" :> WithdrawalsAPI
      :<|> "healthcheck" :> Get '[JSON] NoContent

-- | /contracts sub-API
type ContractsAPI = GetContractsAPI
               :<|> PostContractsAPI
               :<|> Capture "contractId" TxOutRef :> ContractAPI

-- | /withdrawals sub-API
type WithdrawalsAPI = GetWithdrawalsAPI
                 :<|> PostWithdrawalsAPI
                 :<|> Capture "withdrawalId" TxId :> WithdrawalAPI

-- | GET /contracts sub-API
type GetContractsAPI = QueryParams "roleCurrency" PolicyId
                    :> QueryParams "tag" Text
                    :> PaginatedGet '["contractId"] GetContractsResponse

type GetContractsResponse = WithLink "transactions" (WithLink "contract" ContractHeader)

instance HasNamedLink ContractHeader API "contract" where
  type Endpoint ContractHeader API "contract" =
    "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink ContractHeader{..} = Just $ mkLink contractId

instance HasNamedLink ContractHeader API "transactions" where
  type Endpoint ContractHeader API "transactions" =
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
  mimeUnrender _ bs = eitherDecode bs

instance HasNamedLink (CreateTxEnvelope tx) API "contract" where
  type Endpoint (CreateTxEnvelope tx) API "contract" =
    "contracts" :> Capture "contractId" TxOutRef :> GetContractAPI
  namedLink _ _ mkLink CreateTxEnvelope{..} = Just $ mkLink contractId

-- | POST /contracts sub-API
type PostContractsAPI
  =  ReqBody '[JSON] PostContractsRequest :> PostTxAPI (PostCreated '[JSON] (PostContractsResponse CardanoTxBody))
  :<|> ReqBody '[JSON] PostContractsRequest :> PostTxAPI (PostCreated '[TxJSON ContractTx] (PostContractsResponse CardanoTx))

-- | /contracts/:contractId sup-API
type ContractAPI = GetContractAPI
              :<|> PutSignedTxAPI
              :<|> "transactions" :> TransactionsAPI

-- | GET /contracts/:contractId sub-API
type GetContractAPI = Get '[JSON] GetContractResponse

type GetContractResponse = WithLink "transactions" ContractState

instance HasNamedLink ContractState API "transactions" where
  type Endpoint ContractState API "transactions" =
    "contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI
  namedLink _ _ mkLink ContractState{..} = guard (status == Confirmed) $> mkLink contractId

-- | /contracts/:contractId/transactions sup-API
type TransactionsAPI = GetTransactionsAPI
                  :<|> PostTransactionsAPI
                  :<|> Capture "transactionId" TxId :> TransactionAPI

data ApplyInputsTx

instance Accept (TxJSON ApplyInputsTx) where
    contentType _ = "application" // "vendor.iog.marlowe-runtime.apply-inputs-tx-json"

instance MimeRender (TxJSON ApplyInputsTx) (PostTransactionsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON ApplyInputsTx) (PostTransactionsResponse CardanoTx) where
  mimeUnrender _ bs = eitherDecode bs


-- | POST /contracts/:contractId/transactions sub-API
type PostTransactionsAPI
  =  ReqBody '[JSON] PostTransactionsRequest :> PostTxAPI (PostCreated '[JSON] (PostTransactionsResponse CardanoTxBody))
  :<|> ReqBody '[JSON] PostTransactionsRequest :> PostTxAPI (PostCreated '[TxJSON ApplyInputsTx] (PostTransactionsResponse CardanoTx))

type PostTransactionsResponse tx = WithLink "transaction" (ApplyInputsTxEnvelope tx)

instance HasNamedLink (ApplyInputsTxEnvelope tx) API "transaction" where
  type Endpoint (ApplyInputsTxEnvelope tx) API "transaction" =
    "contracts"
    :> Capture "contractId" TxOutRef
    :> "transactions"
    :> Capture "transactionId" TxId
    :> GetTransactionAPI
  namedLink _ _ mkLink ApplyInputsTxEnvelope{..} = Just $ mkLink contractId transactionId

-- | GET /contracts/:contractId/transactions sup-API
type GetTransactionsAPI = PaginatedGet '["transactionId"] GetTransactionsResponse

type GetTransactionsResponse = WithLink "transaction" TxHeader

instance HasNamedLink TxHeader API "transaction" where
  type Endpoint TxHeader API "transaction" =
    "contracts"
    :> Capture "contractId" TxOutRef
    :> "transactions"
    :> Capture "transactionId" TxId
    :> GetTransactionAPI
  namedLink _ _ mkLink TxHeader{..} = Just $ mkLink contractId transactionId

-- | /contracts/:contractId/transactions/:transactionId sup-API
type TransactionAPI = GetTransactionAPI
                 :<|> PutSignedTxAPI

-- | GET /contracts/:contractId/transactions/:transactionId sub-API
type GetTransactionAPI = Get '[JSON] GetTransactionResponse

type GetTransactionResponse = WithLink "previous" (WithLink "next" Tx)

type PutSignedTxAPI = ReqBody '[JSON] TextEnvelope :> PutAccepted '[JSON] NoContent

instance HasNamedLink Tx API "previous" where
  type Endpoint Tx API "previous" =
    "contracts"
    :> Capture "contractId" TxOutRef
    :> "transactions"
    :> Capture "transactionId" TxId
    :> GetTransactionAPI
  namedLink _ _ mkLink Tx{..} = guard (inputUtxo /= contractId) $> mkLink contractId (txId inputUtxo)

instance HasNamedLink Tx API "next" where
  type Endpoint Tx API "next" =
    "contracts"
    :> Capture "contractId" TxOutRef
    :> "transactions"
    :> Capture "transactionId" TxId
    :> GetTransactionAPI
  namedLink _ _ mkLink Tx{..} = mkLink contractId <$> consumingTx

-- | GET /contracts/:contractId/withdrawals sup-API
type GetWithdrawalsAPI = QueryParams "roleCurrency" PolicyId
                      :> PaginatedGet '["withdrawalId"] GetWithdrawalsResponse

type GetWithdrawalsResponse = WithLink "withdrawal" WithdrawalHeader

instance HasNamedLink WithdrawalHeader API "withdrawal" where
  type Endpoint WithdrawalHeader API "withdrawal" =
    "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink WithdrawalHeader{..} = Just $ mkLink withdrawalId

-- | POST /contracts sub-API
type PostWithdrawalsAPI
  =  ReqBody '[JSON] PostWithdrawalsRequest :> PostTxAPI (PostCreated '[JSON] (PostWithdrawalsResponse CardanoTxBody))
  :<|> ReqBody '[JSON] PostWithdrawalsRequest :> PostTxAPI (PostCreated '[TxJSON WithdrawTx] (PostWithdrawalsResponse CardanoTx))

type PostWithdrawalsResponse tx = WithLink "withdrawal" (WithdrawTxEnvelope tx)

data WithdrawTx

instance Accept (TxJSON WithdrawTx) where
    contentType _ = "application" // "vendor.iog.marlowe-runtime.withdraw-tx-json"

instance MimeRender (TxJSON WithdrawTx) (PostWithdrawalsResponse CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON WithdrawTx) (PostWithdrawalsResponse CardanoTx) where
  mimeUnrender _ bs = eitherDecode bs

instance HasNamedLink (WithdrawTxEnvelope tx) API "withdrawal" where
  type Endpoint (WithdrawTxEnvelope tx) API "withdrawal" =
    "withdrawals" :> Capture "withdrawalId" TxId :> GetWithdrawalAPI
  namedLink _ _ mkLink WithdrawTxEnvelope{..} = Just $ mkLink withdrawalId

-- | /contracts/:contractId/withdrawals/:withdrawalId sup-API
type WithdrawalAPI = GetWithdrawalAPI
                :<|> PutSignedTxAPI

-- | GET /contracts/:contractId/withdrawals/:withdrawalId sub-API
type GetWithdrawalAPI = Get '[JSON] Withdrawal

-- | Helper type for defining generic paginated GET endpoints
type PaginatedGet rangeFields resource
  =  Header "Range" (Ranges rangeFields resource)
  :> GetPartialContent '[JSON] (PaginatedResponse rangeFields resource)

-- | Helper type for describing the response type of generic paginated APIs
type PaginatedResponse fields resource =
  Headers (Header "Total-Count" Int ': PageHeaders fields resource) (ListObject resource)

newtype ListObject a = ListObject { results :: [a] }
  deriving (Eq, Show, Ord, Functor, Generic)

instance ToJSON a => ToJSON (ListObject a)
instance FromJSON a => FromJSON (ListObject a)
instance ToSchema a => ToSchema (ListObject a)

type PostTxAPI api
  =  Header' '[Required, Strict] "X-Change-Address" Address
  :> Header "X-Address" (CommaList Address)
  :> Header "X-Collateral-UTxO" (CommaList TxOutRef)
  :> api

class ParseHttpApiData a where
  urlPieceParser :: Parser a

instance ParseHttpApiData TxOutRef where
  urlPieceParser = TxOutRef <$> urlPieceParser <*> do
    _ <- string "%23"
    digits <- many1 digit
    case readMaybe digits of
      Just txIx -> pure txIx
      Nothing -> fail "txIx too large"

instance ParseHttpApiData TxId where
  urlPieceParser = do
    let
      octet :: Parser Word8
      octet = do
        gb <- hexDigit
        lb <- hexDigit
        let gbi = fromIntegral $ digitToInt gb
        let lbi = fromIntegral $ digitToInt lb
        pure $ shiftL gbi 4 .|. lbi
    octets <- replicateM 32 octet
    pure $ TxId $ BS.pack octets

class HasLink endpoint => HasLinkParser endpoint where
  linkParser :: Bool -> Proxy endpoint -> Parser (MkLink endpoint a -> a)

instance (KnownSymbol seg, HasLinkParser endpoint) => HasLinkParser (seg :> endpoint) where
  linkParser isStart _ = do
    unless isStart $ void $ char '/'
    string (symbolVal $ Proxy @seg) *> linkParser False (Proxy @endpoint)

instance HasLinkParser (Verb m s ct a) where
  linkParser _ _ = eof $> id

instance HasLinkParser sub => HasLinkParser (Header' mods sym a :> sub) where
  linkParser isStart _ = linkParser isStart $ Proxy @sub

instance
  ( ParseHttpApiData a
  , ToHttpApiData a
  , HasLinkParser sub
  ) => HasLinkParser (Capture' mods name a :> sub) where
  linkParser isStart _ = do
    unless isStart $ void $ char '/'
    a <- urlPieceParser
    withSubMkLink <- linkParser False $ Proxy @sub
    pure \mkLink -> withSubMkLink $ mkLink a

class (IsElem (Endpoint a api name) api, HasLink (Endpoint a api name)) => HasNamedLink a api (name :: Symbol) where
  type (Endpoint a api name) :: *
  namedLink :: Proxy api -> Proxy name -> MkLink (Endpoint a api name) Link -> a -> Maybe Link

instance HasNamedLink a api name => HasNamedLink (WithLink name' a) api name where
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
  showsPrec p (IncludeLink name a) = showParen (p >= 11)
    ( showString "IncludeLink (Proxy @"
    . showSpace
    . showsPrec 11 (symbolVal name)
    . showString ")"
    . showSpace
    . showsPrec 11 a
    )
  showsPrec p (OmitLink a) = showParen (p >= 11)
    ( showString "OmitLink"
    . showSpace
    . showsPrec 11 a
    )

class ToJSONWithLinks a where
  toJSONWithLinks :: a -> ([(String, Link)], Value)

class FromJSONWithLinks a where
  fromJSONWithLinks :: ([(String, String)], Value) -> A.Parser a

instance {-# OVERLAPPING #-}
  ( HasNamedLink a API name
  , ToJSONWithLinks a
  , KnownSymbol name
  ) => ToJSONWithLinks (WithLink name a) where
  toJSONWithLinks (IncludeLink name a) = (maybe links (: links) link, value)
    where
      (links, value) = toJSONWithLinks a
      link = (symbolVal name,) <$> namedLink api name (safeLink api $ Proxy @(Endpoint a API name)) a
  toJSONWithLinks (OmitLink a) = toJSONWithLinks a

instance {-# OVERLAPPING #-} ToJSON a => ToJSONWithLinks a where
  toJSONWithLinks a = ([], toJSON a)

instance {-# OVERLAPPING #-}
  ( HasLinkParser (Endpoint a API name)
  , FromJSONWithLinks a
  , KnownSymbol name
  ) => FromJSONWithLinks (WithLink name a) where
  fromJSONWithLinks (links, value) = do
    let mUri = lookup (symbolVal $ Proxy @name) links
    case mUri of
      Nothing -> OmitLink <$> fromJSONWithLinks (links, value)
      Just uri -> case runParser (linkParser True (Proxy @(Endpoint a API name))) () "" uri of
        Right _ -> IncludeLink (Proxy @name) <$> fromJSONWithLinks (links, value)
        Left err -> parseFail $ show err

instance {-# OVERLAPPING #-} FromJSON a => FromJSONWithLinks a where
  fromJSONWithLinks = parseJSON . snd

instance
  ( HasNamedLink a API name
  , ToJSONWithLinks a
  , KnownSymbol name
  ) => ToJSON (WithLink name a) where
  toJSON = toJSON' . toJSONWithLinks
    where
      toJSON' (links, value) = object
        [ "resource" .= value
        , "links" .= object (bimap fromString (toJSON . show . linkURI) <$> links)
        ]

instance
  ( HasLinkParser (Endpoint a API name)
  , FromJSONWithLinks a
  , KnownSymbol name
  ) => FromJSON (WithLink name a) where
  parseJSON = fromJSONWithLinks <=< parseJSON'
    where
      parseJSON' = withObject "WithLink" \obj -> do
        value <- obj .: "resource"
        links <- Map.toList <$> obj .: "links"
        pure (links, value)

instance HasPagination resource field => HasPagination (WithLink name resource) field where
  type RangeType (WithLink name resource) field = RangeType resource field
  getFieldValue p (IncludeLink _ resource) = getFieldValue p resource
  getFieldValue p (OmitLink resource) = getFieldValue p resource

class ToSchemaWithLinks a where
  declareNamedSchemaWithLinks :: Proxy a -> Declare (Definitions Schema) ([String], Referenced Schema)

instance {-# OVERLAPPING #-}
  ( ToSchemaWithLinks a
  , KnownSymbol name
  ) => ToSchemaWithLinks (WithLink name a) where
  declareNamedSchemaWithLinks _  = do
    (links, namedSchema) <- declareNamedSchemaWithLinks (Proxy @a)
    pure (symbolVal (Proxy @name) : links, namedSchema)

instance {-# OVERLAPPING #-} ToSchema a => ToSchemaWithLinks a where
  declareNamedSchemaWithLinks p = ([],) <$> declareSchemaRef p

instance
  ( Typeable a
  , ToSchemaWithLinks a
  , KnownSymbol name
  ) => ToSchema (WithLink name a) where
  declareNamedSchema _  = do
    (links, schema) <- declareNamedSchemaWithLinks (Proxy @(WithLink name a))
    stringSchema <- declareSchemaRef (Proxy @String)
    pure $ NamedSchema Nothing $ mempty
      & type_ ?~ OpenApiObject
      & required .~ ["resource", "links"]
      & properties .~
          [ ("resource", schema)
          , ( "links", Inline $ mempty
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
  ) => ContentRangeFromHttpApiData (field ': fields) resource where
  contentRangeFromHttpApiData field start end
    | field == T.pack (symbolVal $ Proxy @field) = ContentRange
        <$> parseUrlPiece start
        <*> parseUrlPiece end
        <*> pure (Proxy @field)
    | otherwise = do
        ContentRange start' end' field' <-
          contentRangeFromHttpApiData @fields @resource field start end
        pure $ ContentRange start' end' field'

instance ContentRangeFromHttpApiData fields resource
  => FromHttpApiData (ContentRange fields resource) where
  parseUrlPiece text = case T.splitOn " " text of
    [field, suffix] -> case T.splitOn ".." suffix of
      [start, end] -> contentRangeFromHttpApiData field start end
      _ -> Left "Invalid content range"
    _ -> Left "Invalid content range"

instance FromHttpApiData (AcceptRanges fields) where
  parseUrlPiece = const $ Right AcceptRanges
