{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.

module Language.Marlowe.Runtime.Web.API
  where

import Control.Lens hiding ((.=))
import Control.Monad (guard, replicateM, (<=<))
import Data.Aeson
import Data.Aeson.Types (parseFail)
import qualified Data.Aeson.Types as A
import Data.Bifunctor (first)
import Data.Bits (Bits(shiftL), (.|.))
import qualified Data.ByteString as BS
import Data.Char (digitToInt)
import Data.Functor (($>))
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
import Servant
import Servant.Pagination
import Text.Parsec (anyChar, digit, eof, hexDigit, many1, manyTill, runParser, spaces, string)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

api :: Proxy API
api = Proxy

-- | The REST API of the Marlowe Runtime
type API = "contracts" :> ContractsAPI

-- | /contracts sub-API
type ContractsAPI = GetContractsAPI
               :<|> PostContractsAPI
               :<|> Capture "contractId" TxOutRef :> ContractAPI

-- | GET /contracts sub-API
type GetContractsAPI = PaginatedGet '["contractId"] GetContractsResponse

type GetContractsResponse = WithLink "contract" ContractHeader

parseContractId :: Parser TxOutRef
parseContractId = TxOutRef <$> parseTransactionId <*> do
  digits <- many1 digit
  case readMaybe digits of
    Just txIx -> pure txIx
    Nothing -> fail "txIx too large"

parseTransactionId :: Parser TxId
parseTransactionId = do
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

instance HasNamedLink ContractHeader API "contract" where
  namedLink _ _ ContractHeader{..} = Just $ safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> GetContractAPI))
    contractId
  parseLink _ _ _ = do
    contractId <- string "contracts/" *> parseContractId <* eof
    pure $ safeLink
      api
      (Proxy @("contracts" :> Capture "contractId" TxOutRef :> GetContractAPI))
      contractId

-- | POST /contracts sub-API
type PostContractsAPI
  =  ReqBody '[JSON] PostContractsRequest
  :> PostTxAPI (PostCreated '[JSON] PostContractsResponse)

type PostContractsResponse = WithLink "contract" CreateTxBody

instance HasNamedLink CreateTxBody API "contract" where
  namedLink _ _ CreateTxBody{..} = Just $ safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> GetContractAPI))
    contractId
  parseLink _ _ _ = do
    contractId <- string "contracts/" *> parseContractId <* eof
    pure $ safeLink
      api
      (Proxy @("contracts" :> Capture "contractId" TxOutRef :> GetContractAPI))
      contractId


-- | /contracts/:contractId sup-API
type ContractAPI = GetContractAPI
              :<|> PutSignedTxAPI
              :<|> "transactions" :> TransactionsAPI

-- | GET /contracts/:contractId sub-API
type GetContractAPI = Get '[JSON] GetContractResponse

type GetContractResponse = WithLink "transactions" ContractState

instance HasNamedLink ContractState API "transactions" where
  namedLink _ _ ContractState{..} = guard (status == Confirmed) $> safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI))
    contractId
  parseLink _ _ _ = do
    contractId <- string "contracts/"
      *> parseContractId
      <* string "/transactions"
      <* eof
    pure $ safeLink
      api
      (Proxy @("contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI))
      contractId

-- | /contracts/:contractId/transactions sup-API
type TransactionsAPI = GetTransactionsAPI
                  :<|> PostTransactionsAPI
                  :<|> Capture "transactionId" TxId :> TransactionAPI

-- | POST /contracts/:contractId/transactions sub-API
type PostTransactionsAPI
  =  ReqBody '[JSON] PostTransactionsRequest
  :> PostTxAPI (PostCreated '[JSON] PostTransactionsResponse)

type PostTransactionsResponse = WithLink "transaction" ApplyInputsTxBody

instance HasNamedLink ApplyInputsTxBody API "transaction" where
  namedLink _ _ ApplyInputsTxBody{..} = Just $ safeLink
    api
    (Proxy @("contracts"
          :> Capture "contractId" TxOutRef
          :> "transactions"
          :> Capture "transactionId" TxId
          :> GetTransactionAPI
    ))
    contractId
    transactionId

  parseLink _ _ _ = do
    contractId <- string "contracts/"
      *> parseContractId
      <* string "/transactions"
    transactionId <- string "/transactions"
      *> parseTransactionId
      <* eof
    pure $ safeLink
      api
      (Proxy @("contracts"
            :> Capture "contractId" TxOutRef
            :> "transactions"
            :> Capture "transactionId" TxId
            :> GetTransactionAPI
      ))
      contractId
      transactionId

-- | GET /contracts/:contractId/transactions sup-API
type GetTransactionsAPI = PaginatedGet '["transactionId"] GetTransactionsResponse

type GetTransactionsResponse = WithLink "transaction" TxHeader

instance HasNamedLink TxHeader API "transaction" where
  namedLink _ _ TxHeader{..} = Just $ safeLink
    api
    (Proxy @("contracts"
          :> Capture "contractId" TxOutRef
          :> "transactions"
          :> Capture "transactionId" TxId
          :> GetTransactionAPI
    ))
    contractId
    transactionId

  parseLink _ _ _ = do
    contractId <- string "contracts/"
      *> parseContractId
      <* string "/transactions"
    transactionId <- string "/transactions"
      *> parseTransactionId
      <* eof
    pure $ safeLink
      api
      (Proxy @("contracts"
            :> Capture "contractId" TxOutRef
            :> "transactions"
            :> Capture "transactionId" TxId
            :> GetTransactionAPI
      ))
      contractId
      transactionId


-- | /contracts/:contractId/transactions/:transactionId sup-API
type TransactionAPI = GetTransactionAPI
                 :<|> PutSignedTxAPI

-- | GET /contracts/:contractId/transactions/:transactionId sub-API
type GetTransactionAPI = Get '[JSON] GetTransactionResponse

type GetTransactionResponse = WithLink "previous" (WithLink "next" Tx)

type PutSignedTxAPI = ReqBody '[JSON] TextEnvelope :> PutAccepted '[JSON] NoContent

instance HasNamedLink Tx API "previous" where
  namedLink _ _ Tx{..} = guard (inputUtxo /= contractId) $> safeLink
    api
    (Proxy @("contracts"
          :> Capture "contractId" TxOutRef
          :> "transactions"
          :> Capture "transactionId" TxId
          :> GetTransactionAPI
    ))
    contractId
    (txId inputUtxo)

  parseLink _ _ _ = do
    contractId <- string "contracts/"
      *> parseContractId
      <* string "/transactions"
    transactionId <- string "/transactions"
      *> parseTransactionId
      <* eof
    pure $ safeLink
      api
      (Proxy @("contracts"
            :> Capture "contractId" TxOutRef
            :> "transactions"
            :> Capture "transactionId" TxId
            :> GetTransactionAPI
      ))
      contractId
      transactionId


instance HasNamedLink Tx API "next" where
  namedLink _ _ Tx{..} = safeLink api
    (Proxy @("contracts"
          :> Capture "contractId" TxOutRef
          :> "transactions"
          :> Capture "transactionId" TxId
          :> GetTransactionAPI
    ))
    contractId
    <$> consumingTx

  parseLink _ _ _ = do
    contractId <- string "contracts/"
      *> parseContractId
      <* string "/transactions"
    transactionId <- string "/transactions"
      *> parseTransactionId
      <* eof
    pure $ safeLink
      api
      (Proxy @("contracts"
            :> Capture "contractId" TxOutRef
            :> "transactions"
            :> Capture "transactionId" TxId
            :> GetTransactionAPI
      ))
      contractId
      transactionId


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

class HasNamedLink a api (name :: Symbol) where
  namedLink :: Proxy api -> Proxy name -> a -> Maybe Link
  parseLink :: Proxy api -> Proxy name -> Proxy a -> Parser Link

instance HasNamedLink a api name => HasNamedLink (WithLink name' a) api name where
  namedLink api' name = \case
    IncludeLink _ a -> namedLink api' name a
    OmitLink a -> namedLink api' name a
  parseLink api' name _ = parseLink api' name $ Proxy @a

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
      link = (symbolVal name,) <$> namedLink api name a
  toJSONWithLinks (OmitLink a) = toJSONWithLinks a

instance {-# OVERLAPPING #-} ToJSON a => ToJSONWithLinks a where
  toJSONWithLinks a = ([], toJSON a)

instance {-# OVERLAPPING #-}
  ( HasNamedLink a API name
  , FromJSONWithLinks a
  , KnownSymbol name
  ) => FromJSONWithLinks (WithLink name a) where
  fromJSONWithLinks (links, value) = do
    let mUri = lookup (symbolVal $ Proxy @name) links
    case mUri of
      Nothing -> OmitLink <$> fromJSONWithLinks (links, value)
      Just uri -> case runParser (parseLink api (Proxy @name) (Proxy @a)) () "" uri of
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
  ( HasNamedLink a API name
  , FromJSONWithLinks a
  , KnownSymbol name
  ) => FromJSON (WithLink name a) where
  parseJSON = fromJSONWithLinks <=< parseJSON'
    where
      parseJSON' = withObject "WithLink" \obj -> do
        value <- obj .: "resource"
        links <- obj .: "links"
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
