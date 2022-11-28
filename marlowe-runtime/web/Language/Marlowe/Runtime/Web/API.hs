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

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.

module Language.Marlowe.Runtime.Web.API
  where

import Control.Lens hiding ((.=))
import Control.Monad (guard)
import Data.Aeson
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
import Data.Typeable (Typeable)
import GHC.Base (Symbol)
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Runtime.Web.Types
import Servant
import Servant.Pagination

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

instance HasNamedLink ContractHeader API "contract" where
  namedLink _ _ ContractHeader{..} = Just $ safeLink
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

-- | /contracts/:contractId sup-API
type ContractAPI = GetContractAPI
              :<|> PutSignedTxAPI
              :<|> "transactions" :> TransactionsAPI
              :<|> "withdrawals" :> WithdrawalsAPI

-- | GET /contracts/:contractId sub-API
type GetContractAPI = Get '[JSON] GetContractResponse

type GetContractResponse = WithLink "withdrawals" (WithLink "transactions" ContractState)

instance HasNamedLink ContractState API "transactions" where
  namedLink _ _ ContractState{..} = guard (status == Confirmed) $> safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI))
    contractId

instance HasNamedLink ContractState API "withdrawals" where
  namedLink _ _ ContractState{..} = guard (status == Confirmed) $> safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> "withdrawals" :> GetWithdrawalsAPI))
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

-- | /contracts/:contractId/withdrawals sup-API
type WithdrawalsAPI = GetWithdrawalsAPI

-- | GET /contracts/:contractId/transactions sup-API
type GetWithdrawalsAPI = PaginatedGet '["withdrawalId"] GetWithdrawalsResponse

type GetWithdrawalsResponse = Withdrawal

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

instance HasNamedLink a api name => HasNamedLink (WithLink name' a) api name where
  namedLink api' name = \case
    IncludeLink _ a -> namedLink api' name a
    OmitLink a -> namedLink api' name a

data WithLink (name :: Symbol) a where
  IncludeLink :: Proxy name -> a -> WithLink name a
  OmitLink :: a -> WithLink name a

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
