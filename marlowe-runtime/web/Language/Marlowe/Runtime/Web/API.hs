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
import Data.Aeson
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
  namedLink _ _ ContractHeader{..} = safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> GetContractAPI))
    contractId

-- | POST /contracts sub-API
type PostContractsAPI
  =  ReqBody '[JSON] PostContractsRequest
  :> PostTxAPI (Post '[JSON] PostContractsResponse)

type PostContractsResponse = WithLink "contract" CreateTxBody

instance HasNamedLink CreateTxBody API "contract" where
  namedLink _ _ CreateTxBody{..} = safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> GetContractAPI))
    contractId

-- | /contracts/:contractId sup-API
type ContractAPI = GetContractAPI
              :<|> "transactions" :> TransactionsAPI

-- | GET /contracts/:contractId sub-API
type GetContractAPI = Get '[JSON] GetContractResponse

type GetContractResponse = WithLink "transactions" ContractState

instance HasNamedLink ContractState API "transactions" where
  namedLink _ _ ContractState{..} = safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> "transactions" :> GetTransactionsAPI))
    contractId

-- | /contracts/:contractId/transactions sup-API
type TransactionsAPI = GetTransactionsAPI

-- | GET /contracts/:contractId/transactions sup-API
type GetTransactionsAPI = PaginatedGet '["transactionId"] TxHeader

-- | Helper type for defining generic paginated GET endpoints
type PaginatedGet rangeFields resource
  =  Header "Range" (Ranges rangeFields resource)
  :> GetPartialContent '[JSON] (PaginatedResponse rangeFields resource)

-- | Helper type for describing the response type of generic paginated APIs
type PaginatedResponse fields resource =
  Headers (Header "Total-Count" Int ': PageHeaders fields resource) [resource]

type PostTxAPI api
  =  Header' '[Required, Strict] "X-Change-Address" Address
  :> Header "X-Address" (CommaList Address)
  :> Header "X-Collateral-UTxO" (CommaList TxOutRef)
  :> api

class HasNamedLink a api (name :: Symbol) where
  namedLink :: Proxy api -> Proxy name -> a -> Link

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
  toJSONWithLinks (IncludeLink name a) = (link : links, value)
    where
      (links, value) = toJSONWithLinks a
      link = (symbolVal name, namedLink api name a)
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
