{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.

module Language.Marlowe.Runtime.Web.API
  where

import Language.Marlowe.Runtime.Web.Types
import Servant
import Servant.Pagination

api :: Proxy API
api = Proxy

-- | The REST API of the Marlowe Runtime
type API = "contracts" :> ContractsAPI

-- | /contracts sub-API
type ContractsAPI = GetContractsAPI
               :<|> Capture "contractId" TxOutRef :> ContractAPI

-- | GET /contracts sub-API
type GetContractsAPI = PaginatedGet '["contractId"] GetContractsResponse

type GetContractsResponse = WithLink "contract" ContractHeader

instance HasNamedLink ContractHeader API "contract" where
  namedLink _ _ ContractHeader{..} = safeLink
    api
    (Proxy @("contracts" :> Capture "contractId" TxOutRef :> GetContractAPI))
    contractId

-- | /contracts/:contractId sup-API
type ContractAPI = GetContractAPI

-- | GET /contracts/:contractId sub-API
type GetContractAPI = Get '[JSON] ContractState

-- | Helper type for defining generic paginated GET endpoints
type PaginatedGet rangeFields resource
  =  Header "Range" (Ranges rangeFields resource)
  :> GetPartialContent '[JSON] (PaginatedResponse rangeFields resource)

-- | Helper type for describing the response type of generic paginated APIs
type PaginatedResponse fields resource =
  Headers (Header "Total-Count" Int ': PageHeaders fields resource) [resource]
