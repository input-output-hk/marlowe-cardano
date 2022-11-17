{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.

module Language.Marlowe.Runtime.Web.API
  where

import Language.Marlowe.Runtime.Web.Types
import Servant
import Servant.Pagination

api :: Proxy API
api = Proxy

-- | The REST API of the Marlowe Runtime
type API = AsumAPI
  '[ "contracts" :> ContractsAPI
   ]

-- | /contracts sub-API
type ContractsAPI = AsumAPI
  '[ GetContractsAPI
   , Capture "contractId" TxOutRef :> ContractAPI
   ]

-- | /contracts/:contractId sup-API
type ContractAPI = AsumAPI
  '[ GetContractAPI
   ]

-- | GET /contracts sub-API
type GetContractsAPI =  PaginatedGet '["contractId"] ContractHeader

-- | GET /contracts/:contractId sub-API
type GetContractAPI = Get '[JSON] ContractState

-- | Helper type for defining generic paginated GET endpoints
type PaginatedGet rangeFields resource
  =  Header "Range" (Ranges rangeFields resource)
  :> GetPartialContent '[JSON] (PaginatedResponse '["contractId"] resource)

-- | Helper type for describing the response type of generic paginated APIs
type PaginatedResponse fields resource =
  Headers (Header "Total-Count" Int ': PageHeaders fields resource) [resource]

-- | Type family for folding the :<|> combinator over a list of APIs.
type family AsumAPI (apis :: [*]) where
  AsumAPI '[] = EmptyAPI
  AsumAPI '[api] = api
  AsumAPI (api ': apis) = api :<|> AsumAPI apis
