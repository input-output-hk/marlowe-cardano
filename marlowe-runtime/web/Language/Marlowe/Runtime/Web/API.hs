{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Web.API
  where

import Language.Marlowe.Runtime.Web.Types (ContractHeader)
import Servant
import Servant.Pagination

type API = AsumAPI
  '[ "contracts" :> ContractsAPI
   ]

type ContractsAPI = AsumAPI
  '[ GetContractsAPI
   ]

type GetContractsAPI =  PaginatedGet '["contractId"] ContractHeader

type PaginatedGet rangeFields resource
  =  Header "Range" (Ranges rangeFields resource)
  :> GetPartialContent '[JSON] (PaginatedResponse '["contractId"] resource)

type PaginatedResponse fields resource =
  Headers (Header "Total-Count" Int ': PageHeaders fields resource) [resource]

type family AsumAPI (apis :: [*]) where
  AsumAPI '[] = EmptyAPI
  AsumAPI '[api] = api
  AsumAPI (api ': apis) = api :<|> AsumAPI apis
