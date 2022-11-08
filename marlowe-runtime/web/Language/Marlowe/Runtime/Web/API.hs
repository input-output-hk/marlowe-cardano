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
  :> GetPartialContent
      '[JSON]
      ( Headers
          (  Header "Total-Count" Int
          ': PageHeaders '["contractId"] resource
          )
          [resource]
      )

type family AsumAPI (apis :: [*]) where
  AsumAPI '[] = EmptyAPI
  AsumAPI '[api] = api
  AsumAPI (api ': apis) = api :<|> AsumAPI apis
