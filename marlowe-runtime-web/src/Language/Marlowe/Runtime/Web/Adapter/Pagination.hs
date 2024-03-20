{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
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

module Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedGet, PaginatedResponse) where

import Servant.Pagination (
  PageHeaders,
  Ranges,
 )

import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject)
import Servant (
  GetPartialContent,
  Header,
  Headers,
  JSON,
  type (:>),
 )

-- | Helper type for defining generic paginated GET endpoints
type PaginatedGet rangeFields resource =
  Header "Range" (Ranges rangeFields resource)
    :> GetPartialContent '[JSON] (PaginatedResponse rangeFields resource)

-- | Helper type for describing the response type of generic paginated APIs
type PaginatedResponse fields resource =
  Headers (Header "Total-Count" Int ': PageHeaders fields resource) (ListObject resource)
