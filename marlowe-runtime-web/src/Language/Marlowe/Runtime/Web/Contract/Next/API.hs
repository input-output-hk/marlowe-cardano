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

module Language.Marlowe.Runtime.Web.Contract.Next.API (nextApi, NextAPI) where

import Data.Time (UTCTime)
import Language.Marlowe.Core.V1.Next (Next)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Language.Marlowe.Runtime.Web.Adapter.Servant (OperationId)

import Language.Marlowe.Runtime.Web.Core.Party (Party)
import Servant (
  Description,
  Get,
  JSON,
  Proxy (..),
  QueryParam',
  QueryParams,
  Required,
  Summary,
  type (:>),
 )

nextApi :: Proxy NextAPI
nextApi = Proxy

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
