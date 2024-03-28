{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /payouts REST API.
module Language.Marlowe.Runtime.Web.Payout.Server (server) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page (..), PayoutFilter (..))
import Language.Marlowe.Runtime.Web.Adapter.Links (
  WithLink (IncludeLink),
 )
import Language.Marlowe.Runtime.Web.Adapter.Pagination (
  PaginatedResponse,
 )
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  ListObject (ListObject),
 )
import Language.Marlowe.Runtime.Web.Adapter.Server.ApiError (badRequest', notFound', rangeNotSatisfiable')
import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (FromDTO (..), ToDTO (..), fromDTOThrow, fromPaginationRange)
import Language.Marlowe.Runtime.Web.Adapter.Server.Monad (
  ServerM,
  loadPayout,
  loadPayouts,
 )
import Language.Marlowe.Runtime.Web.Payout.API (
  GetPayoutResponse,
  GetPayoutsResponse,
  PayoutHeader,
  PayoutStatus (..),
  PayoutsAPI,
 )

import Language.Marlowe.Runtime.Web.Core.Asset (AssetId)
import Language.Marlowe.Runtime.Web.Core.Tx (TxOutRef)
import Servant (
  HasServer (ServerT),
  Proxy (Proxy),
  addHeader,
  throwError,
  type (:<|>) ((:<|>)),
 )
import Servant.Pagination (
  ExtractRange (extractRange),
  HasPagination (getDefaultRange),
  Range,
  Ranges,
  returnRange,
 )

server :: ServerT PayoutsAPI ServerM
server =
  get
    :<|> getOne

get
  :: [TxOutRef]
  -> [AssetId]
  -> Maybe PayoutStatus
  -> Maybe (Ranges '["payoutId"] GetPayoutsResponse)
  -> ServerM (PaginatedResponse '["payoutId"] GetPayoutsResponse)
get contractIds roleTokens status ranges = do
  let range :: Range "payoutId" TxOutRef
      range = fromMaybe (getDefaultRange (Proxy @PayoutHeader)) $ extractRange =<< ranges
  range' <- maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  contractIds' <-
    traverse
      ( \contractId -> maybe (throwError $ badRequest' $ "Invalid contractId value " <> show contractId) pure $ fromDTO contractId
      )
      contractIds
  roleTokens' <-
    traverse
      (\assetId -> maybe (throwError $ badRequest' $ "Invalid contractId value " <> show assetId) pure $ fromDTO assetId)
      roleTokens
  let status' =
        status <&> \case
          Available -> False
          Withdrawn -> True
  let pFilter = PayoutFilter status' (Set.fromList contractIds') (Set.fromList roleTokens')
  loadPayouts pFilter range' >>= \case
    Nothing -> throwError $ rangeNotSatisfiable' "Initial payout ID not found"
    Just Page{..} -> do
      let payouts = toDTO items
      let response = IncludeLink (Proxy @"payout") <$> payouts
      addHeader totalCount . fmap ListObject <$> returnRange range response

getOne :: TxOutRef -> ServerM GetPayoutResponse
getOne payoutId = do
  payoutId' <- fromDTOThrow (badRequest' "Invalid payout id value") payoutId
  loadPayout payoutId' >>= \case
    Nothing -> throwError $ notFound' "Payout not found"
    Just result ->
      pure $
        IncludeLink (Proxy @"contract") $
          IncludeLink (Proxy @"transaction") $
            IncludeLink (Proxy @"withdrawal") $
              toDTO result
