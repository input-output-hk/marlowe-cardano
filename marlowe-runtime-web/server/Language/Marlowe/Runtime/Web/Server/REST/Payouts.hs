{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /payouts REST API.
module Language.Marlowe.Runtime.Web.Server.REST.Payouts where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page (..), PayoutFilter (..))
import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.DTO (FromDTO (..), ToDTO (..), fromPaginationRange)
import Language.Marlowe.Runtime.Web.Server.Monad
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (badRequest', rangeNotSatisfiable')
import Servant
import Servant.Pagination

server :: ServerT PayoutsAPI ServerM
server =
  get

get
  :: [TxOutRef]
  -> [AssetId]
  -> Bool
  -> Maybe (Ranges '["payoutId"] PayoutRef)
  -> ServerM (PaginatedResponse '["payoutId"] PayoutRef)
get contractIds roleTokens unclaimed ranges = do
  let range :: Range "payoutId" TxOutRef
      range = fromMaybe (getDefaultRange (Proxy @PayoutRef)) $ extractRange =<< ranges
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
  let pFilter = PayoutFilter unclaimed (Set.fromList contractIds') (Set.fromList roleTokens')
  loadPayouts pFilter range' >>= \case
    Nothing -> throwError $ rangeNotSatisfiable' "Initial payout ID not found"
    Just Page{..} -> do
      let payouts = toDTO items
      addHeader totalCount . fmap ListObject <$> returnRange range payouts
