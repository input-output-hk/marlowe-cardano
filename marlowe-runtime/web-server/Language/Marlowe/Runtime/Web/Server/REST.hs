{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the REST API.

module Language.Marlowe.Runtime.Web.Server.REST
  where

import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadContractHeaders)
import Servant
import Servant.Pagination

server :: ServerT API AppM
server = getContracts

getContracts
  :: Maybe (Ranges '["contractId"] ContractHeader)
  -> AppM (PaginatedResponse '["contractId"] ContractHeader)
getContracts ranges = do
  let
    range :: Range "contractId" TxOutRef
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
  startFrom <- case traverse fromDTO rangeValue of
    Nothing -> throwError err404
    Just startFrom -> pure startFrom
  loadContractHeaders startFrom rangeLimit rangeOffset rangeOrder >>= \case
    Nothing -> throwError err416
    Just headers -> addHeader (length headers) <$> returnRange range (toDTO headers)
