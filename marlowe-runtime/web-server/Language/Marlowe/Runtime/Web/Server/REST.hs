{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Web.Server.REST
  where

import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadContractHeaders)
import Servant
import Servant.Pagination

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = getContracts

getContracts
  :: Maybe (Ranges '["contractId"] ContractHeader)
  -> AppM (PaginatedResponse '["contractId"] ContractHeader)
getContracts ranges = loadContractHeaders range >>= \case
  Nothing -> throwError err416
  Just headers -> addHeader (length headers) <$> returnRange range headers
  where
    range :: Range "contractId" TxOutRef
    range = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
