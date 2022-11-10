{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Web
  where

import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Web
import Monad (AppM, loadContractHeaders)
import Servant
import Servant.Pagination

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = getContracts

type GetContractsHeaders =
  Header "Total-Count" Int ': PageHeaders '["contractId"] ContractHeader

getContracts
  :: Maybe (Ranges '["contractId"] ContractHeader)
  -> AppM (Headers GetContractsHeaders [ContractHeader])
getContracts ranges = loadContractHeaders range >>= \case
  Nothing -> throwError err416
  Just headers -> addHeader (length headers) <$> returnRange range headers
  where
    range :: Range "contractId" TxOutRef
    range = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
