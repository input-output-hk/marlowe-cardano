{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts/:ContractId/next REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts.Next
  ( server
  ) where

import Control.Monad.Except (MonadError)
import Data.Time
import Language.Marlowe
import Language.Marlowe.Core.V1.Semantics.Next
import qualified Language.Marlowe.Core.V1.Semantics.Next as Semantics
import Language.Marlowe.Core.V1.Semantics.Next.CanReduce
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, loadContract)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (badRequest', badRequestWithErrorCode, notFoundWithErrorCode)
import Servant (throwError)
import Servant.Server

server :: TxOutRef ->  ServerT NextAPI ServerM
server = nextOverCardano'

nextOverCardano' ::  TxOutRef -> UTCTime -> UTCTime -> ServerM Next
nextOverCardano' contractId validityStart validityEnd
  = nextOverCardano contractId $ environment validityStart validityEnd

nextOverCardano ::  TxOutRef -> Environment -> ServerM Next
nextOverCardano contractId environment'
  = fromDTOThrow (badRequest' "Invalid contract id value") contractId
      >>= loadContract
      >>= whenNothingThrow (notFoundWithErrorCode "Contract not found" "contract_not_found")
      >>= whenNothingThrow (notFoundWithErrorCode "Contract Closed" "contract_closed")
          . notClosedContractMaybe . either toDTO toDTO
      >>= whenLeftThrow (\AmbiguousIntervalProvided -> badRequestWithErrorCode "Invalid Interval Provided" "ambiguous_interval_provided" )
          . (uncurry $ Semantics.next environment')



notClosedContractMaybe :: ContractState -> Maybe (State, Contract)
notClosedContractMaybe ContractState {state = Just state, currentContract = Just contract } = Just (state,contract)
notClosedContractMaybe _ = Nothing


whenNothingThrow :: MonadError e m => e -> Maybe a ->  m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: MonadError e m => (a -> e) -> Either a b ->  m b
whenLeftThrow toErr = either (throwError . toErr) pure

