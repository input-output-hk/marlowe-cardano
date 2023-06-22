{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts/:ContractId/next REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts.Next
  ( server
  ) where

import Control.Monad.Except (MonadError)
import Data.Time (UTCTime)
import Language.Marlowe.Core.V1.Next (Next)
import qualified Language.Marlowe.Core.V1.Next as Semantics
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Environment, State)

import Language.Marlowe.Runtime.Web (ContractState(ContractState, currentContract, state), NextAPI, TxOutRef)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO), fromDTOThrow)
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, loadContract)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (badRequest', badRequest'', notFoundWithErrorCode)
import Servant (throwError)
import Servant.Server (HasServer(ServerT))


server :: Language.Marlowe.Runtime.Web.TxOutRef ->  ServerT Language.Marlowe.Runtime.Web.NextAPI ServerM
server = nextOverCardano'

nextOverCardano' ::  Language.Marlowe.Runtime.Web.TxOutRef -> UTCTime -> UTCTime -> ServerM Next
nextOverCardano' contractId validityStart validityEnd
  = nextOverCardano contractId $ Semantics.mkEnvironment validityStart validityEnd

nextOverCardano ::  Language.Marlowe.Runtime.Web.TxOutRef -> Environment -> ServerM Next
nextOverCardano contractId environment'
  = fromDTOThrow (badRequest' "Invalid contract id value") contractId
      >>= loadContract
      >>= whenNothingThrow (notFoundWithErrorCode "Contract not found" "contract_not_found")
      >>= whenNothingThrow (notFoundWithErrorCode "Contract Closed" "contract_closed")
          . notClosedContractMaybe . either toDTO toDTO
      >>= whenLeftThrow (badRequest'' "Invalid Interval Provided" "invalid_interval")
          . (uncurry $ Semantics.next environment')


notClosedContractMaybe :: Language.Marlowe.Runtime.Web.ContractState -> Maybe (State, Contract)
notClosedContractMaybe Language.Marlowe.Runtime.Web.ContractState {state = Just state, currentContract = Just contract } = Just (state,contract)
notClosedContractMaybe _ = Nothing


whenNothingThrow :: MonadError e m => e -> Maybe a ->  m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: MonadError e m => (a -> e) -> Either a b ->  m b
whenLeftThrow toErr = either (throwError . toErr) pure

