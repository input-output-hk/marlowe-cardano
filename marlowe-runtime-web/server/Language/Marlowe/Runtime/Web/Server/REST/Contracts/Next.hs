{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the /contracts/:ContractId/next REST API.
module Language.Marlowe.Runtime.Web.Server.REST.Contracts.Next (
  server,
) where

import Control.Monad.Except (MonadError)
import Data.Time (UTCTime)
import Language.Marlowe.Core.V1.Next (Next)
import qualified Language.Marlowe.Core.V1.Next as Semantics
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Environment, State)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Language.Marlowe.Runtime.Core.Api hiding (Contract, State)
import Language.Marlowe.Runtime.Web (ContractState (ContractState, currentContract, state), NextAPI, Party, TxOutRef)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO (toDTO), fromDTOThrow)

import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, loadContract)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (badRequest', badRequest'', notFoundWithErrorCode)
import Servant (throwError)
import Servant.Server (HasServer (ServerT))

server :: TxOutRef -> ServerT NextAPI ServerM
server = nextOverCardano'

nextOverCardano' :: TxOutRef -> UTCTime -> UTCTime -> [Party] -> ServerM Next
nextOverCardano' contractId validityStart validityEnd parties =
  do
    a <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
    b <- fromDTOThrow (badRequest' "Invalid parties") (nonEmpty parties)
    nextOverCardano (Semantics.mkEnvironment validityStart validityEnd) a b

nextOverCardano :: Environment -> ContractId -> Maybe (NonEmpty Semantics.Party) -> ServerM Next
nextOverCardano environment contractId parties =
  loadContract contractId
    >>= whenNothingThrow (notFoundWithErrorCode "Contract not found" "contractNotFound")
    >>= whenNothingThrow (notFoundWithErrorCode "Contract Closed" "contractClosed")
      . notClosedContractMaybe
      . either toDTO toDTO
    >>= whenLeftThrow (badRequest'' "Invalid Interval" "invalidInterval")
      . fmap (Semantics.filterByParties parties)
      . (uncurry $ Semantics.next environment)

notClosedContractMaybe :: ContractState -> Maybe (State, Contract)
notClosedContractMaybe ContractState{state = Just state, currentContract = Just contract} = Just (state, contract)
notClosedContractMaybe _ = Nothing

whenNothingThrow :: (MonadError e m) => e -> Maybe a -> m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: (MonadError e m) => (a -> e) -> Either a b -> m b
whenLeftThrow toErr = either (throwError . toErr) pure
