{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

-- | This module defines a server for the /contracts/:ContractId/next REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts.Next
  ( server
  ) where

import Control.Monad.Except (MonadError)
import Data.Time
import Language.Marlowe
import Language.Marlowe.Core.V1.Semantics.Next
import qualified Language.Marlowe.Core.V1.Semantics.Next as Semantics
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadContract)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError (badRequest', notFoundWithErrorCode)
import Servant (throwError)
import Servant.Server

server :: TxOutRef ->  ServerT NextAPI AppM
server = nextOverCardano'

nextOverCardano' ::  TxOutRef -> UTCTime -> UTCTime -> AppM Next
nextOverCardano' contractId validityStart validityEnd
  = nextOverCardano contractId $ environment validityStart validityEnd

nextOverCardano ::  TxOutRef -> Environment -> AppM Next
nextOverCardano contractId environment'
  = fromDTOThrow (badRequest' "Invalid contract id value") contractId
      >>= loadContract
      >>= whenNothingThrow (notFoundWithErrorCode "Contract not found" "contract_not_found")
      >>= whenNothingThrow (notFoundWithErrorCode "Contract Closed" "contract_closed")
          . notClosedContractMaybe . either toDTO toDTO
      >>= whenLeftThrow (\AmbiguousIntervalProvided -> notFoundWithErrorCode "Contract Suspended" "ambiguous_interval_provided" )
          . (uncurry $ Semantics.next environment')



notClosedContractMaybe :: ContractState -> Maybe (State, Contract)
notClosedContractMaybe ContractState {state = Just state, currentContract = Just contract } = Just (state,contract)
notClosedContractMaybe _ = Nothing


whenNothingThrow :: MonadError e m => e -> Maybe a ->  m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: MonadError e m => (a -> e) -> Either a b ->  m b
whenLeftThrow toErr = either (throwError . toErr) pure

