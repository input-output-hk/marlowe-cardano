{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Language.Marlowe.Runtime.Transaction.Burn where

import Cardano.Api (BabbageEraOnwards)
import Cardano.Api.Shelley (LedgerProtocolParameters)
import Control.Error (ExceptT)
import Language.Marlowe.Runtime.Transaction.Api (BurnError, BurnTxInEra, RoleTokenFilter)
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext (..))
import UnliftIO (MonadUnliftIO)

burnRoleTokens
  :: (MonadUnliftIO m)
  => BabbageEraOnwards era
  -> LedgerProtocolParameters era
  -> WalletContext
  -> RoleTokenFilter
  -> ExceptT BurnError m (BurnTxInEra era)
burnRoleTokens era protocol WalletContext{..} tokenFilter = undefined
