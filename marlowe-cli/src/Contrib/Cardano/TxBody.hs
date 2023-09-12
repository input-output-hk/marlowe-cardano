{-# LANGUAGE GADTs #-}

module Contrib.Cardano.TxBody (exUnits) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Data.Map.Strict as Map

-- | Total the execution units in a transaction.
exUnits
  :: C.TxBody era
  -- ^ The transaction body.
  -> ExUnits
  -- ^ The execution units.
exUnits (CS.ShelleyTxBody C.ShelleyBasedEraBabbage _ _ (C.TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . Map.elems $ redeemers
exUnits (CS.ShelleyTxBody C.ShelleyBasedEraAlonzo _ _ (C.TxBodyScriptData _ _ (Redeemers redeemers)) _ _) =
  mconcat . fmap snd . Map.elems $ redeemers
exUnits _ = mempty
