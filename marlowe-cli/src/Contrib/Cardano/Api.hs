{-# LANGUAGE BlockArguments #-}

module Contrib.Cardano.Api (
  lovelaceToInt,
  lovelaceFromInt,
  lppPParamsL,
  ledgerProtVerToPlutusMajorProtocolVersion,
) where

import Cardano.Api.Ledger (Coin (Coin))
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.BaseTypes qualified as Ledger
import Lens.Micro qualified as Micro
import PlutusLedgerApi.Common qualified as P

lovelaceToInt :: Coin -> Int
lovelaceToInt (Coin l) = fromInteger l

lovelaceFromInt :: Int -> Coin
lovelaceFromInt = Coin . toInteger

lppPParamsL :: Micro.Getting f (C.LedgerProtocolParameters era) (Ledger.PParams (C.ShelleyLedgerEra era))
lppPParamsL = Micro.to \(C.LedgerProtocolParameters pp) -> pp

ledgerProtVerToPlutusMajorProtocolVersion :: Ledger.ProtVer -> P.MajorProtocolVersion
ledgerProtVerToPlutusMajorProtocolVersion (Ledger.ProtVer m _) = P.MajorProtocolVersion . fromInteger $ Ledger.getVersion m
