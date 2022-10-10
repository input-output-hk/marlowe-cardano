{-# LANGUAGE NoImplicitPrelude #-}

module Language.Marlowe.Runtime.Plutus.V2.Scripts
  where

import Plutus.V2.Ledger.Api as PV2
import PlutusTx.Prelude (Bool(..), check, ($))

-- * Extracted from plutus-ledger

-- | Signature of an untyped validator script.
type ValidatorFn = BuiltinData -> BuiltinData -> BuiltinData -> ()

-- | Turns typed validator which accepts decoded data and returns a Bool
-- into a raw one which can be used on the chain. The raw wan accepts bytes
-- and throws an exception on failure.
{-# INLINEABLE wrapValidator #-}
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorFn
wrapValidator f d r c =
  check $ f datum redeemer context
 where
  datum = unsafeFromBuiltinData d
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c

-- | Signature of an untyped minting policy script.
type MintingPolicyFn = BuiltinData -> BuiltinData -> ()

-- | Turns typed function into a minting policy which can be used
-- on the chain.
{-# INLINEABLE wrapMintingPolicy #-}
wrapMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyFn
wrapMintingPolicy f r c =
  PlutusTx.Prelude.check (f redeemer context)
 where
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c

