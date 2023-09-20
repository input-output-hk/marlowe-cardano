{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Otherwise we get a complaint about the 'fromIntegral' call in the generated instance of 'Integral' for 'Ada'
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- | Functions for working with 'Ada' in Template Haskell.
module Plutus.V1.Ledger.Ada (
  Ada (..),
  adaSymbol,
  adaToken,
  getAda,

  -- * Constructors
  adaOf,
  adaValueOf,
  fromValue,
  lovelaceOf,
  lovelaceValueOf,
  toValue,

  -- * Num operations
  divide,

  -- * Etc.
  isZero,
) where

import Prelude qualified as Haskell

import Data.Fixed

import Codec.Serialise.Class (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Tagged
import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (CurrencySymbol (..), TokenName (..), Value)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (divide)
import PlutusTx.Prelude qualified as P
import Prettyprinter (Pretty)

{-# INLINEABLE adaSymbol #-}

-- | The 'CurrencySymbol' of the 'Ada' currency.
adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol emptyByteString

{-# INLINEABLE adaToken #-}

-- | The 'TokenName' of the 'Ada' currency.
adaToken :: TokenName
adaToken = TokenName emptyByteString

-- | ADA, the special currency on the Cardano blockchain. The unit of Ada is Lovelace, and
--   1M Lovelace is one Ada.
--   See note [Currencies] in 'Ledger.Validation.Value.TH'.
newtype Ada = Lovelace {getLovelace :: Integer}
  deriving (Haskell.Enum)
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype
    ( Eq
    , Ord
    , Haskell.Num
    , AdditiveSemigroup
    , AdditiveMonoid
    , AdditiveGroup
    , MultiplicativeSemigroup
    , MultiplicativeMonoid
    , Haskell.Integral
    , Haskell.Real
    , Serialise
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )
  deriving (Pretty) via (Tagged "Lovelace:" Integer)

instance Haskell.Semigroup Ada where
  Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Semigroup Ada where
  Lovelace a1 <> Lovelace a2 = Lovelace (a1 + a2)

instance Haskell.Monoid Ada where
  mempty = Lovelace 0

instance Monoid Ada where
  mempty = Lovelace 0

makeLift ''Ada

{-# INLINEABLE getAda #-}

-- | Get the amount of Ada (the unit of the currency Ada) in this 'Ada' value.
getAda :: Ada -> Micro
getAda (Lovelace i) = MkFixed i

{-# INLINEABLE toValue #-}

-- | Create a 'Value' containing only the given 'Ada'.
toValue :: Ada -> Value
toValue (Lovelace i) = Value.singleton adaSymbol adaToken i

{-# INLINEABLE fromValue #-}

-- | Get the 'Ada' in the given 'Value'.
fromValue :: Value -> Ada
fromValue v = Lovelace (Value.valueOf v adaSymbol adaToken)

{-# INLINEABLE lovelaceOf #-}

-- | Create 'Ada' representing the given quantity of Lovelace (the unit of the currency Ada).
lovelaceOf :: Integer -> Ada
lovelaceOf = Lovelace

{-# INLINEABLE adaOf #-}

-- | Create 'Ada' representing the given quantity of Ada (1M Lovelace).
adaOf :: Micro -> Ada
adaOf (MkFixed x) = Lovelace x

{-# INLINEABLE lovelaceValueOf #-}

-- | A 'Value' with the given amount of Lovelace (the currency unit).
--
--   @lovelaceValueOf == toValue . lovelaceOf@
lovelaceValueOf :: Integer -> Value
lovelaceValueOf = Value.singleton adaSymbol adaToken

{-# INLINEABLE adaValueOf #-}

-- | A 'Value' with the given amount of Ada (the currency unit).
--
--   @adaValueOf == toValue . adaOf@
adaValueOf :: Micro -> Value
adaValueOf (MkFixed x) = Value.singleton adaSymbol adaToken x

{-# INLINEABLE divide #-}

-- | Divide one 'Ada' value by another.
divide :: Ada -> Ada -> Ada
divide (Lovelace a) (Lovelace b) = Lovelace (P.divide a b)

{-# INLINEABLE isZero #-}

-- | Check whether an 'Ada' value is zero.
isZero :: Ada -> Bool
isZero (Lovelace i) = i == 0
