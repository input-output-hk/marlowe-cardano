{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Big hammer, but helps
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans             #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}

-- | Ledger independent representation of Money
--
-- Intended for qualified import
--
-- > import Language.Marlowe.Core.V1.Semantics.Money (Money)
-- > import qualified Language.Marlowe.Core.V1.Semantics.Money as Money
module Language.Marlowe.Core.V1.Semantics.Money (
    Money(..)
  , singleton
  , toList
  ) where

import PlutusTx.Prelude hiding (toList, zero)
import qualified Prelude as Haskell

import qualified Data.List as Haskell
import GHC.Generics (Generic)
import PlutusTx.AssocMap (Map)
import PlutusTx.Lift (makeLift)

import qualified PlutusTx.AssocMap as Map

{-------------------------------------------------------------------------------
  Basic API
-------------------------------------------------------------------------------}

-- | Ledger-independent representation of money
newtype Money t = Money (Map t Integer)
  deriving stock (Haskell.Show,Generic)

singleton :: t -> Integer -> Money t
{-# INLINEABLE singleton #-}
singleton t v = Money (Map.singleton t v)

toList :: Money t -> [(t, Integer)]
{-# INLINEABLE toList #-}
toList (Money m) = Map.toList m

{-------------------------------------------------------------------------------
  Haskell Prelude instances

  We jump through a few hoops to make sure that the Haskell instances only
  have Haskell superclass constraints.
-------------------------------------------------------------------------------}

normalizeHaskell :: forall t. Haskell.Ord t => Map t Integer -> Map t Integer
normalizeHaskell = Map.fromList . go . Map.toList
  where
    go :: [(t, Integer)] -> [(t, Integer)]
    go = Haskell.sort . Haskell.filter ((/= 0) . snd)

instance Haskell.Ord t => Haskell.Eq (Money t) where
  Money m == Money m' = normalizeHaskell m Haskell.== normalizeHaskell m'

instance Haskell.Ord t => Haskell.Semigroup (Money t) where
  Money m <> Money m' = Money $
      Map.fromList $
        go (Map.toList $ normalizeHaskell m)
           (Map.toList $ normalizeHaskell m')
    where
      go :: [(t, Integer)] -> [(t, Integer)] -> [(t, Integer)]
      go [] ys = ys
      go xs [] = xs
      go ((x,v):xs) ((y,v'):ys)
        | x Haskell.< y = (x,v)    : go        xs  ((y,v'):ys )
        | x Haskell.> y = (y,v')   : go ((x,v):xs)         ys
        | otherwise     = (x,v+v') : go xs ys

instance Haskell.Ord t => Haskell.Monoid (Money t) where
  mempty = Money Map.empty

{-------------------------------------------------------------------------------
  Plutus Prelude instances
-------------------------------------------------------------------------------}

normalizePlutus :: forall t. Ord t => Map t Integer -> Map t Integer
{-# INLINEABLE normalizePlutus #-}
normalizePlutus = Map.fromList . go . Map.toList
  where
    go :: [(t, Integer)] -> [(t, Integer)]
    go = sort . filter ((/= 0) . snd)

instance Ord t => Eq (Money t) where
  {-# INLINEABLE (==) #-}
  Money m == Money m' = normalizePlutus m == normalizePlutus m'

instance Ord t => Semigroup (Money t) where
  {-# INLINEABLE (<>) #-}
  Money m <> Money m' = Money $
      Map.fromList $
        go (Map.toList $ normalizePlutus m)
           (Map.toList $ normalizePlutus m')
    where
      go :: [(t, Integer)] -> [(t, Integer)] -> [(t, Integer)]
      go [] ys = ys
      go xs [] = xs
      go ((x,v):xs) ((y,v'):ys)
        | x < y = (x,v)    : go        xs  ((y,v'):ys )
        | x > y = (y,v')   : go ((x,v):xs)         ys
        | otherwise     = (x,v+v') : go xs ys

instance Ord t => Monoid (Money t) where
  {-# INLINEABLE mempty #-}
  mempty = Money Map.empty

makeLift ''Money
