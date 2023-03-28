{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Matcher
  where

import Cardano.Api.ChainSync.ClientPipelined (N(..), Nat(..))
import Control.Exception (catch, throw)
import Data.Kind (Type)
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec

class Matcher a matcher | matcher -> a where
  match :: matcher -> a -> Expectation

data TrivialMatcher = TrivialMatcher

instance Matcher () TrivialMatcher where
  match _ _ = pure ()

newtype EqMatcher a = EqMatcher a

instance (Eq a, Show a) => Matcher a (EqMatcher a) where
  match (EqMatcher a) a' = a' `shouldBe` a

newtype CustomMatcher a = CustomMatcher (a -> Expectation)

instance Matcher a (CustomMatcher a) where
  match (CustomMatcher matcher) a = matcher a

data VariantMatcher s (matchers :: [Type]) where
  VariantMatcherNil :: VariantMatcher s '[]
  VariantMatcherConsEmpty
    :: VariantMatcher s matchers
    -> VariantMatcher s (matcher ': matchers)
  VariantMatcherCons
    :: Matcher a matcher
    => matcher
    -> (s -> Maybe a)
    -> VariantMatcher s matchers
    -> VariantMatcher s (matcher ': matchers)

instance Show s => Matcher s (VariantMatcher s matchers) where
  match VariantMatcherNil s = expectationFailure $ "Unexpected result " <> show s
  match (VariantMatcherConsEmpty matchers) s =
    -- Give next matcher a change to match.
    match matchers s
  match (VariantMatcherCons matcher proj matchers) s = case proj s of
    Nothing -> match matchers s
    Just a ->
      -- If this matcher fails, give the next matcher a chance to pass.
      match matcher a `catch` \(failure :: HUnitFailure) -> do
        -- If the next matcher also fails, throw the first failure.
        match matchers s `catch` \(_ :: HUnitFailure) -> throw failure

instance Semigroup (VariantMatcher s matchers) where
  VariantMatcherNil <> VariantMatcherNil =
    VariantMatcherNil
  VariantMatcherConsEmpty matchers <> VariantMatcherConsEmpty matchers' =
    VariantMatcherConsEmpty $ matchers <> matchers'
  VariantMatcherConsEmpty matchers <> VariantMatcherCons proj matcher matchers' =
    VariantMatcherCons proj matcher $ matchers <> matchers'
  VariantMatcherCons proj matcher matchers <> VariantMatcherConsEmpty matchers' =
    VariantMatcherCons proj matcher $ matchers <> matchers'
  VariantMatcherCons _ _ matchers <> VariantMatcherCons proj matcher matchers' =
    VariantMatcherCons proj matcher $ matchers <> matchers'

class KnownMatchers (matchers :: [Type]) where
  emptyMatchers :: VariantMatcher s matchers

instance KnownMatchers '[] where
  emptyMatchers = VariantMatcherNil

instance KnownMatchers matchers => KnownMatchers (matcher ': matchers) where
  emptyMatchers = VariantMatcherConsEmpty emptyMatchers

class InjectMatcher matcher s (n :: N) (matchers :: [Type]) where
  injectMatcher :: Matcher a matcher => Nat n -> matcher -> (s -> Maybe a) -> VariantMatcher s matchers

instance
  ( InjectMatcher matcher s n matchers
  ) => InjectMatcher matcher s ('S n) (matcher' ': matchers) where
  injectMatcher (Succ n) matcher proj = VariantMatcherConsEmpty $ injectMatcher n matcher proj

instance
  ( KnownMatchers matchers
  ) => InjectMatcher matcher s 'Z (matcher ': matchers) where
  injectMatcher _ matcher proj = VariantMatcherCons matcher proj emptyMatchers
