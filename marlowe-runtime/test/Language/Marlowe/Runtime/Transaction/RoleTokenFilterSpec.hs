{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.Runtime.Transaction.RoleTokenFilterSpec where

import qualified Data.Set as Set
import Data.Word (Word8)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Transaction.Api (
  IsToken (..),
  RoleTokenFilter' (..),
  evalRoleTokenFilter,
  optimizeRoleTokenFilter,
 )
import Language.Marlowe.Runtime.Transaction.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck

spec :: Spec
spec = modifyMaxSuccess (* 100) do
  prop "and-annulment" \a -> a `RoleTokensAnd` RoleTokenFilterNone .=== RoleTokenFilterNone
  prop "or-annulment" \a -> a `RoleTokensOr` RoleTokenFilterAny .=== RoleTokenFilterAny
  prop "and-identity" \a -> a `RoleTokensAnd` RoleTokenFilterAny .=== a
  prop "or-identity" \a -> a `RoleTokensOr` RoleTokenFilterNone .=== a
  prop "and-idempotent" \a -> a `RoleTokensAnd` a .=== a
  prop "or-idempotent" \a -> a `RoleTokensOr` a .=== a
  prop "and-complement" \a -> a `RoleTokensAnd` RoleTokensNot a .=== RoleTokenFilterNone
  prop "or-complement" \a -> a `RoleTokensOr` RoleTokensNot a .=== RoleTokenFilterAny
  prop "double-negation" \a -> RoleTokensNot (RoleTokensNot a) .=== a
  prop "and-commutative" \a b -> a `RoleTokensAnd` b .=== b `RoleTokensAnd` a
  prop "or-commutative" \a b -> a `RoleTokensOr` b .=== b `RoleTokensOr` a
  prop "distributive" \a b c -> a `RoleTokensAnd` (b `RoleTokensOr` c) .=== (a `RoleTokensAnd` b) `RoleTokensOr` (a `RoleTokensAnd` c)
  prop "inverse-distributive" \a b c -> (a `RoleTokensOr` b) `RoleTokensAnd` (a `RoleTokensOr` c) .=== a `RoleTokensOr` (b `RoleTokensAnd` c)
  prop "and-absorption-1" \a b -> a `RoleTokensAnd` (a `RoleTokensOr` b) .=== a
  prop "and-absorption-2" \a b -> a `RoleTokensAnd` (RoleTokensNot a `RoleTokensOr` b) .=== a `RoleTokensAnd` b
  prop "or-absorption-1" \a b -> a `RoleTokensOr` (a `RoleTokensAnd` b) .=== a
  prop "or-absorption-2" \a b -> a `RoleTokensOr` (RoleTokensNot a `RoleTokensAnd` b) .=== a `RoleTokensOr` b
  prop "and-de-morgan" \a b -> RoleTokensNot (a `RoleTokensAnd` b) .=== RoleTokensNot a `RoleTokensOr` RoleTokensNot b
  prop "or-de-morgan" \a b -> RoleTokensNot (a `RoleTokensOr` b) .=== RoleTokensNot a `RoleTokensAnd` RoleTokensNot b
  prop "consensus" \a b c ->
    let ab = a `RoleTokensAnd` b
        a'c = RoleTokensNot a `RoleTokensAnd` c
        bc = b `RoleTokensAnd` c
        lhs = ab `RoleTokensOr` a'c `RoleTokensOr` bc
        rhs = ab `RoleTokensOr` a'c
     in lhs .=== rhs
  prop "null-contracts" $ RoleTokenFilterByContracts mempty .=== RoleTokenFilterNone
  prop "null-policyIds" $ RoleTokenFilterByPolicyIds mempty .=== RoleTokenFilterNone
  prop "null-tokens" $ RoleTokenFilterByTokens mempty .=== RoleTokenFilterNone
  prop "and-contracts" \c1 c2 ->
    RoleTokensAnd (RoleTokenFilterByContracts c1) (RoleTokenFilterByContracts c2)
      .=== RoleTokenFilterByContracts (Set.intersection c1 c2)
  prop "and-policyIds" \p1 p2 ->
    RoleTokensAnd (RoleTokenFilterByPolicyIds p1) (RoleTokenFilterByPolicyIds p2)
      .=== RoleTokenFilterByPolicyIds (Set.intersection p1 p2)
  prop "and-tokens" \t1 t2 ->
    RoleTokensAnd (RoleTokenFilterByTokens t1) (RoleTokenFilterByTokens t2)
      .=== RoleTokenFilterByTokens (Set.intersection t1 t2)
  prop "and-policyIds-tokens" \p t ->
    RoleTokensAnd (RoleTokenFilterByPolicyIds p) (RoleTokenFilterByTokens t)
      .=== RoleTokenFilterByTokens (Set.filter (flip Set.member p . tokenPolicyId) t)
  prop "or-contracts" \c1 c2 ->
    RoleTokensOr (RoleTokenFilterByContracts c1) (RoleTokenFilterByContracts c2)
      .=== RoleTokenFilterByContracts (Set.union c1 c2)
  prop "or-policyIds" \p1 p2 ->
    RoleTokensOr (RoleTokenFilterByPolicyIds p1) (RoleTokenFilterByPolicyIds p2)
      .=== RoleTokenFilterByPolicyIds (Set.union p1 p2)
  prop "or-tokens" \t1 t2 ->
    RoleTokensOr (RoleTokenFilterByTokens t1) (RoleTokenFilterByTokens t2)
      .=== RoleTokenFilterByTokens (Set.union t1 t2)
  prop "or-policyIds-tokens" \p t ->
    RoleTokensOr (RoleTokenFilterByPolicyIds p) (RoleTokenFilterByTokens t)
      .=== RoleTokensOr
        (RoleTokenFilterByPolicyIds p)
        (RoleTokenFilterByTokens $ Set.filter (not . flip Set.member p . tokenPolicyId) t)
  prop "optimize" \f ->
    let f' = optimizeRoleTokenFilter f in counterexample ("Optimized: " <> show f') $ f' .=== f

(.===) :: RoleTokenFilter -> RoleTokenFilter -> ContractId -> Token -> Property
(.===) a b c t = evalRoleTokenFilter a c t === evalRoleTokenFilter b c t

infix 4 .===

type RoleTokenFilter = RoleTokenFilter' ContractId PolicyId Token

newtype ContractId = ContractId Word8
  deriving newtype (Show, Eq, Ord, Arbitrary)

newtype PolicyId = PolicyId Word8
  deriving newtype (Show, Eq, Ord, Arbitrary)

newtype TokenName = TokenName Word8
  deriving newtype (Show, Eq, Ord, Arbitrary)

data Token = Token PolicyId TokenName
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary Token where
  arbitrary = Token <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance IsToken Token PolicyId where
  tokenPolicyId (Token p _) = p
