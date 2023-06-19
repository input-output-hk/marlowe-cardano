{-# LANGUAGE MultiParamTypeClasses #-}


module Spec.Marlowe.Semantics.Next.Contract.When.Deposit
  ( EvaluatedDeposit(..)
  , evaluateDeposits
  , hasIdenticalEvaluatedDeposits
  , hasNoIdenticalEvaluatedDeposits
  ) where

import Data.List (nubBy)
import Data.Types.Isomorphic (Injective(..), Iso)
import Language.Marlowe.Core.V1.Semantics (evalValue)
import Language.Marlowe.Core.V1.Semantics.Next.Applicables.CanDeposit (CanDeposit(..))
import Language.Marlowe.Core.V1.Semantics.Next.Indexed (Indexed(..))
import Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics.Types
  (AccountId, Action(Deposit), Case, Contract, Environment, Party, State, Token)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Contract.When (indexedCaseActions)



data EvaluatedDeposit
    = EvaluatedDeposit Party AccountId Token Integer IsMerkleizedContinuation
    deriving (Show,Eq,Ord)

instance Injective EvaluatedDeposit CanDeposit where
   to (EvaluatedDeposit a b c d e) = CanDeposit a b c d e
instance Injective CanDeposit EvaluatedDeposit  where
   to (CanDeposit a b c d e) =  EvaluatedDeposit a b c d e

instance Iso EvaluatedDeposit CanDeposit

hasIdenticalEvaluatedDeposits :: Environment -> State -> [Case Contract] -> Bool
hasIdenticalEvaluatedDeposits e s = not . hasNoIdenticalEvaluatedDeposits e s

hasNoIdenticalEvaluatedDeposits :: Environment -> State -> [Case Contract] -> Bool
hasNoIdenticalEvaluatedDeposits e s c =
    let xs = evaluateDeposits e s c
    in nubBy(\ (Indexed _ a) (Indexed _ b) -> a == b) xs == xs

evaluateDeposits :: Environment -> State -> [Case Contract] -> [Indexed EvaluatedDeposit]
evaluateDeposits e s = evaluateDepositsQuantities e s . indexedCaseActions


evaluateDepositsQuantities :: Environment -> State -> [Indexed (IsMerkleizedContinuation,Action)] -> [Indexed EvaluatedDeposit]
evaluateDepositsQuantities _  _ [] = []
evaluateDepositsQuantities e s ((Indexed caseIndex (isMerkleizedContinuation, Deposit a b c quantityObs)) : xs)
  = Indexed caseIndex (EvaluatedDeposit a b c (evalValue e s quantityObs) isMerkleizedContinuation) : evaluateDepositsQuantities e s xs
evaluateDepositsQuantities e s (_: xs) = evaluateDepositsQuantities e s xs

