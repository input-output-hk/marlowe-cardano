{-# LANGUAGE MultiParamTypeClasses #-}



module Spec.Marlowe.Semantics.Next.When.Deposit
  ( EvaluatedDeposit(..)
  , evaluateDeposits
  ) where

import Data.Types.Isomorphic (Injective(..), Iso)
import Language.Marlowe.Core.V1.Semantics (evalValue)
import Language.Marlowe.Core.V1.Semantics.Next (CanDeposit(..), Indexed(..), IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Action(Deposit), Contract, Environment, Party, State, Token)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.When (When'(indexedActions), reducibleToAWhen)


data EvaluatedDeposit = EvaluatedDeposit Party AccountId Token Integer IsMerkleizedContinuation deriving (Show,Eq,Ord)

instance Injective EvaluatedDeposit CanDeposit where
   to (EvaluatedDeposit a b c d e) = CanDeposit a b c d e
instance Injective CanDeposit EvaluatedDeposit  where
   to (CanDeposit a b c d e) =  EvaluatedDeposit a b c d e

instance Iso EvaluatedDeposit CanDeposit


evaluateDeposits :: Environment -> State -> Contract -> [Indexed EvaluatedDeposit]
evaluateDeposits e s = maybe
      []
      (uncurry (evaluateDepositsQuantities e) . fmap indexedActions)
    . reducibleToAWhen e s

evaluateDepositsQuantities :: Environment -> State -> [Indexed (IsMerkleizedContinuation,Action)] -> [Indexed EvaluatedDeposit]
evaluateDepositsQuantities _  _ [] = []
evaluateDepositsQuantities e  s ((Indexed caseIndex (isMerkleizedContinuation,Deposit a b c quantityObs)) : xs)
  = Indexed caseIndex (EvaluatedDeposit a b c (evalValue e s quantityObs) isMerkleizedContinuation) : evaluateDepositsQuantities e s xs
evaluateDepositsQuantities e  s (_: xs) = evaluateDepositsQuantities e s xs

