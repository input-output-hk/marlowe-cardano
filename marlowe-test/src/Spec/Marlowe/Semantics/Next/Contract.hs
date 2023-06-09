{-# LANGUAGE LambdaCase #-}

module Spec.Marlowe.Semantics.Next.Contract
  ( hasValidEnvironement
  , isEmptyWhenNonTimedOut
  , isIrreducible
  , isNotClose
  , isReducible
  , isReducibleToClose
  ) where


import Language.Marlowe.Core.V1.Semantics
  (ReduceResult(ContractQuiescent, RRAmbiguousTimeIntervalError), reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types (Contract(Close), Environment, State, isNotTimedOut)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.When (When'(..), reducibleToAWhen)


isIrreducible :: Environment -> State -> Contract -> Bool
isIrreducible environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      ContractQuiescent False _ _ _ _ -> True
      _otherwise ->  False

isNotClose :: Environment -> State -> Contract -> Bool
isNotClose _' _ Close = False
isNotClose _ _ _ = True

isReducible :: Environment -> State -> Contract -> Bool
isReducible environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      ContractQuiescent True _ _ _ _ -> True
      _otherwise ->  False


hasValidEnvironement :: Environment -> State -> Contract -> Bool
hasValidEnvironement environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      RRAmbiguousTimeIntervalError -> False
      _otherwise -> True

isReducibleToClose :: Environment -> State -> Contract -> Bool
isReducibleToClose environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      ContractQuiescent _ _ _ _ Close -> True
      _otherwise ->  False

isEmptyWhenNonTimedOut :: Environment -> State -> Contract -> Bool
isEmptyWhenNonTimedOut e s
  = (\case
      Just (_,When' [] timeout') | isNotTimedOut timeout' e   -> True
      _otherwise -> False )
  . reducibleToAWhen e s
