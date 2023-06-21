{-# LANGUAGE LambdaCase #-}

module Spec.Marlowe.Semantics.Next.Contract
  ( hasValidEnvironement
  , isIrreducible
  , isNotClose
  , isReducible
  , isReducibleToClose
  ) where


import Language.Marlowe.Core.V1.Semantics (ReduceResult(ContractQuiescent), fixInterval, reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types (Contract(Close), Environment(..), IntervalResult(..), State)
import Spec.Marlowe.Semantics.Arbitrary ()

isIrreducible :: Environment -> State -> Contract -> Bool
isIrreducible environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      ContractQuiescent False _ _ _ _ -> True
      _otherwise ->  False

isNotClose :: Environment -> State -> Contract -> Bool
isNotClose _ _ Close = False
isNotClose _ _ _ = True

isReducible :: Environment -> State -> Contract -> Bool
isReducible environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      ContractQuiescent True _ _ _ _ -> True
      _otherwise ->  False


hasValidEnvironement :: Environment -> State -> Contract -> Bool
hasValidEnvironement environment state contract
  = case fixInterval (timeInterval environment) state of
      IntervalTrimmed e s ->
        case reduceContractUntilQuiescent e s contract of
          ContractQuiescent {} -> True
          _otherwise ->  False
      IntervalError _ -> False

isReducibleToClose :: Environment -> State -> Contract -> Bool
isReducibleToClose environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      ContractQuiescent _ _ _ _ Close -> True
      _otherwise ->  False


