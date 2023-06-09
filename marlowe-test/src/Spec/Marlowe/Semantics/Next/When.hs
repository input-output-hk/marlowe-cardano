

module Spec.Marlowe.Semantics.Next.When
  ( When'(..)
  , indexedCaseActions
  , reducibleToAWhen
  ) where


import Data.Bifunctor (first)
import Data.List.Index
import Language.Marlowe.Core.V1.Semantics (ReduceResult(ContractQuiescent), reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Next (CaseIndex(CaseIndex), Indexed(..), IsMerkleizedContinuation(..))
import Language.Marlowe.Core.V1.Semantics.Types (Action, Case(..), Contract(When), Environment, State, Timeout)
import Spec.Marlowe.Semantics.Arbitrary ()

data When' = When' {indexedActions :: [Indexed (IsMerkleizedContinuation,Action)], timeout::Timeout}

reducibleToAWhen :: Environment -> State -> Contract -> Maybe (State , When')
reducibleToAWhen environment' state contract
  = case reduceContractUntilQuiescent environment' state contract of
      ContractQuiescent _ _ _ newState (When caseActions timeout' _) -> Just (newState, When' (indexedCaseActions caseActions) timeout')
      _otherwise ->  Nothing


indexedCaseActions :: [Case Contract] -> [Indexed (IsMerkleizedContinuation,Action)]
indexedCaseActions
  = fmap (uncurry Indexed . first (CaseIndex . fromIntegral))
  . indexed
  . (getAction' <$>)


getAction' :: Case a -> (IsMerkleizedContinuation,Action)
getAction' (Case action _)           = (IsMerkleizedContinuation False,action)
getAction' (MerkleizedCase action _) = (IsMerkleizedContinuation True,action)
