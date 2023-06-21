

module Spec.Marlowe.Semantics.Next.Contract.When.Notify
  ( firstNotifyTrueIndex
  ) where

import Language.Marlowe.Core.V1.Next.Indexed (CaseIndex, Indexed(..))
import Language.Marlowe.Core.V1.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics (evalObservation)
import Language.Marlowe.Core.V1.Semantics.Types (Action(Notify), Case, Contract, Environment, State)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Contract.When (indexedCaseActions)


firstNotifyTrueIndex :: Environment -> State -> [Case Contract] -> Maybe CaseIndex
firstNotifyTrueIndex e s = firstNotifyTrueIndex' e s . indexedCaseActions

firstNotifyTrueIndex' :: Environment -> State -> [Indexed (IsMerkleizedContinuation,Action)] -> Maybe CaseIndex
firstNotifyTrueIndex' _ _ [] = Nothing
firstNotifyTrueIndex' e s ((Indexed caseIndex (_,Notify observation)) : _) | evalObservation e s observation = Just caseIndex
firstNotifyTrueIndex' e s (_: xs) = firstNotifyTrueIndex' e s xs

