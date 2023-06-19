

module Spec.Marlowe.Semantics.Next.Contract.When.Notify
  ( areOnlyFalsifiedNotifies
  , atLeastOneNotifyTrue
  , falsifiedNotifies
  , firstNotifyTrueIndex
  ) where

import Data.Maybe (isJust)
import Language.Marlowe.Core.V1.Semantics (evalObservation)
import Language.Marlowe.Core.V1.Semantics.Next.Indexed (CaseIndex, Indexed(..))
import Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics.Types (Action(Notify), Contract, Environment, State)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Contract.When (When'(indexedActions), reducibleToAWhen)

areOnlyFalsifiedNotifies :: Environment -> State -> Contract -> Bool
areOnlyFalsifiedNotifies e s c
  = maybe False
    ((\(newState,actions) ->
      (not . null $ actions)
      && actions == falsifiedNotifies e newState actions ) . fmap indexedActions)
    (reducibleToAWhen e s c)

atLeastOneNotifyTrue :: Environment -> State -> Contract -> Bool
atLeastOneNotifyTrue e s c
  = maybe False
    ((isJust . uncurry (firstNotifyTrueIndex' e)) . fmap indexedActions)
    (reducibleToAWhen e s c)

firstNotifyTrueIndex :: Environment -> State -> Contract -> Maybe CaseIndex
firstNotifyTrueIndex e s c = reducibleToAWhen e s c >>= uncurry (firstNotifyTrueIndex' e) . fmap indexedActions

firstNotifyTrueIndex' :: Environment -> State -> [Indexed (IsMerkleizedContinuation,Action)] -> Maybe CaseIndex
firstNotifyTrueIndex' _  _ [] = Nothing
firstNotifyTrueIndex' e  s ((Indexed caseIndex (_,Notify observation)) : _) | evalObservation e s observation = Just caseIndex
firstNotifyTrueIndex' e  s (_: xs) = firstNotifyTrueIndex' e s xs

falsifiedNotifies :: Environment -> State -> [Indexed (IsMerkleizedContinuation,Action)] -> [Indexed (IsMerkleizedContinuation,Action)]
falsifiedNotifies _  _ [] = []
falsifiedNotifies e  s (x@(Indexed _ (_,Notify observation)) : xs) | not . evalObservation e s $ observation = x : falsifiedNotifies e s xs
falsifiedNotifies e  s (_: xs) = falsifiedNotifies e s xs
