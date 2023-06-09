{-# LANGUAGE MultiParamTypeClasses #-}
module Spec.Marlowe.Semantics.Next.When.Choice
  ( Choice'(..)
  , onlyIndexedChoices
  ) where

import Data.Types.Isomorphic (Injective(..))
import Language.Marlowe.Core.V1.Semantics.Next (CanChoose(..), Indexed(..), IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics.Types (Action(Choice), Bound, ChoiceId, Contract, Environment, State)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.When (When'(indexedActions), reducibleToAWhen)

data Choice' = Choice' ChoiceId [Bound] IsMerkleizedContinuation deriving (Show,Eq,Ord)


instance Injective Choice' CanChoose where
   to (Choice' a b c) = CanChoose a b c
instance Injective CanChoose Choice'  where
   to (CanChoose a b c) =  Choice' a b c

onlyIndexedChoices :: Environment -> State -> Contract -> [Indexed Choice']
onlyIndexedChoices e s = maybe
      []
      (uncurry (onlyChoices e) . fmap indexedActions)
    . reducibleToAWhen e s

onlyChoices :: Environment -> State -> [Indexed (IsMerkleizedContinuation,Action)] -> [Indexed Choice']
onlyChoices _  _ [] = []
onlyChoices e  s ((Indexed caseIndex (isMerkleizedContinuation, Choice a b)) : xs)
  = Indexed caseIndex (Choice' a b isMerkleizedContinuation ) : onlyChoices e s xs
onlyChoices e  s (_: xs) = onlyChoices e s xs
