{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Spec.Marlowe.Semantics.Next.Contract.When.Choice
  ( Choice'(..)
  , hasOnlyChoicesWithNoBounds
  , onlyIndexedChoices
  ) where

import Data.Types.Isomorphic (Injective(..))
import qualified Language.Marlowe.Core.V1.Next.Applicables.CanChoose as Semantics
import Language.Marlowe.Core.V1.Next.Indexed (Indexed(..), getIndexedValue)
import Language.Marlowe.Core.V1.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics.Types (Action(Choice), Bound, Case, ChoiceId, Contract, Environment, State)
import Spec.Marlowe.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Contract.When (indexedCaseActions)

data Choice'
    = Choice'
      { choiceId :: ChoiceId
      , bounds :: [Bound]
      , isMerkleizedContinuation :: IsMerkleizedContinuation
      }
  deriving (Show,Eq,Ord)


instance Injective Choice' Semantics.CanChoose where
   to (Choice' a b c) = Semantics.CanChoose a b c

instance Injective Semantics.CanChoose Choice'  where
   to (Semantics.CanChoose a b c) = Choice' a b c


hasOnlyChoicesWithNoBounds :: Environment -> State -> [Case Contract] -> Bool
hasOnlyChoicesWithNoBounds e s c
  = let choices = onlyIndexedChoices e s c
    in (not.null $ choices) && all (null . bounds . getIndexedValue) choices

onlyIndexedChoices :: Environment -> State -> [Case Contract]  -> [Indexed Choice']
onlyIndexedChoices e s = onlyChoices e s . indexedCaseActions

onlyChoices :: Environment -> State -> [Indexed (IsMerkleizedContinuation,Action)] -> [Indexed Choice']
onlyChoices _  _ [] = []
onlyChoices e  s ((Indexed caseIndex (isMerkleized, Choice a b)) : xs)
  = Indexed caseIndex (Choice' a b isMerkleized) : onlyChoices e s xs
onlyChoices e  s (_: xs) = onlyChoices e s xs
