module Language.Marlowe.FindInputs(getAllInputs) where

import Data.Bifunctor (Bifunctor (second), bimap)
import Data.Maybe (catMaybes)
import Data.SBV (ThmResult)
import Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import Language.Marlowe.Semantics (TransactionInput)
import Language.Marlowe.SemanticsTypes (Case (..), Contract (..), Observation (..))
import Ledger (Slot)

expandCase :: Case Contract -> [Case Contract]
expandCase (Case ac con)        = [Case ac c | c <- expandContract con]
expandCase (MerkleizedCase _ _) = []

expandCases :: [Case Contract] -> [[Case Contract]]
expandCases [] = []
expandCases (firstCase:restOfCases) =
       [c:restOfCases | c <- expandCase firstCase]
    ++ [firstCase:ec | ec <- expandCases restOfCases]

expandContract :: Contract -> [Contract]
expandContract Close = [Assert FalseObs Close]
expandContract (Pay pa pa' to va con) = [Pay pa pa' to va c | c <- expandContract con]
expandContract (If ob con con') = [If ob c con' | c <- expandContract con]
                               ++ [If ob con c | c <- expandContract con']
expandContract (When cas sl con) = [When cas sl c | c <- expandContract con]
                                ++ [When c sl con | c <- expandCases cas]
expandContract (Let vi va con) = [Let vi va c | c <- expandContract con]
expandContract (Assert _ con) = expandContract con

getInputs :: Contract -> IO (Either (ThmResult, Contract) (Maybe (Slot, [TransactionInput])))
getInputs c = bimap (\tr -> (tr, c)) (fmap (\(s, t, _) -> (s, t))) <$> warningsTrace c

-- | Uses static analysis to obtain a list of "unit tests" (lists of transactions) that
-- | cover the different branches of the given contract. If static analysis fails
-- | it returns a tuple that includes the error by the solver and the offending
-- | extension of the contract
getAllInputs :: Contract -> IO (Either (ThmResult, Contract) [(Slot, [TransactionInput])])
getAllInputs c = second catMaybes . sequence <$> mapM getInputs (expandContract c)

