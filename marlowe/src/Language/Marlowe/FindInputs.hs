module Language.Marlowe.FindInputs(getAllInputs) where

import Data.Bifunctor (Bifunctor (second), bimap)
import Data.Maybe (catMaybes)
import Data.SBV (ThmResult)
import Language.Marlowe.Analysis.FSSemantics (onlyAssertionsWithState)
import Language.Marlowe.Core.V1.Semantics (TransactionInput)
import Language.Marlowe.Core.V1.Semantics.Types (Case (..), Contract (..), Observation (..), Token)
import Plutus.V1.Ledger.Api (POSIXTime)

-- | Removes all the assertions from a contract
removeAsserts :: Contract Token -> Contract Token
removeAsserts = go
  where go :: Contract Token -> Contract Token
        go Close                  = Close
        go (Pay pa pa' to va con) = Pay pa pa' to va (go con)
        go (If ob con con')       = If ob (go con) (go con')
        go (When cas sl con)      = When (map goCase cas) sl (go con)
        go (Let vi va con)        = Let vi va (go con)
        go (Assert _ con)         = con

        goCase :: Case (Contract Token) -> Case (Contract Token)
        goCase (Case ac con)           = Case ac (go con)
        goCase mc@(MerkleizedCase _ _) = mc

expandCase :: Case (Contract Token) -> [Case (Contract Token)]
expandCase (Case ac con)        = [Case ac c | c <- expandContract con]
expandCase (MerkleizedCase _ _) = []

expandCases :: [Case (Contract Token)] -> [[Case (Contract Token)]]
expandCases [] = []
expandCases (firstCase:restOfCases) =
       [c:restOfCases | c <- expandCase firstCase]
    ++ [firstCase:ec | ec <- expandCases restOfCases]

expandContract :: Contract Token -> [Contract Token]
expandContract Close = [Assert FalseObs Close]
expandContract (Pay pa pa' to va con) = [Pay pa pa' to va c | c <- expandContract con]
expandContract (If ob con con') = [If ob c con' | c <- expandContract con]
                               ++ [If ob con c | c <- expandContract con']
expandContract (When cas sl con) = [When cas sl c | c <- expandContract con]
                                ++ [When c sl con | c <- expandCases cas]
expandContract (Let vi va con) = [Let vi va c | c <- expandContract con]
expandContract (Assert _ con) = expandContract con

getInputs :: Contract Token -> IO (Either (ThmResult, Contract Token) (Maybe (POSIXTime, [TransactionInput])))
getInputs c = bimap (\tr -> (tr, c)) (fmap (\(s, t, _) -> (s, t))) <$> onlyAssertionsWithState c Nothing

-- | Uses static analysis to obtain a list of "unit tests" (lists of transactions) that
-- | cover the different branches of the given contract. If static analysis fails
-- | it returns a tuple that includes the error by the solver and the offending
-- | extension of the contract
getAllInputs :: Contract Token -> IO (Either (ThmResult, Contract Token) [(POSIXTime, [TransactionInput])])
getAllInputs c = second catMaybes . sequence <$> mapM getInputs (expandContract (removeAsserts c))

