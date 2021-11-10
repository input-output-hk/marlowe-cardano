{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS Generator

Given ACTUS contract terms a Marlowe contract is generated.
With 'genStaticContract' the risk factors are all known at contract creation

-}

module Language.Marlowe.ACTUS.Generator.GeneratorStatic
  ( genStaticContract )
where

import Data.List as L (foldl')
import Data.Time (LocalTime)
import Data.Validation (Validation (..))
import Language.Marlowe (Contract (..), Slot (..), Value (..))
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactors)
import Language.Marlowe.ACTUS.Domain.ContractTerms (ContractTerms, TermValidationError (..))
import Language.Marlowe.ACTUS.Domain.Schedule (CashFlow (..))
import Language.Marlowe.ACTUS.Generator.Analysis (genProjectedCashflows)
import Language.Marlowe.ACTUS.Generator.Generator (invoice)
import Language.Marlowe.ACTUS.Generator.MarloweCompat (timeToSlotNumber)
import Language.Marlowe.ACTUS.Model.APPL.Applicability (validateTerms)

-- |'genStaticContract' validates the contract terms in order to generate a
-- Marlowe contract with risk factors known in advance. The contract therefore
-- only consists of transactions, i.e. 'Deposit' and 'Pay'
genStaticContract ::
     (EventType -> LocalTime -> RiskFactors)   -- ^ Risk factors per event and time
  -> ContractTerms                             -- ^ ACTUS contract terms
  -> Validation [TermValidationError] Contract -- ^ Marlowe contract or applicability errors
genStaticContract rf = fmap (genStaticContract' rf) . validateTerms

-- |Same as 'genStaticContract' without validation
genStaticContract' ::
     (EventType -> LocalTime -> RiskFactors)
  -> ContractTerms
  -> Contract
genStaticContract' rf ct =
  let cfs = genProjectedCashflows rf ct
      gen CashFlow {..}
        | amount == 0.0 = id
        | amount > 0.0 =
          invoice
            "party"
            "counterparty"
            (Constant $ round amount)
            (Slot $ timeToSlotNumber cashPaymentDay)
        | otherwise =
          invoice
            "counterparty"
            "party"
            (Constant $ round $ - amount)
            (Slot $ timeToSlotNumber cashPaymentDay)
   in foldl' (flip gen) Close $ reverse cfs
