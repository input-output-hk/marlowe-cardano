{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

{-| = ACTUS Generator

Given ACTUS contract terms a Marlowe contract is generated.

In 'genFsContract' risk factors are added to the Marlowe contract, i.e. will
be observed during the life time of the contract

-}

module Language.Marlowe.ACTUS.Generator.GeneratorFs
  ( genFsContract )
where

import           Data.Time                                       (LocalTime)
import           Data.Validation                                 (Validation (..))
import           Language.Marlowe                                (Contract (..), Observation (..), Slot (..),
                                                                  Value (..))
import           Language.Marlowe.ACTUS.Domain.BusinessEvents    (EventType (..), RiskFactorsMarlowe)
import           Language.Marlowe.ACTUS.Domain.ContractTerms     (ContractTermsMarlowe, TermValidationError (..))
import           Language.Marlowe.ACTUS.Domain.Ops               (ActusOps (..))
import           Language.Marlowe.ACTUS.Domain.Schedule          (CashFlowPoly (..))
import           Language.Marlowe.ACTUS.Generator.Analysis       (genProjectedCashflows)
import           Language.Marlowe.ACTUS.Generator.Generator      (invoice)
import           Language.Marlowe.ACTUS.Generator.MarloweCompat  (timeToSlotNumber)
import           Language.Marlowe.ACTUS.Model.APPL.Applicability (validateTerms)

-- |'genFsContract' validatate the applicabilty of the contract terms in order
-- to genereate a Marlowe contract with risk factors observed at a given point
-- in time
genFsContract ::
     (EventType -> LocalTime -> RiskFactorsMarlowe)   -- ^ Risk factors per event and time
  -> ContractTermsMarlowe                             -- ^ ACTUS contract terms
  -> Validation [TermValidationError] Contract -- ^ Marlowe contract or applicabilty errors
genFsContract rf = fmap (genFsContract' rf) . validateTerms

genFsContract' ::
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  ContractTermsMarlowe ->
  Contract
genFsContract' rf ct =
  let projectedCashflows = genProjectedCashflows rf ct

      gen :: CashFlowPoly (Value Observation) -> Contract -> Contract
      gen CashFlowPoly {..} cont =
        let timeout = Slot $ timeToSlotNumber cashPaymentDay
         in If
              (ValueGT amount _zero)
              (invoice "party" "counterparty" amount timeout cont)
              ( If
                  (ValueLT amount _zero)
                  (invoice "counterparty" "party" (NegValue amount) timeout cont)
                  cont
              )
   in foldr gen Close projectedCashflows
