{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

{-| = ACTUS Generator

Given ACTUS contract terms a Marlowe contract is generated.

In 'genFsContract' risk factors are added to the Marlowe contract, i.e. will
be observed during the life time of the contract

-}

module Language.Marlowe.ACTUS.Generator.GeneratorFs (genFsContract, genFsContract') where

import Data.List (foldl')
import Data.Time (LocalTime)
import Data.Validation (Validation (..))
import Language.Marlowe
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsMarlowe)
import Language.Marlowe.ACTUS.Domain.ContractTerms (ContractTermsMarlowe, TermValidationError (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusOps (..), marloweFixedPoint, reduceContract, reduceValue)
import Language.Marlowe.ACTUS.Domain.Schedule (CashFlowPoly (..))
import Language.Marlowe.ACTUS.Generator.Analysis (genProjectedCashflows)
import Language.Marlowe.ACTUS.Generator.Generator (invoice)
import Language.Marlowe.ACTUS.Generator.MarloweCompat (timeToSlotNumber)
import Language.Marlowe.ACTUS.Model.Applicability (validateTerms)

-- | 'genFsContract' validatates the applicabilty of the contract terms in order
--  to genereate a Marlowe contract with risk factors observed at a given point
--  in time
genFsContract ::
  -- | Risk factors per event and time
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  -- | ACTUS contract terms
  ContractTermsMarlowe ->
  -- | Marlowe contract or applicabilty errors
  Validation [TermValidationError] Contract
genFsContract rf = fmap (genFsContract' rf) . validateTerms

genFsContract' ::
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  ContractTermsMarlowe ->
  Contract
genFsContract' rf ct =
  let cfs = genProjectedCashflows rf ct

      gen :: CashFlowPoly (Value Observation) -> Contract -> Contract
      gen CashFlowPoly {..} cont =
        let t = POSIXTime $ timeToSlotNumber cashPaymentDay
            a = reduceValue $ DivValue amount (Constant marloweFixedPoint)
         in reduceContract $
              If
                (_zero `ValueLT` a)
                ( invoice
                    "party"
                    "counterparty"
                    a
                    t
                    cont
                )
                ( If
                    (a `ValueLT` _zero)
                    ( invoice
                        "counterparty"
                        "party"
                        (NegValue a)
                        t
                        cont
                    )
                    cont
                )
   in foldl' (flip gen) Close $ reverse cfs
