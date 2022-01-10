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

import Data.List (foldl')
import Data.Time (LocalTime)
import Data.Validation (Validation (..))
import Language.Marlowe
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsMarlowe)
import Language.Marlowe.ACTUS.Domain.ContractTerms (ContractTermsMarlowe, TermValidationError (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusOps (..), marloweFixedPoint)
import Language.Marlowe.ACTUS.Domain.Schedule (CashFlowPoly (..))
import Language.Marlowe.ACTUS.Generator.Analysis (genProjectedCashflows)
import Language.Marlowe.ACTUS.Generator.Generator (invoice)
import Language.Marlowe.ACTUS.Generator.MarloweCompat (timeToSlotNumber)
import Language.Marlowe.ACTUS.Model.Applicability (validateTerms)

-- |'genFsContract' validatates the applicabilty of the contract terms in order
-- to genereate a Marlowe contract with risk factors observed at a given point
-- in time
genFsContract ::
     (EventType -> LocalTime -> RiskFactorsMarlowe) -- ^ Risk factors per event and time
  -> ContractTermsMarlowe                           -- ^ ACTUS contract terms
  -> Validation [TermValidationError] Contract      -- ^ Marlowe contract or applicabilty errors
genFsContract rf = fmap (genFsContract' rf) . validateTerms

genFsContract' ::
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  ContractTermsMarlowe ->
  Contract
genFsContract' rf ct =
  let cfs = genProjectedCashflows rf ct

      gen :: CashFlowPoly (Value Observation) -> Contract -> Contract
      gen CashFlowPoly {..} cont =
        let t = Slot $ timeToSlotNumber cashPaymentDay
            a = reduce $ DivValue amount (Constant marloweFixedPoint)
         in case a of
              Constant x
                | x > 0 ->
                  invoice
                    "party"
                    "counterparty"
                    a
                    t
                    cont
              Constant x
                | x < 0 ->
                  invoice
                    "counterparty"
                    "party"
                    (NegValue a)
                    t
                    cont
              _ ->
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

reduceObservation :: Observation -> Observation
reduceObservation (AndObs a b)  = AndObs (reduceObservation a) (reduceObservation b)
reduceObservation (OrObs a b)   = OrObs (reduceObservation a) (reduceObservation b)
reduceObservation (NotObs a)    = NotObs (reduceObservation a)
reduceObservation (ValueGE a b) = ValueGE (reduce a) (reduce b)
reduceObservation (ValueGT a b) = ValueGT (reduce a) (reduce b)
reduceObservation (ValueLE a b) = ValueLE (reduce a) (reduce b)
reduceObservation (ValueLT a b) = ValueLT (reduce a) (reduce b)
reduceObservation (ValueEQ a b) = ValueEQ (reduce a) (reduce b)
reduceObservation x             = x

reduce :: Value Observation -> Value Observation
reduce (ChoiceValue i) = ChoiceValue i
reduce (UseValue i) = UseValue i
reduce (Constant i) = Constant i

reduce (AddValue (Constant x) (Constant y)) = Constant $ x+y
reduce (AddValue x y) = reduce $ AddValue (reduce x) (reduce y)

reduce (SubValue (Constant x) (Constant y)) = Constant $ x-y
reduce (SubValue x y) = reduce $ SubValue (reduce x) (reduce y)

reduce (MulValue (Constant x) (Constant y)) = Constant $ x*y
reduce (MulValue x y) = reduce $ MulValue (reduce x) (reduce y)

-- same as in Semantics
reduce (DivValue (Constant n) (Constant d)) = Constant $
  if n == 0 || d == 0 then 0
  else let (q, r) = n `quotRem` d
           ar = abs r * 2
           ad = abs d
        in if ar < ad then q -- reminder < 1/2
           else if ar > ad then q + signum n * signum d -- reminder > 1/2
                else let -- reminder == 1/2
                         qIsEven = q `div` 2 == 0
                      in if qIsEven then q else q + signum n * signum d

reduce (DivValue x y) = reduce $ DivValue (reduce x) (reduce y)

reduce (NegValue (Constant x)) = Constant $ -x
reduce (NegValue v) = reduce $ NegValue (reduce v)

reduce (Cond (ValueGT (Constant x) (Constant y)) a _) | x > y = reduce a
reduce (Cond (ValueGT (Constant _) (Constant _)) _ b) = reduce b
reduce (Cond (ValueLT (Constant x) (Constant y)) a _) | x < y = reduce a
reduce (Cond (ValueLT (Constant _) (Constant _)) _ b) = reduce b

reduce (Cond o a b) = reduce $ Cond (reduceObservation o) (reduce a) (reduce b)
reduce x = x
