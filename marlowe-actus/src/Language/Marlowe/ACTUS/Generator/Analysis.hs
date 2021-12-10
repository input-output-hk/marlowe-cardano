{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-| = ACTUS Analysis

Given ACTUS contract terms cashflows can be projected from the predefined risk factors.
The cash flows can be used to generate the payments in a Marlowe contract.

-}

module Language.Marlowe.ACTUS.Generator.Analysis
  ( genProjectedCashflows
  , genProjectedPayoffs
  )
where

import           Control.Applicative                              ((<|>))
import           Control.Monad.Reader                             (runReader)
import           Data.Functor                                     ((<&>))
import           Data.List                                        (groupBy)
import           Data.Maybe                                       (fromMaybe, isNothing)
import           Data.Sort                                        (sortOn)
import           Data.Time                                        (LocalTime)
import           Language.Marlowe.ACTUS.Domain.BusinessEvents     (EventType (..), RiskFactorsPoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractState      (ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms      (CT (..), ContractTermsPoly (..))
import           Language.Marlowe.ACTUS.Domain.Ops                (RoleSignOps (..), ScheduleOps (..), YearFractionOps)
import           Language.Marlowe.ACTUS.Domain.Schedule           (CashFlowPoly (..), ShiftedDay (..), calculationDay,
                                                                   paymentDay)
import           Language.Marlowe.ACTUS.Model.ContractSchedule    (maturity, schedule)
import           Language.Marlowe.ACTUS.Model.Payoff              (CtxPOF (CtxPOF), payoff)
import           Language.Marlowe.ACTUS.Model.StateInitialization (initializeState)
import           Language.Marlowe.ACTUS.Model.StateTransition     (CtxSTF (CtxSTF), stateTransition)

-- |'genProjectedCashflows' generates a list of projected cashflows for
-- given contract terms and provided risk factors. The function returns
-- an empty list, if building the initial state given the contract terms
-- fails or in case there are no cash flows.
genProjectedCashflows :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
  (EventType -> LocalTime -> RiskFactorsPoly a) -- ^ Risk factors as a function of event type and time
  -> ContractTermsPoly a                        -- ^ ACTUS contract terms
  -> [CashFlowPoly a]                           -- ^ List of projected cash flows
genProjectedCashflows rf ct =
  let genCashflow (ev, t, ContractStatePoly {..}, am) =
        CashFlowPoly
          { tick = 0,
            cashContractId = contractId ct,
            cashParty = "party",
            cashCounterParty = "counterparty",
            cashPaymentDay = paymentDay t,
            cashCalculationDay = calculationDay t,
            cashEvent = ev,
            amount = am,
            notional = nt,
            currency = fromMaybe "unknown" (settlementCurrency ct)
          }
   in sortOn cashPaymentDay . fmap genCashflow . genProjectedPayoffs rf $ ct

genProjectedPayoffs :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
  (EventType -> LocalTime -> RiskFactorsPoly a)        -- ^ Risk factors as a function of event type and time
  -> ContractTermsPoly a                               -- ^ ACTUS contract terms
  -> [(EventType, ShiftedDay, ContractStatePoly a, a)] -- ^ List of projected payoffs
genProjectedPayoffs rf ct@ContractTermsPoly {..} =
  let -- schedules

      schedules =
        filter filtersSchedules . postProcessSchedules . sortOn (paymentDay . snd) $
          concatMap scheduleEvent eventTypes
        where
          eventTypes = [IED, MD, IP, IPFX, IPFL, RR, RRF, PR, PRF, IPCB, IPCI, PRD, TD, SC, DV, XD, STD]
          scheduleEvent ev = (ev,) <$> schedule ev ct

      -- states

      states =
        filter filtersStates . tail $ runReader (sequence $ scanl apply st0 schedules) ctx
        where
          apply prev (ev', t') =
            prev >>= \(ev, ShiftedDay {..}, st) -> stateTransition ev calculationDay st <&> (ev',t',)

          st0 = initializeState <&> (AD,ShiftedDay statusDate statusDate,)
          ctx = CtxSTF ct fpSchedule prSchedule ipSchedule (maturity ct) rf

          fpSchedule = calculationDay <$> schedule FP ct -- init & stf rely on the fee payment schedule
          prSchedule = calculationDay <$> schedule PR ct -- init & stf rely on the principal redemption schedule
          ipSchedule = calculationDay <$> schedule IP ct -- init & stf rely on the interest payment schedule

      -- payoffs

      payoffs = runReader (sequence $ calculatePayoff <$> states) ctx
        where
          calculatePayoff (ev, ShiftedDay {..}, st) = payoff ev calculationDay st
          ctx = CtxPOF ct rf

   in zipWith (\(x, y, z) -> (x,y,z,)) states payoffs

  where
    filtersSchedules :: (EventType, ShiftedDay) -> Bool
    filtersSchedules (_, ShiftedDay {..}) | contractType == OPTNS = calculationDay > statusDate
    filtersSchedules (_, ShiftedDay {..}) | contractType == FUTUR = calculationDay > statusDate
    filtersSchedules (_, ShiftedDay {..}) = isNothing terminationDate || Just calculationDay <= terminationDate

    postProcessSchedules :: [(EventType, ShiftedDay)] -> [(EventType, ShiftedDay)]
    postProcessSchedules =
      let trim = dropWhile (\(_, d) -> calculationDay d < statusDate)
          regroup = groupBy (\(_, l) (_, r) -> calculationDay l == calculationDay r)
          overwrite = map (sortOn (\(ev, _) -> fromEnum ev)) . regroup
       in concat . overwrite . trim

    filtersStates :: (EventType, ShiftedDay, ContractStatePoly a) -> Bool
    filtersStates (ev, ShiftedDay {..}, _) =
      case contractType of
        PAM -> isNothing purchaseDate || Just calculationDay >= purchaseDate
        LAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
        NAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
        ANN ->
          let b1 = isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
              b2 = let m = maturityDate <|> amortizationDate <|> maturity ct in isNothing m || Just calculationDay <= m
           in b1 && b2
        SWPPV -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
        _ -> True

