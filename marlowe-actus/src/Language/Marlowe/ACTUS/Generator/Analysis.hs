{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

{-| = ACTUS Analysis

Given ACTUS contract terms cashflows can be projected from the predefined risk factors.
The cash flows can be used to generate the payments in a Marlowe contract.

-}

module Language.Marlowe.ACTUS.Generator.Analysis
  ( genProjectedCashflows
  , genProjectedPayoffs
  )
where

import           Control.Applicative                                        ((<|>))
import           Control.Monad.Reader                                       (runReader)
import           Data.Functor                                               ((<&>))
import qualified Data.List                                                  as L (groupBy)
import           Data.Maybe                                                 (fromMaybe, isNothing)
import           Data.Sort                                                  (sortOn)
import           Data.Time                                                  (LocalTime)
import           Language.Marlowe.ACTUS.Domain.BusinessEvents               (EventType (..), RiskFactorsPoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractState                (ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms                (CT (..), ContractTermsPoly (..))
import           Language.Marlowe.ACTUS.Domain.Ops                          (DateOps (..), RoleSignOps (..),
                                                                             YearFractionOps)
import           Language.Marlowe.ACTUS.Domain.Schedule                     (CashFlowPoly (..), ShiftedDay (..),
                                                                             calculationDay, paymentDay)
import           Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel (initializeState)
import           Language.Marlowe.ACTUS.Model.POF.Payoff                    (payoff)
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule        as S (maturity, schedule)
import           Language.Marlowe.ACTUS.Model.STF.StateTransition           (CtxSTF (..), stateTransition)

-- |'genProjectedCashflows' generates a list of projected cashflows for
-- given contract terms and provided risk factors. The function returns
-- an empty list, if building the initial state given the contract terms
-- fails or in case there are no cash flows.
genProjectedCashflows :: (DateOps LocalTime a, RoleSignOps a, YearFractionOps LocalTime a) =>
  (EventType -> LocalTime -> RiskFactorsPoly a) -- ^ Risk factors as a function of event type and time
  -> ContractTermsPoly a                        -- ^ ACTUS contract terms
  -> [CashFlowPoly a]                           -- ^ List of projected cash flows
genProjectedCashflows getRiskFactors ct =
  let genCashflow (ContractStatePoly {..}, ev, t, am) =
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
   in sortOn cashPaymentDay . fmap genCashflow . genProjectedPayoffs getRiskFactors $ ct

genProjectedPayoffs :: (DateOps LocalTime a, RoleSignOps a, YearFractionOps LocalTime a) =>
  (EventType -> LocalTime -> RiskFactorsPoly a)        -- ^ Risk factors as a function of event type and time
  -> ContractTermsPoly a                               -- ^ ACTUS contract terms
  -> [(ContractStatePoly a, EventType, ShiftedDay, a)] -- ^ List of projected payoffs
genProjectedPayoffs getRiskFactors ct@ContractTermsPoly {..} =
  let -- schedules

      schedules =
        filter filtersSchedules . postProcessSchedule . sortOn (paymentDay . snd) $
          concatMap scheduleEvent eventTypes
        where
          eventTypes = [IED, MD, RR, RRF, IP, PR, PRF, IPCB, IPCI, PRD, TD, SC, DV, XD, STD]
          scheduleEvent ev = (ev,) <$> schedule ev ct

      -- states

      states =
        filter filtersStates . tail $
          runReader (sequence $ scanl applyStateTransition initialState schedules) context
        where
          initialState = initializeState <&> (,AD,ShiftedDay statusDate statusDate)

          applyStateTransition x (ev', t') = do
            (st, ev, d) <- x
            let t = calculationDay d
            let rf = getRiskFactors ev t
            stateTransition ev rf t st <&> (,ev',t')

          context = CtxSTF ct fpSchedule prSchedule ipSchedule mat

          fpSchedule = calculationDay <$> schedule FP ct -- init & stf rely on the fee payment schedule
          prSchedule = calculationDay <$> schedule PR ct -- init & stf rely on the principal redemption schedule
          ipSchedule = calculationDay <$> schedule IP ct -- init & stf rely on the interest payment schedule

      -- payoffs

      payoffs = calculatePayoff <$> states
        where
          calculatePayoff (st, ev, d) =
            let t = calculationDay d
                rf = getRiskFactors ev t
             in payoff ev rf ct st t
   in zipWith (\(x,y,z) -> (x,y,z,)) states payoffs
  where
    mat = S.maturity ct

    filtersSchedules :: (EventType, ShiftedDay) -> Bool
    filtersSchedules (_, ShiftedDay {..}) | contractType == OPTNS = calculationDay > statusDate
    filtersSchedules (_, ShiftedDay {..}) | contractType == FUTUR = calculationDay > statusDate
    filtersSchedules (_, ShiftedDay {..}) = isNothing terminationDate || Just calculationDay <= terminationDate

    filtersStates :: (ContractStatePoly a, EventType, ShiftedDay) -> Bool
    filtersStates (_, ev, ShiftedDay {..}) =
      case contractType of
        PAM -> isNothing purchaseDate || Just calculationDay >= purchaseDate
        LAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
        NAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
        ANN ->
          let b1 = isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
              b2 = let m = maturityDate <|> amortizationDate <|> mat in isNothing m || Just calculationDay <= m
           in b1 && b2
        _ -> True

    postProcessSchedule :: [(EventType, ShiftedDay)] -> [(EventType, ShiftedDay)]
    postProcessSchedule =
      let trim = dropWhile (\(_, d) -> calculationDay d < statusDate)

          priority :: (EventType, ShiftedDay) -> Int
          priority (event, _) = fromEnum event

          similarity (_, l) (_, r) = calculationDay l == calculationDay r
          regroup = L.groupBy similarity

          overwrite = map (sortOn priority) . regroup
       in concat . overwrite . trim
