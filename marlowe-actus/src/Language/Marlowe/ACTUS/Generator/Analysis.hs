{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-| = ACTUS Analysis

Given ACTUS contract terms cashflows can be projected from the predefined risk factors.
The cash flows can be used to generate the payments in a Marlowe contract.

-}

module Language.Marlowe.ACTUS.Generator.Analysis
  ( genStates
  , genPayoffs
  , genCashflow
  , genProjectedPayoffs
  , genProjectedCashflows
  )
where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Data.Functor ((<&>))
import Data.List (groupBy)
import Data.Maybe (fromMaybe, isNothing)
import Data.Sort (sortOn)
import Data.Time (LocalTime)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTermsPoly (..))
import Language.Marlowe.ACTUS.Domain.Ops (RoleSignOps (..), ScheduleOps (..), YearFractionOps)
import Language.Marlowe.ACTUS.Domain.Schedule (CashFlowPoly (..), ShiftedDay (..), calculationDay, paymentDay)
import Language.Marlowe.ACTUS.Model.ContractSchedule as S (maturity, schedule)
import Language.Marlowe.ACTUS.Model.Payoff (CtxPOF (CtxPOF), payoff)
import Language.Marlowe.ACTUS.Model.StateInitialization (initializeState)
import Language.Marlowe.ACTUS.Model.StateTransition (CtxSTF (..), stateTransition)

-- |'genProjectedCashflows' generates a list of projected cashflows for
-- given contract terms and provided risk factors. The function returns
-- an empty list, if building the initial state given the contract terms
-- fails or in case there are no cash flows.
genProjectedCashflows :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
  (EventType -> LocalTime -> RiskFactorsPoly a) -- ^ Risk factors as a function of event type and time
  -> ContractTermsPoly a                        -- ^ Contract terms
  -> [CashFlowPoly a]                           -- ^ List of projected cash flows
genProjectedCashflows rf ct =
  genCashflow ct
    <$> ( runReader
            genProjectedPayoffs
            $ CtxSTF
              ct
              (calculationDay <$> schedule FP ct) -- init & stf rely on the fee payment schedule
              (calculationDay <$> schedule PR ct) -- init & stf rely on the principal redemption schedule
              (calculationDay <$> schedule IP ct) -- init & stf rely on the interest payment schedule
              (S.maturity ct)
              rf
        )

-- |Generate cash flows
genCashflow ::
  ContractTermsPoly a                                -- ^ Contract terms
  -> (EventType, ShiftedDay, ContractStatePoly a, a) -- ^ Projected payoff
  -> CashFlowPoly a                                  -- ^ Projected cash flow
genCashflow ct (ev, t, ContractStatePoly {..}, am) =
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

-- |Generate projected cash flows
genProjectedPayoffs :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
  Reader (CtxSTF a) [(EventType, ShiftedDay, ContractStatePoly a, a)]
genProjectedPayoffs =
  do
    schedules <- genSchedules . contractTerms <$> ask
    st0 <- initializeState
    states <- genStates schedules st0
    payoffs <- trans $ genPayoffs states

    return $
      sortOn (\(_,y,_,_) -> y) $
        zipWith (\(x,y,z) -> (x,y,z,)) states payoffs

  where
    trans :: Reader (CtxPOF a) b -> Reader (CtxSTF a) b
    trans = withReader (\c -> CtxPOF (contractTerms c) (riskFactors c))

-- |Generate schedules
genSchedules :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
  ContractTermsPoly a          -- ^ Contract terms
  -> [(EventType, ShiftedDay)] -- ^ Schedules
genSchedules ct@ContractTermsPoly {..} =
  filter filtersSchedules . postProcessSchedules . sortOn (paymentDay . snd) $
    concatMap scheduleEvent eventTypes
  where
    eventTypes = [IED, MD, IP, IPFX, IPFL, RR, RRF, PR, PRF, IPCB, IPCI, PRD, TD, SC, DV, XD, STD]
    scheduleEvent ev = (ev,) <$> schedule ev ct

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

-- |Generate states
genStates :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
  [(EventType, ShiftedDay)]                                           -- ^ Schedules
  -> ContractStatePoly a                                              -- ^ Initial state
  -> Reader (CtxSTF a) [(EventType, ShiftedDay, ContractStatePoly a)] -- ^ New states
genStates scs stn =
  let l = scanl apply st0 scs
    in sequence l >>= filterM filtersStates . tail
  where
    apply prev (ev', t') =
      prev >>= \(ev, ShiftedDay {..}, st) -> stateTransition ev calculationDay st <&> (ev',t',)

    st0 = return (AD,ShiftedDay (sd stn) (sd stn),stn)

    filtersStates ::
      (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
      (EventType, ShiftedDay, ContractStatePoly a) ->
      Reader (CtxSTF a) Bool
    filtersStates (ev, ShiftedDay {..}, _) =
      do ct@ContractTermsPoly {..} <- contractTerms <$> ask
         case contractType of
            PAM -> return $ isNothing purchaseDate || Just calculationDay >= purchaseDate
            LAM -> return $ isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
            NAM -> return $ isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
            ANN ->
              let b1 = isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
                  b2 = let m = maturityDate <|> amortizationDate <|> S.maturity ct in isNothing m || Just calculationDay <= m
               in return $ b1 && b2
            SWPPV -> return $ isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
            _ -> return True

-- |Generate payoffs
genPayoffs :: (RoleSignOps a, YearFractionOps a) =>
  [(EventType, ShiftedDay, ContractStatePoly a)] -- ^ States with schedule
  -> Reader (CtxPOF a) [a]                       -- ^ Payoffs
genPayoffs = mapM calculatePayoff
  where
    calculatePayoff (ev, ShiftedDay {..}, st) = payoff ev calculationDay st
