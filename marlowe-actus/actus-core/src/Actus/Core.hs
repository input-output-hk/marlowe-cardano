{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


{-| = ACTUS

Given ACTUS contract terms, cashflows are projected based on risk factors.

-}

module Actus.Core
  where

import Actus.Domain
  ( CR(..)
  , CT(..)
  , CashFlow(..)
  , ContractState(..)
  , ContractStructure(..)
  , ContractTerms(..)
  , DS(..)
  , EventType(..)
  , MinMax(..)
  , Reference(..)
  , ReferenceRole(..)
  , RiskFactors(..)
  , ShiftedDay(..)
  )
import Actus.Model (CtxPOF(CtxPOF), CtxSTF(..), initializeState, maturity, payoff, schedule, stateTransition)
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.Reader (Reader, asks, runReader, withReader)
import Data.List (groupBy, nub)
import Data.Maybe (fromMaybe, isNothing)
import Data.Sort (sortOn)
import Data.Time (LocalTime)

-- |'genProjectedCashflows' generates a list of projected cashflows for
-- given contract terms and provided risk factors. The function returns
-- an empty list, if building the initial state given the contract terms
-- fails or in case there are no cash flows.
genProjectedCashflows ::
  MinMax a =>
  RealFrac a =>
  -- | Risk factors as a function of event type and time
  (String -> EventType -> LocalTime -> RiskFactors a) ->
  -- | Contract terms
  ContractTerms a ->
  -- | Unscheduled events
  [Event] ->
  -- | List of projected cash flows
  [CashFlow a]
genProjectedCashflows rf ct us =
  let ctx = buildCtx rf ct us
   in check ct $ genCashflow ct <$> runReader (genProjectedPayoffs us) ctx
  where
    check :: Fractional a => ContractTerms a -> [CashFlow a] -> [CashFlow a]
    check ContractTerms {deliverySettlement = Just DS_S} = netCashflows
    check _                                              = id

    netCashflows :: Fractional a => [CashFlow a] -> [CashFlow a]
    netCashflows cf = foldl1 plus <$> groupBy f cf
      where
        f a b =
          cashEvent a == cashEvent b
            && cashPaymentDay a == cashPaymentDay b
            && cashParty a == cashParty b
            && cashCounterParty a == cashCounterParty b
            && cashCurrency a == cashCurrency b
        plus a b =
          a
            { amount = amount a + amount b,
              notional = notional a + notional b
            }

-- | Bulid the context allowing to perform state transitions
buildCtx ::
  MinMax a =>
  RealFrac a =>
  -- | Risk factors as a function of event type and time
  (String -> EventType -> LocalTime -> RiskFactors a) ->
  -- | Contract terms
  ContractTerms a ->
  -- | Unscheduled events
  [Event] ->
  -- | Context
  CtxSTF a
buildCtx rf ct us =
  CtxSTF
    ct
    (calculationDay . snd <$> schedule FP ct) -- init & stf rely on the fee payment schedule
    (calculationDay . snd <$> schedule PR ct) -- init & stf rely on the principal redemption schedule
    (calculationDay . snd <$> schedule IP ct) -- init & stf rely on the interest payment schedule
    (maturity ct)
    rf
    $ map f (contractStructure ct)
  where
    f cs = case reference cs of
      ReferenceTerms rt ->
        let ut = case referenceRole cs of
              FIL | contractRole ct == CR_RFL -> rt {contractRole = CR_RPA}
              FIL                             -> rt {contractRole = CR_RPL}
              SEL | contractRole ct == CR_RFL -> rt {contractRole = CR_RPL}
              SEL                             -> rt {contractRole = CR_RPA}
              _                               -> rt
            unscheduledEvents = nub $ map monitorEvent us
            monitorEvent (_, _, sd) = (contractId rt, AD, sd)
         in runReader (genProjectedPayoffs unscheduledEvents) $ buildCtx rf ut unscheduledEvents
      ReferenceId _ -> []

-- |Generate cash flows
genCashflow ::
  Fractional a =>
  -- | Contract terms
  ContractTerms a ->
  -- | Projected payoff
  (Event, ContractState a, a) ->
  -- | Projected cash flow
  CashFlow a
genCashflow ct ((cid, ev, t), ContractState {..}, am) =
  CashFlow
    { tick = 0,
      cashContractId = cid,
      cashParty = "party",
      cashCounterParty = "counterparty",
      cashPaymentDay = paymentDay t,
      cashCalculationDay = calculationDay t,
      cashEvent = ev,
      amount = am,
      notional = nt,
      cashCurrency = fromMaybe "unknown" (currency ct)
    }

-- |Generate projected cash flows
genProjectedPayoffs ::
  MinMax a =>
  RealFrac a =>
  -- | Unscheduled events
  [Event] ->
  Reader (CtxSTF a) [(Event, ContractState a, a)]
genProjectedPayoffs us =
  do
    ct <- asks contractTerms
    genProjectedPayoffs' $ genSchedule ct us

-- |Generate projected cash flows
genProjectedPayoffs' ::
  MinMax a =>
  RealFrac a =>
  -- | Events
  [Event] ->
  -- | Projected cash flows
  Reader (CtxSTF a) [(Event, ContractState a, a)]
genProjectedPayoffs' events =
  do
    states <- initializeState >>= genStates events
    (eventTypes, filteredStates) <- unzip <$> filterM filtersStates (zip (tail events) states)

    payoffs <- trans $ genPayoffs eventTypes filteredStates

    pure $ zip3 eventTypes filteredStates payoffs
  where
    trans :: Reader (CtxPOF a) b -> Reader (CtxSTF a) b
    trans = withReader (\c -> CtxPOF (contractTerms c) (riskFactors c) (referenceStates c))

-- |Generate schedules
genSchedule ::
  RealFrac a =>
  -- | Contract terms
  ContractTerms a ->
  -- | Schedule
  [Event] ->
  -- | Schedule
  [Event]
genSchedule ct us =
  sortOn (\(_, ev, d) -> (paymentDay d, ev)) $ genFixedSchedule ct <> us

genFixedSchedule ::
  RealFrac a =>
  -- | Contract terms
  ContractTerms a ->
  -- | Schedule
  [Event]
genFixedSchedule ct@ContractTerms {..} =
  filter filtersSchedules . postProcessSchedules . sortOn (\(_, ev, d) -> (paymentDay d, ev)) $
    (event : concatMap scheduleEvent eventTypes)
  where
    event = ("", AD, ShiftedDay statusDate statusDate)

    eventTypes = enumFrom (toEnum 0)
    scheduleEvent ev = map (\(cid, d) -> (cid, ev, d)) $ schedule ev ct

    filtersSchedules :: Event -> Bool
    filtersSchedules (_, AD, ShiftedDay {..}) = calculationDay == statusDate
    filtersSchedules (_, _, ShiftedDay {..}) | contractType == OPTNS = calculationDay > statusDate
    filtersSchedules (_, _, ShiftedDay {..}) | contractType == FUTUR = calculationDay > statusDate
    filtersSchedules (_, _, ShiftedDay {..}) | contractType == CLM = calculationDay > statusDate
    filtersSchedules (_, _, ShiftedDay {..}) = isNothing terminationDate || Just calculationDay <= terminationDate

    postProcessSchedules :: [Event] -> [Event]
    postProcessSchedules =
      let trim = dropWhile (\(_, _, d) -> calculationDay d < statusDate)
          regroup = groupBy (\(_, _, l) (_, _, r) -> calculationDay l == calculationDay r)
          overwrite = map (sortOn (\(_, ev, _) -> fromEnum ev)) . regroup
       in concat . overwrite . trim

type Event = (String, EventType, ShiftedDay)

-- |Generate states
genStates ::
  MinMax a =>
  Fractional a =>
  -- | Schedules
  [Event] ->
  -- | Initial state
  ContractState a ->
  -- | New states
  Reader (CtxSTF a) [ContractState a]
genStates ((_, eventType, ShiftedDay { calculationDay }) : events) state = do
  nextState <- stateTransition eventType calculationDay state
  nextStates <- genStates events nextState
  pure (nextState : nextStates)
genStates [] _ = pure []

filtersStates ::
  RealFrac a =>
  ((String, EventType, ShiftedDay), ContractState a) ->
  Reader (CtxSTF a) Bool
filtersStates ((_, ev, ShiftedDay {..}), _) =
  do
    ct@ContractTerms {..} <- asks contractTerms
    return $ case contractType of
      PAM -> isNothing purchaseDate || Just calculationDay >= purchaseDate
      LAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
      NAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
      ANN ->
        let b1 = isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
            b2 = let m = maturityDate <|> amortizationDate <|> maturity ct in isNothing m || Just calculationDay <= m
         in b1 && b2
      SWPPV -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
      SWAPS -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
      CLM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
      _ -> True

-- |Generate payoffs
genPayoffs ::
  MinMax a =>
  Fractional a =>
  -- | States with schedule
  [Event] ->
  -- | States with schedule
  [ContractState a] ->
  -- | Payoffs
  Reader (CtxPOF a) [a]
genPayoffs evs sts = mapM calculatePayoff $ zip evs sts
  where
    calculatePayoff ((cid, ev, ShiftedDay {..}), st) = payoff (cid, ev, calculationDay) st
