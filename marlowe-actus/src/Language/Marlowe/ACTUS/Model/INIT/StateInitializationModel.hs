{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS contract state initialization per t0

The implementation is a transliteration of the ACTUS specification v1.1
Note: initial states rely also on some schedules (and vice versa)

-}

module Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel
  ( initializeState
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Reader (Reader, reader)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Time.LocalTime (LocalTime)
import Language.Marlowe.ACTUS.Domain.ContractState (ContractState, ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTerms, ContractTermsPoly (..), Cycle (..),
                                                    FEB (..), IPCB (..), PRF, SCEF (..))
import Language.Marlowe.ACTUS.Domain.Ops (RoleSignOps (_r), YearFractionOps (_y))
import Language.Marlowe.ACTUS.Model.STF.StateTransition (CtxSTF (..))
import Language.Marlowe.ACTUS.Utility.ANN.Annuity (annuity)
import Language.Marlowe.ACTUS.Utility.ScheduleGenerator (generateRecurrentScheduleWithCorrections, inf', sup')

{-# ANN module "HLint: ignore Use camelCase" #-}

-- |'initializeState' initializes the state variables at t0 based on the
-- provided context
initializeState :: Reader (CtxSTF Double LocalTime) ContractState
initializeState = reader initializeState'
  where
    initializeState' :: CtxSTF Double LocalTime -> ContractState
    initializeState' CtxSTF {..} =
      ContractStatePoly
        { sd = t0,
          prnxt = nextPrincipalRedemptionPayment contractTerms,
          ipcb = interestPaymentCalculationBase contractTerms,
          tmd = maturity,
          nt = notionalPrincipal contractTerms,
          ipnr = nominalInterestRate contractTerms,
          ipac = interestAccrued contractTerms,
          feac = feeAccrued contractTerms,
          nsc = notionalScaling contractTerms,
          isc = interestScaling contractTerms,
          prf = contractPerformance contractTerms,
          xd = exerciseDate contractTerms,
          xa = exerciseAmount contractTerms <|> futuresPrice contractTerms
        }
      where
        t0 = statusDate contractTerms

        tMinusFP = fromMaybe t0 (sup' fpSchedule t0)
        tPlusFP = fromMaybe t0 (inf' fpSchedule t0)
        tMinusIP = fromMaybe t0 (sup' ipSchedule t0)

        scalingEffect_xNx :: SCEF -> Bool
        scalingEffect_xNx SE_ONO = True
        scalingEffect_xNx SE_ONM = True
        scalingEffect_xNx SE_INO = True
        scalingEffect_xNx SE_INM = True
        scalingEffect_xNx _      = False

        scalingEffect_Ixx :: SCEF -> Bool
        scalingEffect_Ixx SE_INO = True
        scalingEffect_Ixx SE_INM = True
        scalingEffect_Ixx SE_IOO = True
        scalingEffect_Ixx SE_IOM = True
        scalingEffect_Ixx _      = False

        interestScaling :: ContractTerms -> Double
        interestScaling
          ContractTermsPoly
            { scalingEffect = Just scef,
              interestScalingMultiplier = Just scip
            } | scalingEffect_Ixx scef = scip
        interestScaling _ = 1.0

        notionalScaling :: ContractTerms -> Double
        notionalScaling
          ContractTermsPoly
            { scalingEffect = Just scef,
              notionalScalingMultiplier = Just scnt
            } | scalingEffect_xNx scef = scnt
        notionalScaling _ = 1.0

        notionalPrincipal :: ContractTerms -> Double
        notionalPrincipal
          ContractTermsPoly
            { initialExchangeDate = Just ied
            } | ied > t0 = 0.0
        notionalPrincipal
          ct@ContractTermsPoly
            { notionalPrincipal = Just nt
            } = _r (contractRole ct) * nt
        notionalPrincipal _ = 0.0

        nominalInterestRate :: ContractTerms -> Double
        nominalInterestRate
          ContractTermsPoly
            { initialExchangeDate = Just ied
            } | ied > t0 = 0.0
        nominalInterestRate
          ContractTermsPoly
            { nominalInterestRate = Just ipnr
            } =
            ipnr
        nominalInterestRate _ = 0.0

        interestAccrued :: ContractTerms -> Double
        interestAccrued
          ContractTermsPoly
            { nominalInterestRate = Nothing
            } = 0.0
        interestAccrued
          ContractTermsPoly
            { accruedInterest = Just ipac
            } = ipac
        interestAccrued
          ContractTermsPoly
            { dayCountConvention = Just dcc
            } =
            let nt = notionalPrincipal contractTerms
                ipnr = nominalInterestRate contractTerms
             in _y dcc tMinusIP t0 maturity * nt * ipnr
        interestAccrued _ = 0.0

        nextPrincipalRedemptionPayment :: ContractTerms -> Double
        nextPrincipalRedemptionPayment ContractTermsPoly {contractType = PAM} = 0.0
        nextPrincipalRedemptionPayment ContractTermsPoly {nextPrincipalRedemptionPayment = Just prnxt} = prnxt
        nextPrincipalRedemptionPayment
          ContractTermsPoly
            { contractType = LAM,
              nextPrincipalRedemptionPayment = Nothing,
              maturityDate = Just md,
              notionalPrincipal = Just nt,
              cycleOfPrincipalRedemption = Just prcl,
              cycleAnchorDateOfPrincipalRedemption = Just pranx,
              scheduleConfig
            } = nt / fromIntegral (length $ generateRecurrentScheduleWithCorrections pranx (prcl {includeEndDay = True}) md scheduleConfig)
        nextPrincipalRedemptionPayment
          ContractTermsPoly
            { contractType = ANN,
              nextPrincipalRedemptionPayment = Nothing,
              accruedInterest = Just ipac,
              maturityDate = md,
              notionalPrincipal = Just nt,
              nominalInterestRate = Just ipnr,
              dayCountConvention = Just dcc
            } =
            let scale = nt + ipac
                frac = annuity ipnr ti
             in frac * scale
            where
              prDates = prSchedule ++ maybeToList maturity
              ti = zipWith (\tn tm -> _y dcc tn tm md) prDates (tail prDates)
        nextPrincipalRedemptionPayment _ = 0.0

        interestPaymentCalculationBase :: ContractTerms -> Double
        interestPaymentCalculationBase
          ContractTermsPoly
            { contractType = LAM,
              initialExchangeDate = Just ied
            } | t0 < ied = 0.0
        interestPaymentCalculationBase
          ct@ContractTermsPoly
            { notionalPrincipal = Just nt,
              interestCalculationBase = Just ipcb
            } | ipcb == IPCB_NT = _r (contractRole ct) * nt
        interestPaymentCalculationBase
          ct@ContractTermsPoly
            { interestCalculationBaseA = Just ipcba
            } = _r (contractRole ct) * ipcba
        interestPaymentCalculationBase _ = 0.0

        feeAccrued :: ContractTerms -> Double
        feeAccrued
          ContractTermsPoly
            { feeRate = Nothing
            } = 0.0
        feeAccrued
          ContractTermsPoly
            { feeAccrued = Just feac
            } = feac
        feeAccrued
          ContractTermsPoly
            { feeBasis = Just FEB_N,
              dayCountConvention = Just dcc,
              feeRate = Just fer,
              notionalPrincipal = Just nt,
              maturityDate = md
            } = _y dcc tMinusFP t0 md * nt * fer
        feeAccrued
          ContractTermsPoly
            { dayCountConvention = Just dcc,
              feeRate = Just fer,
              maturityDate = md
            } = _y dcc tMinusFP t0 md / _y dcc tMinusFP tPlusFP md * fer
        feeAccrued _ = 0.0

        contractPerformance :: ContractTerms -> PRF
        contractPerformance ContractTermsPoly {contractPerformance = Just prf} = prf
        contractPerformance _                                                  = error "PRF is not set in ContractTerms"
