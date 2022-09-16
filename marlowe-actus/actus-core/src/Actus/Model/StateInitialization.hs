{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-| = ACTUS contract state initialization per t0

The implementation is a transliteration of the ACTUS specification v1.1
Note: initial states rely also on some schedules (and vice versa)

-}

module Actus.Model.StateInitialization
  ( initializeState
  ) where

import Actus.Domain
  ( ActusFrac
  , CEGE(..)
  , CT(..)
  , ContractState(..)
  , ContractStructure(..)
  , ContractTerms(..)
  , Cycle(..)
  , FEB(..)
  , IPCB(..)
  , PRF(..)
  , Reference(..)
  , SCEF(..)
  , sign
  )
import Actus.Model.StateTransition (CtxSTF(..))
import Actus.Utility (annuity, generateRecurrentSchedule, inf, sup, yearFraction)
import Control.Applicative ((<|>))
import Control.Monad.Reader (Reader, reader)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import GHC.Records (getField)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- |'initializeState' initializes the state variables at t0 based on the
-- provided context
initializeState :: ActusFrac a => Reader (CtxSTF a) (ContractState a)
initializeState = reader initializeState'
  where
    initializeState' :: ActusFrac a => CtxSTF a -> ContractState a
    initializeState' CtxSTF {..} =
      ContractState
        { sd = t0,
          prnxt = nextPrincipalRedemptionPayment contractTerms,
          ipcb = interestPaymentCalculationBase contractTerms,
          tmd = maturity,
          nt = notionalPrincipal contractTerms,
          ipnr = nominalInterestRate contractTerms,
          ipac = interestAccrued contractTerms,
          ipac1 = interestAccrued1 contractTerms,
          ipac2 = interestAccrued2 contractTerms,
          ipla = Nothing,
          feac = feeAccrued contractTerms,
          nsc = notionalScaling contractTerms,
          isc = interestScaling contractTerms,
          prf = contractPerformance contractTerms,
          xd = exerciseDate contractTerms,
          xa = exerciseAmount contractTerms <|> futuresPrice contractTerms
        }
      where
        t0 = statusDate contractTerms

        tMinusFP = fromMaybe t0 (sup fpSchedule t0)
        tPlusFP = fromMaybe t0 (inf fpSchedule t0)
        tMinusIP = fromMaybe t0 (sup ipSchedule t0)

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

        interestScaling
          ContractTerms
            { scalingEffect = Just scef,
              interestScalingMultiplier = Just scip
            } | scalingEffect_Ixx scef = scip
        interestScaling _ = 1

        notionalScaling
          ContractTerms
            { scalingEffect = Just scef,
              notionalScalingMultiplier = Just scnt
            } | scalingEffect_xNx scef = scnt
        notionalScaling _ = 1

        notionalPrincipal
          ContractTerms
            { initialExchangeDate = Just ied
            } | ied > t0 = 0
        notionalPrincipal
          ContractTerms
            { contractType = CEG,
              notionalPrincipal = Just nt,
              coverageOfCreditEnhancement = Just cecv,
              contractRole
            } = sign contractRole * nt * cecv
        notionalPrincipal
          ContractTerms
            { contractType = CEG,
              coverageOfCreditEnhancement = Just cecv,
              guaranteedExposure = Just CEGE_NO,
              contractStructure,
              contractRole
            } | not (null contractStructure) =
              let cts = mapMaybe referenceContractTerms contractStructure
                  s = sum $ mapMaybe (getField @"notionalPrincipal") cts
               in sign contractRole * cecv * s
        notionalPrincipal
          ContractTerms
            { contractType = CEG,
              coverageOfCreditEnhancement = Just cecv,
              guaranteedExposure = Just CEGE_NI,
              contractStructure,
              contractRole
            } | not (null contractStructure) =
              let cts = mapMaybe referenceContractTerms contractStructure
                  s = sum $ mapMaybe (getField @"notionalPrincipal") cts
                  i = sum $ mapMaybe (getField @"accruedInterest") cts
               in sign contractRole * cecv * s * i
        notionalPrincipal
          ct@ContractTerms
            { notionalPrincipal = Just nt
            } = sign (contractRole ct) * nt
        notionalPrincipal _ = 0

        nominalInterestRate
          ContractTerms
            { initialExchangeDate = Just ied
            } | ied > t0 = 0
        nominalInterestRate
          ContractTerms
            { contractType = SWPPV
            , nominalInterestRate2 = Just ipnr2
            } =
            ipnr2
        nominalInterestRate
          ContractTerms
            { nominalInterestRate = Just ipnr
            } =
            ipnr
        nominalInterestRate _ = 0

        interestAccrued
          ContractTerms
            { contractType = SWPPV
            , dayCountConvention = Just dcc
            , nominalInterestRate = Just ipnr'
            } =
            let nt = notionalPrincipal contractTerms
                ipnr = nominalInterestRate contractTerms
             in yearFraction dcc tMinusIP t0 maturity * nt * (ipnr' - ipnr)
        interestAccrued
          ContractTerms
            { nominalInterestRate = Nothing
            } = 0
        interestAccrued
          ContractTerms
            { accruedInterest = Just ipac
            } = ipac
        interestAccrued
          ContractTerms
            { dayCountConvention = Just dcc
            } =
            let nt = notionalPrincipal contractTerms
                ipnr = nominalInterestRate contractTerms
             in yearFraction dcc tMinusIP t0 maturity * nt * ipnr
        interestAccrued _ = 0

        interestAccrued1
          ContractTerms
            { contractType = SWPPV
            , dayCountConvention = Just dcc
            , nominalInterestRate = Just ipnr'
            } =
            let nt = notionalPrincipal contractTerms
             in Just $ yearFraction dcc tMinusIP t0 maturity * nt * ipnr'
        interestAccrued1 _ = Nothing

        interestAccrued2
          ContractTerms
            { contractType = SWPPV
            , dayCountConvention = Just dcc
            } =
            let nt = notionalPrincipal contractTerms
                ipnr = nominalInterestRate contractTerms
             in Just $ yearFraction dcc tMinusIP t0 maturity * nt * ipnr
        interestAccrued2 _ = Nothing

        nextPrincipalRedemptionPayment ContractTerms {contractType = PAM} = 0
        nextPrincipalRedemptionPayment ContractTerms {nextPrincipalRedemptionPayment = Just prnxt} = prnxt
        nextPrincipalRedemptionPayment
          ContractTerms
            { contractType = LAM,
              nextPrincipalRedemptionPayment = Nothing,
              maturityDate = Just md,
              notionalPrincipal = Just nt,
              cycleOfPrincipalRedemption = Just prcl,
              cycleAnchorDateOfPrincipalRedemption = Just pranx,
              scheduleConfig
            } = nt / fromInteger (fromIntegral . length $ generateRecurrentSchedule pranx (prcl {includeEndDay = True}) md scheduleConfig)
        nextPrincipalRedemptionPayment
          ContractTerms
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
              ti = zipWith (\tn tm -> yearFraction dcc tn tm md) prDates (tail prDates)
        nextPrincipalRedemptionPayment _ = 0

        interestPaymentCalculationBase
          ContractTerms
            { contractType = LAM,
              initialExchangeDate = Just ied
            } | t0 < ied = 0
        interestPaymentCalculationBase
          ct@ContractTerms
            { notionalPrincipal = Just nt,
              interestCalculationBase = Just ipcb
            } | ipcb == IPCB_NT = sign (contractRole ct) * nt
        interestPaymentCalculationBase
          ct@ContractTerms
            { interestCalculationBaseA = Just ipcba
            } = sign (contractRole ct) * ipcba
        interestPaymentCalculationBase _ = 0

        feeAccrued
          ContractTerms
            { feeRate = Nothing
            } = 0
        feeAccrued
          ContractTerms
            { feeAccrued = Just feac
            } = feac
        feeAccrued
          ContractTerms
            { feeBasis = Just FEB_N,
              dayCountConvention = Just dcc,
              feeRate = Just fer,
              notionalPrincipal = Just nt,
              maturityDate = md
            } = yearFraction dcc tMinusFP t0 md * nt * fer
        feeAccrued
          ContractTerms
            { dayCountConvention = Just dcc,
              feeRate = Just fer,
              maturityDate = md
            } = yearFraction dcc tMinusFP t0 md / yearFraction dcc tMinusFP tPlusFP md * fer
        feeAccrued _ = 0

        contractPerformance ContractTerms {contractPerformance = Just prf} = prf
        contractPerformance _                                              = PRF_PF

        referenceContractTerms :: ContractStructure a -> Maybe (ContractTerms a)
        referenceContractTerms ContractStructure {..} =
          case reference of
            ReferenceTerms rt -> Just rt
            ReferenceId _     -> Nothing
