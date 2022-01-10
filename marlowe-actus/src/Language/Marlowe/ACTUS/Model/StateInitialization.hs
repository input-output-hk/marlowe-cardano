{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

{-| = ACTUS contract state initialization per t0

The implementation is a transliteration of the ACTUS specification v1.1
Note: initial states rely also on some schedules (and vice versa)

-}

module Language.Marlowe.ACTUS.Model.StateInitialization
  ( initializeState
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Reader (Reader, reader)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import GHC.Records (getField)
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CEGE (..), CT (..), ContractStructure (..), ContractTermsPoly (..),
                                                    Cycle (..), FEB (..), IPCB (..), Reference (..), SCEF (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusNum (..), ActusOps (..), RoleSignOps (_r), YearFractionOps (..))
import Language.Marlowe.ACTUS.Model.StateTransition (CtxSTF (..))
import Language.Marlowe.ACTUS.Utility.ANN.Annuity (annuity)
import Language.Marlowe.ACTUS.Utility.ScheduleGenerator (generateRecurrentSchedule, inf, sup)
import Prelude hiding ((*), (+), (-), (/))

{-# ANN module "HLint: ignore Use camelCase" #-}

-- |'initializeState' initializes the state variables at t0 based on the
-- provided context
initializeState :: (RoleSignOps a, YearFractionOps a) => Reader (CtxSTF a) (ContractStatePoly a)
initializeState = reader initializeState'
  where
    initializeState' :: (RoleSignOps a, YearFractionOps a) => CtxSTF a -> ContractStatePoly a
    initializeState' CtxSTF {..} =
      ContractStatePoly
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
          ContractTermsPoly
            { scalingEffect = Just scef,
              interestScalingMultiplier = Just scip
            } | scalingEffect_Ixx scef = scip
        interestScaling _ = _one

        notionalScaling
          ContractTermsPoly
            { scalingEffect = Just scef,
              notionalScalingMultiplier = Just scnt
            } | scalingEffect_xNx scef = scnt
        notionalScaling _ = _one

        notionalPrincipal
          ContractTermsPoly
            { initialExchangeDate = Just ied
            } | ied > t0 = _zero
        notionalPrincipal
          ContractTermsPoly
            { contractType = CEG,
              notionalPrincipal = Just nt,
              coverageOfCreditEnhancement = Just cecv,
              contractRole
            } = _r contractRole * nt * cecv
        notionalPrincipal
          ContractTermsPoly
            { contractType = CEG,
              coverageOfCreditEnhancement = Just cecv,
              guaranteedExposure = Just CEGE_NO,
              contractStructure,
              contractRole
            } | not (null contractStructure) =
              let cts = mapMaybe referenceContractTerms contractStructure
                  s = foldr (+) _zero $ mapMaybe (getField @"notionalPrincipal") cts
               in _r contractRole * cecv * s
        notionalPrincipal
          ContractTermsPoly
            { contractType = CEG,
              coverageOfCreditEnhancement = Just cecv,
              guaranteedExposure = Just CEGE_NI,
              contractStructure,
              contractRole
            } | not (null contractStructure) =
              let cts = mapMaybe referenceContractTerms contractStructure
                  s = foldr (+) _zero $ mapMaybe (getField @"notionalPrincipal") cts
                  i = foldr (+) _zero $ mapMaybe (getField @"accruedInterest") cts
               in _r contractRole * cecv * s * i
        notionalPrincipal
          ct@ContractTermsPoly
            { notionalPrincipal = Just nt
            } = _r (contractRole ct) * nt
        notionalPrincipal _ = _zero

        nominalInterestRate
          ContractTermsPoly
            { initialExchangeDate = Just ied
            } | ied > t0 = _zero
        nominalInterestRate
          ContractTermsPoly
            { contractType = SWPPV
            , nominalInterestRate2 = Just ipnr2
            } =
            ipnr2
        nominalInterestRate
          ContractTermsPoly
            { nominalInterestRate = Just ipnr
            } =
            ipnr
        nominalInterestRate _ = _zero

        interestAccrued
          ContractTermsPoly
            { contractType = SWPPV
            , dayCountConvention = Just dcc
            , nominalInterestRate = Just ipnr'
            } =
            let nt = notionalPrincipal contractTerms
                ipnr = nominalInterestRate contractTerms
             in _y dcc tMinusIP t0 maturity * nt * (ipnr' - ipnr)
        interestAccrued
          ContractTermsPoly
            { nominalInterestRate = Nothing
            } = _zero
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
        interestAccrued _ = _zero

        interestAccrued1
          ContractTermsPoly
            { contractType = SWPPV
            , dayCountConvention = Just dcc
            , nominalInterestRate = Just ipnr'
            } =
            let nt = notionalPrincipal contractTerms
             in Just $ _y dcc tMinusIP t0 maturity * nt * ipnr'
        interestAccrued1 _ = Nothing

        interestAccrued2
          ContractTermsPoly
            { contractType = SWPPV
            , dayCountConvention = Just dcc
            } =
            let nt = notionalPrincipal contractTerms
                ipnr = nominalInterestRate contractTerms
             in Just $ _y dcc tMinusIP t0 maturity * nt * ipnr
        interestAccrued2 _ = Nothing

        nextPrincipalRedemptionPayment ContractTermsPoly {contractType = PAM} = _zero
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
            } = nt / _fromInteger (fromIntegral . length $ generateRecurrentSchedule pranx (prcl {includeEndDay = True}) md scheduleConfig)
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
        nextPrincipalRedemptionPayment _ = _zero

        interestPaymentCalculationBase
          ContractTermsPoly
            { contractType = LAM,
              initialExchangeDate = Just ied
            } | t0 < ied = _zero
        interestPaymentCalculationBase
          ct@ContractTermsPoly
            { notionalPrincipal = Just nt,
              interestCalculationBase = Just ipcb
            } | ipcb == IPCB_NT = _r (contractRole ct) * nt
        interestPaymentCalculationBase
          ct@ContractTermsPoly
            { interestCalculationBaseA = Just ipcba
            } = _r (contractRole ct) * ipcba
        interestPaymentCalculationBase _ = _zero

        feeAccrued
          ContractTermsPoly
            { feeRate = Nothing
            } = _zero
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
        feeAccrued _ = _zero

        contractPerformance ContractTermsPoly {contractPerformance = Just prf} = prf
        contractPerformance _                                                  = error "PRF is not set in ContractTerms"

        referenceContractTerms :: ContractStructure a -> Maybe (ContractTermsPoly a)
        referenceContractTerms ContractStructure {..} =
          case reference of
            ReferenceTerms rt -> Just rt
            ReferenceId _     -> Nothing
