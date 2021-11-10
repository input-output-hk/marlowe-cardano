{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.STF.StateTransition
  (
    stateTransition
  , CtxSTF (..)
  )
where

import Control.Monad.Reader (Reader, reader)
import Data.Maybe (fromMaybe, maybeToList)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTermsPoly (..))
import Language.Marlowe.ACTUS.Domain.Ops (DateOps (..), RoleSignOps (..), YearFractionOps (_y))
import Language.Marlowe.ACTUS.Model.STF.StateTransitionModel
import Language.Marlowe.ACTUS.Utility.ScheduleGenerator (inf', sup')

data CtxSTF a b = CtxSTF
  { contractTerms :: ContractTermsPoly a b
  , fpSchedule    :: [b]
  , prSchedule    :: [b]
  , ipSchedule    :: [b]
  , maturity      :: Maybe b
  }

-- |'stateTransition' updates the contract state based on the contract terms in the reader contrext
stateTransition :: (RoleSignOps a, YearFractionOps b a, DateOps b a, Ord b) =>
     EventType                                   -- ^ Event type
  -> RiskFactorsPoly a                           -- ^ Risk factors
  -> b                                           -- ^ Time
  -> ContractStatePoly a b                       -- ^ Contract state
  -> Reader (CtxSTF a b) (ContractStatePoly a b) -- ^ Updated contract state
stateTransition ev rf t st@ContractStatePoly{..} = reader stateTransition'
  where
    stateTransition' CtxSTF{..} = stf ev contractTerms
        where
          stf AD ContractTermsPoly {dayCountConvention = Just dcc, maturityDate = md} = _STF_AD_PAM st t (_y dcc sd t md)
          stf IED ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, cycleAnchorDateOfInterestPayment = Just ipanx, maturityDate = md} = _STF_IED_PAM contractTerms st t (_y dcc ipanx t md)
          stf IED ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, cycleAnchorDateOfInterestPayment = Just ipanx, maturityDate = md} = _STF_IED_LAM contractTerms st t (_y dcc ipanx t md)
          stf IED ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, cycleAnchorDateOfInterestPayment = Just ipanx, maturityDate = md} = _STF_IED_LAM contractTerms st t (_y dcc ipanx t md)
          stf IED ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, cycleAnchorDateOfInterestPayment = Just ipanx, maturityDate = md} = _STF_IED_LAM contractTerms st t (_y dcc ipanx t md)
          stf PR ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PR_LAM contractTerms st t (_y dcc sd t md)
          stf PR ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PR_NAM contractTerms st t (_y dcc sd t md)
          stf PR ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_PR_NAM contractTerms st t (_y dcc sd t md)
          stf MD _ = _STF_MD_PAM st t
          stf PP ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PP_PAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PP ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PP_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PP ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PP_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PP ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_PP_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PY ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PY_PAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PY ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PY_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PY ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PY_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PY ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_PY_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf FP ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_FP_PAM st t (_y dcc sd t md)
          stf FP ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_FP_LAM st t (_y dcc sd t md)
          stf FP ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_FP_LAM st t (_y dcc sd t md)
          stf FP ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_FP_LAM st t (_y dcc sd t md)
          stf PRD ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PRD_PAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PRD ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PRD_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PRD ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_PRD_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf PRD ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_PRD_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf TD _ = _STF_TD_PAM st t
          stf IP ContractTermsPoly {dayCountConvention = Just dcc, maturityDate = md} = _STF_IP_PAM contractTerms st t (_y dcc sd t md)
          stf IPCI ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_IPCI_PAM contractTerms st t (_y dcc sd t md)
          stf IPCI ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_IPCI_LAM contractTerms st t (_y dcc sd t md)
          stf IPCI ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_IPCI_LAM contractTerms st t (_y dcc sd t md)
          stf IPCI ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_IPCI_LAM contractTerms st t (_y dcc sd t md)
          stf IPCB ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_IPCB_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf IPCB ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_IPCB_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf IPCB ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_IPCB_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf RR ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_RR_PAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf RR ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_RR_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf RR ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_RR_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf RR ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_RR_ANN contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md) (zipWith (\tn tm -> _y dcc tn tm md) prDatesAfterSd (tail prDatesAfterSd))
          stf RRF ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_RRF_PAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf RRF ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_RRF_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf RRF ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_RRF_LAM contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf RRF ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_RRF_ANN contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md) (zipWith (\tn tm -> _y dcc tn tm md) prDatesAfterSd (tail prDatesAfterSd))
          stf PRF ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_PRF_ANN contractTerms st t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md) (_y dcc t tpr_plus md) (zipWith (\tn tm -> _y dcc tn tm md) prDatesAfterSd (tail prDatesAfterSd))
          stf SC ContractTermsPoly {contractType = PAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_SC_PAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf SC ContractTermsPoly {contractType = LAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_SC_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf SC ContractTermsPoly {contractType = NAM, dayCountConvention = Just dcc, maturityDate = md} = _STF_SC_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf SC ContractTermsPoly {contractType = ANN, dayCountConvention = Just dcc, maturityDate = md} = _STF_SC_LAM contractTerms st rf t (_y dcc sd t md) (_y dcc tfp_minus t md) (_y dcc tfp_minus tfp_plus md)
          stf XD ContractTermsPoly {contractType = OPTNS} = _STF_XD_OPTNS contractTerms st rf t
          stf XD ContractTermsPoly {contractType = FUTUR} = _STF_XD_FUTUR contractTerms st rf t
          stf CE ContractTermsPoly {dayCountConvention = Just dcc, maturityDate = md} = _STF_CE_PAM st t (_y dcc sd t md)
          stf _ _ = st

          tfp_minus = fromMaybe t (sup' fpSchedule t)
          tfp_plus = fromMaybe t (inf' fpSchedule t)
          tpr_plus = fromMaybe t (inf' prSchedule t)

          prDates = prSchedule ++ maybeToList maturity
          prDatesAfterSd = filter (>sd) prDates
