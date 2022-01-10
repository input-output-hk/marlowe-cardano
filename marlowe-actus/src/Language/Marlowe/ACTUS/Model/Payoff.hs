{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS payoff functions -}
module Language.Marlowe.ACTUS.Model.Payoff
  ( payoff
  , CtxPOF (..)
  )
where

import Control.Monad.Reader (Reader, reader)
import Data.Time.LocalTime (LocalTime)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTermsPoly (..), FEB (..), PYTP (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusNum (..), ActusOps (..), RoleSignOps (..), YearFractionOps (_y))
import Prelude hiding (Fractional, Num, (*), (+), (-), (/))

-- |The context for payoff functions
data CtxPOF a = CtxPOF
  { contractTerms :: ContractTermsPoly a                         -- ^ Contract terms
  , riskFactors   :: EventType -> LocalTime -> RiskFactorsPoly a -- ^ Risk factors as a function of event type and time
  }

-- |The payoff function
payoff :: (RoleSignOps a, YearFractionOps a) =>
     EventType           -- ^ Event type
  -> LocalTime           -- ^ Time
  -> ContractStatePoly a -- ^ Contract state
  -> Reader (CtxPOF a) a -- ^ Updated contract state
payoff ev t st = reader payoff'
  where
    payoff' CtxPOF {..} = pof ev (riskFactors ev t) contractTerms st
      where
        pof ::
          (RoleSignOps a, YearFractionOps a) =>
            EventType -> RiskFactorsPoly a -> ContractTermsPoly a -> ContractStatePoly a -> a
        ----------------------------
        -- Initial Exchange (IED) --
        ----------------------------
        -- POF_IED_SWPPV
        pof
          IED
          _
          ContractTermsPoly
            { contractType = SWPPV
            }
          _ = _zero
        -- POF_IED_CLM
        pof
          IED
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = CLM,
              contractRole,
              notionalPrincipal = Just nt
            }
          _ = _negate $ o_rf_CURS * _r contractRole * nt
        -- POF_IED_*
        pof
          IED
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { notionalPrincipal = Just nt,
              premiumDiscountAtIED = Just pdied,
              contractRole
            }
          _ = _negate $ o_rf_CURS * _r contractRole * (nt + pdied)
        -- POF_IED_*
        pof
          IED
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { notionalPrincipal = Just nt,
              contractRole
            }
          _ = _negate $ o_rf_CURS * _r contractRole * nt
        -------------------------------
        -- Principal Redemption (PR) --
        -------------------------------
        -- POF_PR_LAM
        pof
          PR
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = LAM,
              contractRole
            }
          ContractStatePoly
            { nt,
              nsc,
              prnxt
            } =
            let redemption = prnxt - _r contractRole * _max _zero (_abs prnxt - _abs nt)
             in o_rf_CURS * _r contractRole * nsc * redemption
        -- POF_PR_NAM
        -- POF_PR_ANN
        pof
          PR
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType,
              dayCountConvention = Just dcc,
              maturityDate,
              contractRole
            }
          ContractStatePoly
            { nt,
              nsc,
              prnxt,
              ipac,
              ipcb,
              ipnr,
              sd
            }
            | contractType `elem` [NAM, ANN] =
              let y_sd_t = _y dcc sd t maturityDate
                  ra = prnxt - _r contractRole * (ipac + y_sd_t * ipnr * ipcb)
                  r = ra - _max _zero (ra - _abs nt)
               in o_rf_CURS * _r contractRole * nsc * r
        -- POF_PR_SWPPV
        pof
          PR
          _
          ContractTermsPoly
            { contractType = SWPPV
            }
          _ = _zero
        -------------------
        -- Maturity (MD) --
        -------------------
        -- POF_MD_OPTNS
        -- POF_MD_SWPPV
        -- POF_MD_CEG
        pof
          MD
          _
          ContractTermsPoly
            { contractType
            }
          _ | contractType `elem` [OPTNS, SWPPV, CEG] = _zero
        -- POF_IED_*
        pof
          MD
          RiskFactorsPoly
            { o_rf_CURS
            }
          _
          ContractStatePoly
            { nt,
              nsc,
              isc,
              ipac,
              feac
            } = o_rf_CURS * (nsc * nt + isc * ipac + feac)
        -------------------------------
        -- Principal Prepayment (PP) --
        -------------------------------
        -- POF_PP_*
        pof
          PP
          RiskFactorsPoly
            { o_rf_CURS,
              pp_payoff
            }
          _
          _ = o_rf_CURS * pp_payoff
        --------------------------
        -- Penalty Payment (PY) --
        --------------------------
        -- POF_PY_*
        pof
          PY
          RiskFactorsPoly
            { o_rf_CURS,
              o_rf_RRMO
            }
          ContractTermsPoly
            { penaltyType = Just pytp,
              penaltyRate = Just pyrt,
              dayCountConvention = Just dcc,
              maturityDate,
              contractRole
            }
          ContractStatePoly
            { nt,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in case pytp of
                  PYTP_A -> o_rf_CURS * _r contractRole * pyrt
                  PYTP_N -> let c = o_rf_CURS * _r contractRole * y_sd_t * nt in c * pyrt
                  PYTP_I -> let c = o_rf_CURS * _r contractRole * y_sd_t * nt in c * _max _zero (ipnr - o_rf_RRMO)
                  PYTP_O -> undefined
        ----------------------
        -- Fee Payment (FP) --
        ----------------------
        -- POF_FP_*
        pof
          FP
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { dayCountConvention = Just dcc,
              feeBasis = Just feb,
              feeRate = Just fer,
              maturityDate,
              contractRole
            }
          ContractStatePoly
            { nt,
              feac,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in case feb of
                  FEB_A -> _r contractRole * o_rf_CURS * fer
                  FEB_N -> o_rf_CURS * fer * y_sd_t * nt * feac
        --------------------
        -- Purchase (PRD) --
        --------------------
        -- POF_PRD_PAM
        pof
          PRD
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = PAM,
              dayCountConvention = Just dcc,
              priceAtPurchaseDate = Just pprd,
              maturityDate,
              contractRole
            }
          ContractStatePoly
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in _negate $ o_rf_CURS * _r contractRole * (pprd + ipac + y_sd_t * ipnr * nt)
        -- POF_PRD_LAM
        -- POF_PRD_NAM
        -- POF_PRD_ANN
        pof
          PRD
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType,
              dayCountConvention = Just dcc,
              priceAtPurchaseDate = Just pprd,
              maturityDate,
              contractRole
            }
          ContractStatePoly
            { ipac,
              ipcb,
              ipnr,
              sd
            } | contractType `elem` [LAM, NAM, ANN] =
              let y_sd_t = _y dcc sd t maturityDate
               in _negate $ o_rf_CURS * _r contractRole * (pprd + ipac + y_sd_t * ipnr * ipcb)
        -- POF_PRD_STK
        -- POF_PRD_OPTNS
        -- POF_PRD_FUTUR
        -- POF_PRD_SWPPV
        -- POF_PRD_CEG
        pof
          PRD
          _
          ContractTermsPoly
            { contractType,
              priceAtPurchaseDate = Just pprd,
              contractRole
            }
          _ | contractType `elem` [STK, OPTNS, FUTUR, SWPPV, CEG] = _negate $ _r contractRole * pprd
        -- POF_PRD_COM
        pof
          PRD
          _
          ContractTermsPoly
            { contractType = COM,
              priceAtPurchaseDate = Just pprd,
              quantity = Just qt,
              contractRole
            }
          _ = _negate $ _r contractRole * pprd * qt
        ----------------------
        -- Termination (TD) --
        ----------------------
        -- POF_TD_PAM
        pof
          TD
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = PAM,
              dayCountConvention = Just dcc,
              priceAtTerminationDate = Just ptd,
              maturityDate,
              contractRole
            }
          ContractStatePoly
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in o_rf_CURS * _r contractRole * (ptd + ipac + y_sd_t * ipnr * nt)
        -- POF_TD_STK
        pof
          TD
          _
          ContractTermsPoly
            { contractType = STK,
              priceAtTerminationDate = Just ptd,
              contractRole
            }
          _ = _r contractRole * ptd
        -- POF_TD_SWPPV
        pof
          TD
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = SWPPV,
              priceAtTerminationDate = Just ptd
            }
          _ = o_rf_CURS * ptd
        -- POF_TD_COM
        pof
          TD
          _
          ContractTermsPoly
            { contractType = COM,
              priceAtTerminationDate = Just ptd,
              contractRole,
              quantity = Just qt
            }
          _ = _r contractRole * ptd * qt
        -- POF_TD_*
        pof
          TD
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { dayCountConvention = Just dcc,
              priceAtTerminationDate = Just ptd,
              maturityDate,
              contractRole
            }
          ContractStatePoly
            { ipac,
              ipcb,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in o_rf_CURS * _r contractRole * (ptd + ipac + y_sd_t * ipnr * ipcb)
        ---------------------------
        -- Interest Payment (IP) --
        ---------------------------
        -- POF_IP_PAM
        pof
          IP
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = PAM,
              dayCountConvention = Just dcc,
              maturityDate
            }
          ContractStatePoly
            { nt,
              isc,
              ipac,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in o_rf_CURS * isc * (ipac + y_sd_t * ipnr * nt)
        -- POF_IP_SWPPV
        pof
          IP
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = SWPPV,
              dayCountConvention = Just dcc,
              nominalInterestRate = Just ipnr',
              maturityDate
            }
          ContractStatePoly
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in o_rf_CURS * (ipac + y_sd_t * (ipnr' - ipnr) * nt)
        -- POF_IP_CLM
        pof
          IP
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = CLM,
              dayCountConvention = Just dcc,
              maturityDate
            }
          ContractStatePoly
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in o_rf_CURS * (ipac + y_sd_t * ipnr * nt)
        -- POF_IP_*
        pof
          IP
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { dayCountConvention = Just dcc,
              maturityDate
            }
          ContractStatePoly
            { isc,
              ipac,
              ipcb,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in o_rf_CURS * isc * (ipac + y_sd_t * ipnr * ipcb)
        ---------------------------------------
        -- Interest Payment Fixed Leg (IPFX) --
        ---------------------------------------
        -- POF_IPFX_SWPPV
        pof
          IPFX
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = SWPPV,
              dayCountConvention = Just dcc,
              nominalInterestRate = Just ipnr',
              maturityDate
            }
          ContractStatePoly
            { nt,
              ipac1 = Just ipac1',
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in o_rf_CURS * (ipac1' + y_sd_t * ipnr' * nt)
        ------------------------------------------
        -- Interest Payment Floating Leg (IPFL) --
        ------------------------------------------
        -- POF_IPFL_SWPPV
        pof
          IPFL
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = SWPPV
            }
          ContractStatePoly
            { nt,
              ipnr,
              ipac2 = Just ipac2',
              ipla = Just lastInterestPeriod
            } =
            o_rf_CURS * (ipac2' - lastInterestPeriod * ipnr * nt)
        ---------------------------
        -- Dividend Payment (DV) --
        ---------------------------
        -- POF_DV_*
        pof
          DV
          RiskFactorsPoly
            { o_rf_CURS,
              dv_payoff
            }
          ContractTermsPoly
            { contractType = STK,
              contractRole
            }
          _ = o_rf_CURS * _r contractRole * dv_payoff
        ----------------------
        -- Settlement (STD) --
        ----------------------
        -- POF_STD_OPTNS
        -- POF_STD_FUTUR
        pof
          STD
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType,
              contractRole
            }
          ContractStatePoly
            { xa = Just exerciseAmount
            } | contractType `elem` [OPTNS, FUTUR] = o_rf_CURS * _r contractRole * exerciseAmount
        -- POF_STD_CEG
        pof
          STD
          RiskFactorsPoly
            { o_rf_CURS
            }
          ContractTermsPoly
            { contractType = CEG
            }
          ContractStatePoly
            { xa = Just exerciseAmount,
              feac
            } = o_rf_CURS * (exerciseAmount + feac)
        -------------------------------
        -- Rate Reset (RR) --
        -------------------------------
        -- POF_RR_SWPPV
        pof
          RR
          _
          ContractTermsPoly
            { contractType = SWPPV
            }
          _ = _zero
        -------------
        -- Default --
        -------------
        pof _ _ _ _ = _zero
