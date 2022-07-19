{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS payoff functions -}
module Actus.Model.Payoff
  ( payoff
  , CtxPOF (..)
  )
where

import Actus.Domain (CT (..), ContractState (..), ContractTerms (..), EventType (..), FEB (..), PYTP (..),
                     RiskFactors (..), _r)
import Actus.Utility.YearFraction (yearFraction)
import Control.Monad.Reader (Reader, reader)
import Data.Time.LocalTime (LocalTime)

-- |The context for payoff functions
data CtxPOF a = CtxPOF
  { contractTerms :: ContractTerms a                         -- ^ Contract terms
  , riskFactors   :: EventType -> LocalTime -> RiskFactors a -- ^ Risk factors as a function of event type and time
  }

-- |The payoff function
payoff :: RealFrac a =>
     EventType           -- ^ Event type
  -> LocalTime           -- ^ Time
  -> ContractState a     -- ^ Contract state
  -> Reader (CtxPOF a) a -- ^ Updated contract state
payoff ev t st = reader payoff'
  where
    payoff' CtxPOF {..} = pof ev (riskFactors ev t) contractTerms st
      where
        pof ::
          RealFrac a =>
            EventType -> RiskFactors a -> ContractTerms a -> ContractState a -> a
        ----------------------------
        -- Initial Exchange (IED) --
        ----------------------------
        -- POF_IED_SWPPV
        pof
          IED
          _
          ContractTerms
            { contractType = SWPPV
            }
          _ = 0
        -- POF_IED_CLM
        pof
          IED
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = CLM,
              contractRole,
              notionalPrincipal = Just nt
            }
          _ = negate $ o_rf_CURS * _r contractRole * nt
        -- POF_IED_*
        pof
          IED
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { notionalPrincipal = Just nt,
              premiumDiscountAtIED = Just pdied,
              contractRole
            }
          _ = negate $ o_rf_CURS * _r contractRole * (nt + pdied)
        -- POF_IED_*
        pof
          IED
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { notionalPrincipal = Just nt,
              contractRole
            }
          _ = negate $ o_rf_CURS * _r contractRole * nt
        -------------------------------
        -- Principal Redemption (PR) --
        -------------------------------
        -- POF_PR_LAM
        pof
          PR
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = LAM,
              contractRole
            }
          ContractState
            { nt,
              nsc,
              prnxt
            } =
            let redemption = prnxt - _r contractRole * max 0 (abs prnxt - abs nt)
             in o_rf_CURS * _r contractRole * nsc * redemption
        -- POF_PR_NAM
        -- POF_PR_ANN
        pof
          PR
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType,
              dayCountConvention = Just dcc,
              maturityDate,
              contractRole
            }
          ContractState
            { nt,
              nsc,
              prnxt,
              ipac,
              ipcb,
              ipnr,
              sd
            }
            | contractType `elem` [NAM, ANN] =
              let timeFromLastEvent = yearFraction dcc sd t maturityDate
                  ra = prnxt - _r contractRole * (ipac + timeFromLastEvent * ipnr * ipcb)
                  r = ra - max 0 (ra - abs nt)
               in o_rf_CURS * _r contractRole * nsc * r
        -- POF_PR_SWPPV
        pof
          PR
          _
          ContractTerms
            { contractType = SWPPV
            }
          _ = 0
        -------------------
        -- Maturity (MD) --
        -------------------
        -- POF_MD_OPTNS
        -- POF_MD_SWPPV
        -- POF_MD_CEG
        pof
          MD
          _
          ContractTerms
            { contractType
            }
          _ | contractType `elem` [OPTNS, SWPPV, CEG] = 0
        -- POF_IED_*
        pof
          MD
          RiskFactors
            { o_rf_CURS
            }
          _
          ContractState
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
          RiskFactors
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
          RiskFactors
            { o_rf_CURS,
              o_rf_RRMO
            }
          ContractTerms
            { penaltyType = Just pytp,
              penaltyRate = Just pyrt,
              dayCountConvention = Just dcc,
              maturityDate,
              contractRole
            }
          ContractState
            { nt,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in case pytp of
                  PYTP_A -> o_rf_CURS * _r contractRole * pyrt
                  PYTP_N -> let c = o_rf_CURS * _r contractRole * timeFromLastEvent * nt in c * pyrt
                  PYTP_I -> let c = o_rf_CURS * _r contractRole * timeFromLastEvent * nt in c * max 0 (ipnr - o_rf_RRMO)
                  PYTP_O -> undefined
        ----------------------
        -- Fee Payment (FP) --
        ----------------------
        -- POF_FP_*
        pof
          FP
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { dayCountConvention = Just dcc,
              feeBasis = Just feb,
              feeRate = Just fer,
              maturityDate,
              contractRole
            }
          ContractState
            { nt,
              feac,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in case feb of
                  FEB_A -> _r contractRole * o_rf_CURS * fer
                  FEB_N -> o_rf_CURS * fer * timeFromLastEvent * nt * feac
        --------------------
        -- Purchase (PRD) --
        --------------------
        -- POF_PRD_PAM
        pof
          PRD
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = PAM,
              dayCountConvention = Just dcc,
              priceAtPurchaseDate = Just pprd,
              maturityDate,
              contractRole
            }
          ContractState
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in negate $ o_rf_CURS * _r contractRole * (pprd + ipac + timeFromLastEvent * ipnr * nt)
        -- POF_PRD_LAM
        -- POF_PRD_NAM
        -- POF_PRD_ANN
        pof
          PRD
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType,
              dayCountConvention = Just dcc,
              priceAtPurchaseDate = Just pprd,
              maturityDate,
              contractRole
            }
          ContractState
            { ipac,
              ipcb,
              ipnr,
              sd
            } | contractType `elem` [LAM, NAM, ANN] =
              let timeFromLastEvent = yearFraction dcc sd t maturityDate
               in negate $ o_rf_CURS * _r contractRole * (pprd + ipac + timeFromLastEvent * ipnr * ipcb)
        -- POF_PRD_STK
        -- POF_PRD_OPTNS
        -- POF_PRD_FUTUR
        -- POF_PRD_SWPPV
        -- POF_PRD_CEG
        pof
          PRD
          _
          ContractTerms
            { contractType,
              priceAtPurchaseDate = Just pprd,
              contractRole
            }
          _ | contractType `elem` [STK, OPTNS, FUTUR, SWPPV, CEG] = negate $ _r contractRole * pprd
        -- POF_PRD_COM
        pof
          PRD
          _
          ContractTerms
            { contractType = COM,
              priceAtPurchaseDate = Just pprd,
              quantity = Just qt,
              contractRole
            }
          _ = negate $ _r contractRole * pprd * qt
        ----------------------
        -- Termination (TD) --
        ----------------------
        -- POF_TD_PAM
        pof
          TD
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = PAM,
              dayCountConvention = Just dcc,
              priceAtTerminationDate = Just ptd,
              maturityDate,
              contractRole
            }
          ContractState
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in o_rf_CURS * _r contractRole * (ptd + ipac + timeFromLastEvent * ipnr * nt)
        -- POF_TD_STK
        pof
          TD
          _
          ContractTerms
            { contractType = STK,
              priceAtTerminationDate = Just ptd,
              contractRole
            }
          _ = _r contractRole * ptd
        -- POF_TD_SWPPV
        pof
          TD
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = SWPPV,
              priceAtTerminationDate = Just ptd
            }
          _ = o_rf_CURS * ptd
        -- POF_TD_COM
        pof
          TD
          _
          ContractTerms
            { contractType = COM,
              priceAtTerminationDate = Just ptd,
              contractRole,
              quantity = Just qt
            }
          _ = _r contractRole * ptd * qt
        -- POF_TD_*
        pof
          TD
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { dayCountConvention = Just dcc,
              priceAtTerminationDate = Just ptd,
              maturityDate,
              contractRole
            }
          ContractState
            { ipac,
              ipcb,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in o_rf_CURS * _r contractRole * (ptd + ipac + timeFromLastEvent * ipnr * ipcb)
        ---------------------------
        -- Interest Payment (IP) --
        ---------------------------
        -- POF_IP_PAM
        pof
          IP
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = PAM,
              dayCountConvention = Just dcc,
              maturityDate
            }
          ContractState
            { nt,
              isc,
              ipac,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in o_rf_CURS * isc * (ipac + timeFromLastEvent * ipnr * nt)
        -- POF_IP_SWPPV
        pof
          IP
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = SWPPV,
              dayCountConvention = Just dcc,
              nominalInterestRate = Just ipnr',
              maturityDate
            }
          ContractState
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in o_rf_CURS * (ipac + timeFromLastEvent * (ipnr' - ipnr) * nt)
        -- POF_IP_CLM
        pof
          IP
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = CLM,
              dayCountConvention = Just dcc,
              maturityDate
            }
          ContractState
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in o_rf_CURS * (ipac + timeFromLastEvent * ipnr * nt)
        -- POF_IP_*
        pof
          IP
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { dayCountConvention = Just dcc,
              maturityDate
            }
          ContractState
            { isc,
              ipac,
              ipcb,
              ipnr,
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in o_rf_CURS * isc * (ipac + timeFromLastEvent * ipnr * ipcb)
        ---------------------------------------
        -- Interest Payment Fixed Leg (IPFX) --
        ---------------------------------------
        -- POF_IPFX_SWPPV
        pof
          IPFX
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = SWPPV,
              dayCountConvention = Just dcc,
              nominalInterestRate = Just ipnr',
              maturityDate
            }
          ContractState
            { nt,
              ipac1 = Just ipac1',
              sd
            } =
            let timeFromLastEvent = yearFraction dcc sd t maturityDate
             in o_rf_CURS * (ipac1' + timeFromLastEvent * ipnr' * nt)
        ------------------------------------------
        -- Interest Payment Floating Leg (IPFL) --
        ------------------------------------------
        -- POF_IPFL_SWPPV
        pof
          IPFL
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = SWPPV
            }
          ContractState
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
          RiskFactors
            { o_rf_CURS,
              dv_payoff
            }
          ContractTerms
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
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType,
              contractRole
            }
          ContractState
            { xa = Just exerciseAmount
            } | contractType `elem` [OPTNS, FUTUR] = o_rf_CURS * _r contractRole * exerciseAmount
        -- POF_STD_CEG
        pof
          STD
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = CEG
            }
          ContractState
            { xa = Just exerciseAmount,
              feac
            } = o_rf_CURS * (exerciseAmount + feac)
        -- POF_STD_CEC
        pof
          STD
          RiskFactors
            { o_rf_CURS
            }
          ContractTerms
            { contractType = CEC
            }
          ContractState
            { xa = Just exerciseAmount,
              feac
            } = o_rf_CURS * (exerciseAmount + feac)
        -------------------------------
        -- Rate Reset (RR) --
        -------------------------------
        -- POF_RR_SWPPV
        -- POF_RR_CLM
        pof
          RR
          _
          ContractTerms
          { contractType
          }
          _ | contractType `elem` [SWPPV, CLM] = 0
        -------------
        -- Default --
        -------------
        pof _ _ _ _ = 0
