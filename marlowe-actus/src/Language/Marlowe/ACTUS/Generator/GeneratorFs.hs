{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-| = ACTUS Generator

Given ACTUS contract terms a Marlowe contract is generated.

In 'genFsContract' risk factors are added to the Marlowe contract, i.e. will
be observed during the life time of the contract

-}

module Language.Marlowe.ACTUS.Generator.GeneratorFs
  ( genFsContract )
where

import Control.Monad.Reader
import Data.Foldable (foldrM)
import qualified Data.List as L (zip5)
import Data.Maybe (maybeToList)
import Data.Monoid (Endo (Endo, appEndo))
import Data.String (IsString (fromString))
import Data.Time (LocalTime)
import Data.Validation (Validation (..))
import Language.Marlowe (Action (..), Bound (..), Case (..), ChoiceId (..), Contract (..), Observation (..), Party (..),
                         Slot (..), Value (..), ValueId (ValueId))
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactors, RiskFactorsMarlowe,
                                                     RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractState, ContractStateMarlowe, ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (Assertion (..), AssertionContext (..), Assertions (..),
                                                    ContractTerms, ContractTermsPoly (..), PRF (..),
                                                    TermValidationError (..))
import qualified Language.Marlowe.ACTUS.Domain.Ops as O (ActusNum (..), YearFractionOps (_y))
import Language.Marlowe.ACTUS.Domain.Schedule (CashFlow (..), ShiftedDay (..), calculationDay)
import Language.Marlowe.ACTUS.Generator.Analysis (genProjectedCashflows)
import Language.Marlowe.ACTUS.Generator.Generator (invoice)
import Language.Marlowe.ACTUS.Generator.MarloweCompat (constnt, letval, letval', marloweTime, timeToSlotNumber,
                                                       toMarloweFixedPoint, useval)
import Language.Marlowe.ACTUS.Model.APPL.Applicability (validateTerms)
import Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel (initializeState)
import Language.Marlowe.ACTUS.Model.POF.PayoffFs (payoffFs)
import Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule (schedule)
import Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule as S (maturity)
import Language.Marlowe.ACTUS.Model.STF.StateTransition (CtxSTF (..))
import Language.Marlowe.ACTUS.Model.STF.StateTransitionFs as FS (stateTransition)
import Ledger.Value (TokenName (TokenName))

-- |'genFsContract' validatate the applicabilty of the contract terms in order
-- to genereate a Marlowe contract with risk factors observed at a given point
-- in time
genFsContract ::
     (EventType -> LocalTime -> RiskFactors)   -- ^ Risk factors per event and time
  -> ContractTerms                             -- ^ ACTUS contract terms
  -> Validation [TermValidationError] Contract -- ^ Marlowe contract or applicabilty errors
genFsContract rf = fmap (genFsContract' rf) . validateTerms

genFsContract' ::
  (EventType -> LocalTime -> RiskFactors) ->
  ContractTerms ->
  Contract
genFsContract' rf ct =
  let gen :: (CashFlow, LocalTime, EventType, Slot, Integer) -> Contract -> Reader (CtxSTF Double LocalTime) Contract
      gen (cf, sd, ev, date, i) cont =
        let t :: LocalTime
            t = cashCalculationDay cf

            comment :: EventType -> Contract -> Contract
            comment IED = letval "IED" i (constnt 0)
            comment MD  = letval "MD" i (constnt 0)
            comment IP  = letval ("IP:" ++ show t ++ show sd) i (constnt 0)
            comment RR  = letval ("RR:" ++ show t) i (constnt 0)
            comment FP  = letval ("FP:" ++ show t) i (constnt 0)
            comment _   = id

            oracle_i = let ac = context <$> constraints ct in inquiryFs ev ("_" ++ show i) date "oracle" ac
            st_i = stateAt i sd
            rf_i = riskFactorAt i
            pof_i c = maybe c (payoff i cf date c) (payoffFs ev rf_i ct st_i sd t)
         in do stf <- stateToContract i <$> FS.stateTransition ev rf_i sd t st_i
               return $ oracle_i $ comment ev $ stf $ pof_i cont

      scheduleAcc :: Reader (CtxSTF Double LocalTime) Contract
      scheduleAcc =
        let projectedCashflows = genProjectedCashflows rf ct
            eventTypesOfCashflows = cashEvent <$> projectedCashflows
            paymentDayCashflows = Slot . timeToSlotNumber . cashPaymentDay <$> projectedCashflows
            previousDates = statusDate ct : (cashCalculationDay <$> projectedCashflows)
         in foldrM gen (postProcess Close) $
              L.zip5 projectedCashflows previousDates eventTypesOfCashflows paymentDayCashflows [1 ..]
   in runReader (stateInitialisation <$> initializeState <*> scheduleAcc) initCtx
  where
    fpSchedule, prSchedule, ipSchedule :: [LocalTime]
    fpSchedule = calculationDay <$> schedule FP ct
    prSchedule = calculationDay <$> schedule PR ct
    ipSchedule = calculationDay <$> schedule IP ct

    initCtx :: CtxSTF Double LocalTime
    initCtx = CtxSTF ct fpSchedule prSchedule ipSchedule (S.maturity ct)

    stateToContract :: Integer -> ContractStateMarlowe -> Contract -> Contract
    stateToContract i ContractStatePoly {..} =
      letval' "tmd" i tmd
        . letval "nt" i nt
        . letval "ipnr" i ipnr
        . letval "ipac" i ipac
        . letval "feac" i feac
        . letval "nsc" i nsc
        . letval "isc" i isc
        . letval "sd" i sd
        . letval "prnxt" i prnxt
        . letval "ipcb" i ipcb
        . letval' "xa" i xa
        . letval' "xd" i xd

    stateInitialisation :: ContractState -> Contract -> Contract
    stateInitialisation ContractStatePoly {..} =
      letval' "tmd" 0 (marloweTime <$> tmd)
        . letval "nt" 0 (constnt nt)
        . letval "ipnr" 0 (constnt ipnr)
        . letval "ipac" 0 (constnt ipac)
        . letval "feac" 0 (constnt feac)
        . letval "nsc" 0 (constnt nsc)
        . letval "isc" 0 (constnt isc)
        . letval "sd" 0 (marloweTime sd)
        . letval "prnxt" 0 (constnt prnxt)
        . letval "ipcb" 0 (constnt ipcb)
        . letval' "xa" 0 (constnt <$> xa)
        . letval' "xd" 0 (marloweTime <$> xd)

    stateAt :: Integer -> LocalTime -> ContractStateMarlowe
    stateAt i sd =
      ContractStatePoly
        { nsc = useval "nsc" $ pred i,
          nt = useval "nt" $ pred i,
          isc = useval "isc" $ pred i,
          ipac = useval "ipac" $ pred i,
          feac = useval "feac" $ pred i,
          ipnr = useval "ipnr" $ pred i,
          ipcb = useval "ipcb" $ pred i,
          xa = Just $ useval "xa" $ pred i,
          xd = Just $ useval "xd" $ pred i,
          prnxt = useval "prnxt" $ pred i,
          tmd = Just $ useval "tmd" i,
          prf = PRF_DF,
          sd = useval "sd" (timeToSlotNumber sd)
        }

    riskFactorAt :: Integer -> RiskFactorsMarlowe
    riskFactorAt i =
      RiskFactorsPoly
        { o_rf_CURS = useval "o_rf_CURS" i,
          o_rf_RRMO = useval "o_rf_RRMO" i,
          o_rf_SCMO = useval "o_rf_SCMO" i,
          pp_payoff = useval "pp_payoff" i,
          xd_payoff = useval "xd_payoff" i,
          dv_payoff = useval "dv_payoff" i
        }

    payoffAt :: Show a => a -> ValueId
    payoffAt t = ValueId $ fromString $ "payoff_" ++ show t

    postProcess :: Contract -> Contract
    postProcess cont =
      let ctr = constraints ct
          toAssert = genZeroRiskAssertions ct <$> (assertions =<< maybeToList ctr)
          compose = appEndo . mconcat . map Endo
       in compose toAssert cont

    payoff i cf date cont p =
      Let (payoffAt i) p $
        if amount cf > 0.0
          then
            invoice
              "party"
              "counterparty"
              (UseValue $ payoffAt i)
              date
              cont
          else
            if amount cf < 0.0
              then
                invoice
                  "counterparty"
                  "party"
                  (NegValue $ UseValue $ payoffAt i)
                  date
                  cont
              else cont

    inquiryFs :: EventType -> String -> Slot -> String -> Maybe AssertionContext -> Contract -> Contract
    inquiryFs ev timePostfix date oracle context continue =
      let oracleRole = Role $ TokenName $ fromString oracle

          letTemplate inputChoiceId inputOwner cont =
            Let
              (ValueId inputChoiceId)
              (ChoiceValue (ChoiceId inputChoiceId inputOwner))
              cont

          inputTemplate inputChoiceId inputOwner inputBound cont =
            When
              [ Case (Choice (ChoiceId inputChoiceId inputOwner) inputBound) $
                  letTemplate inputChoiceId inputOwner cont
              ]
              date
              Close

          inferBounds name ctx = case (name, ctx) of
            ("o_rf_RRMO", Just AssertionContext {..}) ->
              [Bound (toMarloweFixedPoint rrmoMin) (toMarloweFixedPoint rrmoMax)]
            _ -> [Bound 0 maxPseudoDecimalValue]

          riskFactorInquiry name =
            inputTemplate
              (fromString (name ++ timePostfix))
              oracleRole
              (inferBounds name context)

          riskFactorsInquiryEv AD = id
          riskFactorsInquiryEv SC = riskFactorInquiry "o_rf_SCMO"
          riskFactorsInquiryEv RR = riskFactorInquiry "o_rf_RRMO"
          riskFactorsInquiryEv DV = riskFactorInquiry "o_rf_CURS" . riskFactorInquiry "dv_payoff"
          riskFactorsInquiryEv PP = riskFactorInquiry "o_rf_CURS" . riskFactorInquiry "pp_payoff"
          riskFactorsInquiryEv STD = riskFactorInquiry "o_rf_CURS" . riskFactorInquiry "xd_payoff"
          riskFactorsInquiryEv _ =
            if enableSettlement ct
              then riskFactorInquiry "o_rf_CURS"
              else Let (ValueId (fromString ("o_rf_CURS" ++ timePostfix))) (constnt 1.0)
       in riskFactorsInquiryEv ev continue

    maxPseudoDecimalValue :: Integer
    maxPseudoDecimalValue = 100000000000000

    genZeroRiskAssertions :: ContractTerms -> Assertion -> Contract -> Contract
    genZeroRiskAssertions terms@ContractTermsPoly {dayCountConvention = Just dcc, ..} NpvAssertionAgainstZeroRiskBond {..} continue =
      let cfs = genProjectedCashflows rf terms

          dateToYearFraction :: LocalTime -> Double
          dateToYearFraction dt = O._y dcc statusDate dt maturityDate

          dateToDiscountFactor dt = (1 O.- zeroRiskInterest) ** dateToYearFraction dt

          accumulateAndDiscount :: Value Observation -> (CashFlow, Integer) -> Value Observation
          accumulateAndDiscount acc (cf, t) =
            let discountFactor = dateToDiscountFactor $ cashCalculationDay cf
                sign x = if amount cf < 0.0 then NegValue x else x
             in constnt discountFactor O.* (sign $ useval "payoff" t) O.+ acc

          npv = foldl accumulateAndDiscount (constnt 0) (zip cfs [1 ..])
       in Assert (ValueLT (constnt expectedNpv) npv) continue
    genZeroRiskAssertions _ _ c = c
