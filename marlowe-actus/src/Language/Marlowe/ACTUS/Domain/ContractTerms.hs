{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.Marlowe.ACTUS.Domain.ContractTerms where

import Control.Applicative ((<|>))
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import qualified Language.Marlowe as Marlowe (Observation, Value)

-- |ContractType
data CT = PAM   -- ^ Principal at maturity
        | LAM   -- ^ Linear amortizer
        | NAM   -- ^ Negative amortizer
        | ANN   -- ^ Annuity
        | STK   -- ^ Stock
        | OPTNS -- ^ Option
        | FUTUR -- ^ Future
        deriving stock (Show, Read, Eq, Generic)
        deriving anyclass (FromJSON, ToJSON)

-- |ContractRole
data CR = CR_RPA -- ^ Real position asset
        | CR_RPL -- ^ Real position liability
        | CR_CLO -- ^ Role of a collateral
        | CR_CNO -- ^ Role of a close-out-netting
        | CR_COL -- ^ Role of an underlying to a collateral
        | CR_LG  -- ^ Long position
        | CR_ST  -- ^ Short position
        | CR_BUY -- ^ Protection buyer
        | CR_SEL -- ^ Protection seller
        | CR_RFL -- ^ Receive first leg
        | CR_PFL -- ^ Pay first leg
        | CR_RF  -- ^ Receive fix leg
        | CR_PF  -- ^ Pay fix leg
        deriving stock (Show, Read, Eq, Generic)
        deriving anyclass (FromJSON, ToJSON)

-- |DayCountConvention
data DCC = DCC_A_AISDA     -- ^ Actual/Actual ISDA
         | DCC_A_360       -- ^ Actual/360
         | DCC_A_365       -- ^ Actual/365
         | DCC_E30_360ISDA -- ^ 30E/360 ISDA
         | DCC_E30_360     -- ^ 30E/360
         | DCC_B_252       -- ^ Business / 252
         deriving stock (Show, Read, Eq, Generic)
         deriving anyclass (FromJSON, ToJSON)

-- |EndOfMonthConvention
data EOMC = EOMC_EOM -- ^ End of month
          | EOMC_SD  -- ^ Same day
          deriving stock (Show, Read, Eq, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |BusinessDayConvention
data BDC = BDC_NULL -- ^ No shift
         | BDC_SCF  -- ^ Shift/calculate following
         | BDC_SCMF -- ^ Shift/calculate modified following
         | BDC_CSF  -- ^ Calculate/shift following
         | BDC_CSMF -- ^ Calculate/shift modified following
         | BDC_SCP  -- ^ Shift/calculate preceding
         | BDC_SCMP -- ^ Shift/calculate modified preceding
         | BDC_CSP  -- ^ Calculate/shift preceding
         | BDC_CSMP -- ^ Calculate/shift modified preceding
         deriving stock (Show, Read, Eq, Generic)
         deriving anyclass (FromJSON, ToJSON)

data Calendar = CLDR_MF -- ^ Monday to Friday
              | CLDR_NC -- ^ No calendar
              deriving stock (Show, Read, Eq, Generic)
              deriving anyclass (FromJSON, ToJSON)

data ScheduleConfig = ScheduleConfig
  { calendar              :: Maybe Calendar
  , endOfMonthConvention  :: Maybe EOMC
  , businessDayConvention :: Maybe BDC
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- |ContractPerformance
data PRF = PRF_PF -- ^ Performant
         | PRF_DL -- ^ Delayed
         | PRF_DQ -- ^ Delinquent
         | PRF_DF -- ^ Default
         deriving stock (Show, Read, Eq, Generic)
              deriving anyclass (FromJSON, ToJSON)

-- |FeeBasis
data FEB = FEB_A -- ^ Absolute value
         | FEB_N -- ^ Notional of underlying
         deriving stock (Show, Read, Eq, Generic)
              deriving anyclass (FromJSON, ToJSON)

-- |InterestCalculationBase
data IPCB = IPCB_NT    -- ^ Calculation base always equals to NT
          | IPCB_NTIED -- ^ Notional remains constant amount as per IED
          | IPCB_NTL   -- ^ Calculation base is notional base laged
          deriving stock (Show, Read, Eq, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |ScalingEffect
data SCEF = SE_OOO -- ^ No scaling
          | SE_IOO -- ^ Only interest payments scaled
          | SE_ONO -- ^ Only nominal payments scaled
          | SE_OOM -- ^ Only maximum deferred amount scaled
          | SE_INO -- ^ Interest and nominal payments scaled
          | SE_ONM -- ^ Nominal and maximum deferred amount scaled
          | SE_IOM -- ^ Interest and maximum deferred amount scaled
          | SE_INM -- ^ Interest, nominal and maximum deferred amount scaled
          deriving stock (Show, Read, Eq, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |PenaltyType
data PYTP = PYTP_A -- ^ Absolute
          | PYTP_N -- ^ Nominal rate
          | PYTP_I -- ^ Current interest rate differential
          | PYTP_O -- ^ No penalty
          deriving stock (Show, Read, Eq, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |Option Type
data OPTP = OPTP_C  -- ^ Call Option
          | OPTP_P  -- ^ Put Option
          | OPTP_CP -- ^ Call-Put Option
          deriving stock (Show, Read, Eq, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |Option Exercise Type
data OPXT = OPXT_E -- ^ European
          | OPXT_B -- ^ Bermudan
          | OPXT_A -- ^ American
          deriving stock (Show, Read, Eq, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |Settlement
data DS = DS_S -- ^ Cash Settlement
        | DS_D -- ^ Physical Settlement
          deriving stock (Show, Read, Eq, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |PrepaymentEffect
data PPEF = PPEF_N -- ^ No prepayment
          | PPEF_A -- ^ Prepayment allowed, prepayment results in reduction of PRNXT while MD remains
          | PPEF_M -- ^ Prepayment allowed, prepayment results in reduction of MD while PRNXT remains
          deriving stock (Show, Read, Eq, Ord, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |CyclePeriod
data Period = P_D -- ^ Day
            | P_W -- ^ Week
            | P_M -- ^ Month
            | P_Q -- ^ Quarter
            | P_H -- ^ Half year
            | P_Y -- ^ Year
            deriving stock (Show, Read, Eq, Ord, Generic)
            deriving anyclass (FromJSON, ToJSON)

-- |CycleStub
data Stub = ShortStub -- ^ Short last stub
          | LongStub  -- ^ Long last stub
          deriving stock (Show, Eq, Ord, Generic)
          deriving anyclass (FromJSON, ToJSON)

-- |Cycle
data Cycle = Cycle
  { n             :: Integer
  , p             :: Period
  , stub          :: Stub
  , includeEndDay :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- For applicability failures
data TermValidationError =
    Required String
    | NotApplicable String
    deriving stock (Eq)

instance Show TermValidationError where
    show (Required s)      = "Missing required term: " ++ s
    show (NotApplicable s) = "Term not applicable to contract: " ++ s

data Assertions = Assertions
  { context    :: AssertionContext
  , assertions :: [Assertion]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AssertionContext = AssertionContext
  { rrmoMin :: Double
  , rrmoMax :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Assertion = NpvAssertionAgainstZeroRiskBond
  { zeroRiskInterest :: Double
  , expectedNpv      :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- |Reference type
data ReferenceType = CNT
                   | CID
                   | MOC
                   | EID
                   | CST
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- |Reference role
data ReferenceRole = UDL  -- ^ Underlying
                   | FIL  -- ^ First Leg
                   | SEL  -- ^ Second Leg
                   | COVE -- ^ Convered Contract
                   | COVI -- ^ Covering Contract
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- |Market object code
type MarketObjectCode = String

-- |Contract structure
data ContractStructure = ContractStructure
  {
    marketObjectCode :: MarketObjectCode
  , referenceType    :: ReferenceType
  , referenceRole    :: ReferenceRole
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

{-| ACTUS contract terms and attributes are defined in
    https://github.com/actusfrf/actus-dictionary/blob/master/actus-dictionary-terms.json
-}
data ContractTermsPoly a b = ContractTermsPoly
  { -- General
    contractId                               :: String
  , contractType                             :: CT
  , contractStructure                        :: [ContractStructure]
  , contractRole                             :: CR
  , settlementCurrency                       :: Maybe String

  -- Calendar
  , initialExchangeDate                      :: Maybe b          -- ^ Initial Exchange Date
  , dayCountConvention                       :: Maybe DCC        -- ^ Day Count Convention
  , scheduleConfig                           :: ScheduleConfig

  -- Contract Identification
  , statusDate                               :: b                -- ^ Status Date

  -- Counterparty
  , contractPerformance                      :: Maybe PRF        -- ^ Contract Performance

  -- Fees
  , cycleOfFee                               :: Maybe Cycle      -- ^ Cycle Of Fee
  , cycleAnchorDateOfFee                     :: Maybe b          -- ^ Cycle Anchor Date Of Fee
  , feeAccrued                               :: Maybe a          -- ^ Fee Accrued
  , feeBasis                                 :: Maybe FEB        -- ^ Fee Basis
  , feeRate                                  :: Maybe a          -- ^ Fee Rate

  -- Interest
  , cycleAnchorDateOfInterestPayment         :: Maybe b          -- ^ Cycle Anchor Date Of Interest Payment
  , cycleOfInterestPayment                   :: Maybe Cycle      -- ^ Cycle Of Interest Payment
  , accruedInterest                          :: Maybe a          -- ^ Accrued Interest
  , capitalizationEndDate                    :: Maybe b          -- ^ Capitalization End Date
  , cycleAnchorDateOfInterestCalculationBase :: Maybe b          -- ^ Cycle Anchor Date Of Interest Calculation Base
  , cycleOfInterestCalculationBase           :: Maybe Cycle      -- ^ Cycle Of Interest Calculation Base
  , interestCalculationBase                  :: Maybe IPCB       -- ^ Interest Calculation Base
  , interestCalculationBaseA                 :: Maybe a          -- ^ Interest Calculation Base Amount
  , nominalInterestRate                      :: Maybe a          -- ^ Nominal Interest Rate
  , interestScalingMultiplier                :: Maybe a          -- ^ Interest Scaling Multiplier

  -- Dates
  , maturityDate                             :: Maybe b          -- ^ Maturity Date
  , amortizationDate                         :: Maybe b          -- ^ Amortization Date
  , exerciseDate                             :: Maybe b          -- ^ Exercise Date

  -- Notional Principal
  , notionalPrincipal                        :: Maybe a          -- ^ Notional Principal
  , premiumDiscountAtIED                     :: Maybe a          -- ^ Premium Discount At IED
  , cycleAnchorDateOfPrincipalRedemption     :: Maybe b          -- ^ Cycle Anchor Date Of Principal Redemption
  , cycleOfPrincipalRedemption               :: Maybe Cycle      -- ^ Cycle Of Principal Redemption
  , nextPrincipalRedemptionPayment           :: Maybe a          -- ^ Next Principal Redemption Payment
  , purchaseDate                             :: Maybe b          -- ^ Purchase Date
  , priceAtPurchaseDate                      :: Maybe a          -- ^ Price At Purchase Date
  , terminationDate                          :: Maybe b          -- ^ Termination Date
  , priceAtTerminationDate                   :: Maybe a          -- ^ Price At Termination Date

  -- Scaling Index
  , scalingIndexAtStatusDate                 :: Maybe a          -- ^ Scaling Index At Status Date
  , cycleAnchorDateOfScalingIndex            :: Maybe b          -- ^ Cycle Anchor Date Of Scaling Index
  , cycleOfScalingIndex                      :: Maybe Cycle      -- ^ Cycle Of Scaling Index
  , scalingEffect                            :: Maybe SCEF       -- ^ Scaling Effect
  , scalingIndexAtContractDealDate           :: Maybe a          -- ^ Scaling Index At Contract Deal Date
  , marketObjectCodeOfScalingIndex           :: Maybe String     -- ^ Market Object Code Of Scaling Index
  , notionalScalingMultiplier                :: Maybe a          -- ^ Notional Scaling Multiplier

  -- Optionality
  , cycleOfOptionality                       :: Maybe Cycle      -- ^ Cycle Of Optionality
  , cycleAnchorDateOfOptionality             :: Maybe b          -- ^ Cycle Anchor Date Of Optionality
  , optionType                               :: Maybe OPTP       -- ^ Option Type
  , optionStrike1                            :: Maybe a          -- ^ Option Strike 1
  , optionExerciseType                       :: Maybe OPXT       -- ^ Option Exercise Type

  -- Settlement
  , settlementPeriod                         :: Maybe Cycle      -- ^ Settlement Period
  , deliverySettlement                       :: Maybe DS         -- ^ Delivery Settlement
  , exerciseAmount                           :: Maybe a          -- ^ Exercise Amount
  , futuresPrice                             :: Maybe a          -- ^ Futures Price

  -- Penalty
  , penaltyRate                              :: Maybe a          -- ^ Penalty Rate
  , penaltyType                              :: Maybe PYTP       -- ^ Penalty Type
  , prepaymentEffect                         :: Maybe PPEF       -- ^ Prepayment Effect

  -- Rate Reset
  , cycleOfRateReset                         :: Maybe Cycle      -- ^ Cycle Of Rate Reset
  , cycleAnchorDateOfRateReset               :: Maybe b          -- ^ Cycle Anchor Date Of Rate Reset
  , nextResetRate                            :: Maybe a          -- ^ Next Reset Rate
  , rateSpread                               :: Maybe a          -- ^ Rate Spread
  , rateMultiplier                           :: Maybe a          -- ^ Rate Multiplier
  , periodFloor                              :: Maybe a          -- ^ Period Floor
  , periodCap                                :: Maybe a          -- ^ Period Cap
  , lifeCap                                  :: Maybe a          -- ^ Life Cap
  , lifeFloor                                :: Maybe a          -- ^ Life Floor
  , marketObjectCodeOfRateReset              :: Maybe String     -- ^ Market Object Code Of Rate Reset

  -- Dividend
  , cycleOfDividend                          :: Maybe Cycle      -- ^ Cycle Of Dividend
  , cycleAnchorDateOfDividend                :: Maybe b          -- ^ Cycle Anchor Date Of Dividend
  , nextDividendPaymentAmount                :: Maybe a          -- ^ Next Dividend Payment Amount

  , enableSettlement                         :: Bool             -- ^ Enable settlement currency
  , constraints                              :: Maybe Assertions -- ^ Assertions
  , collateralAmount                         :: Integer          -- ^ Collateral Amount
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type ContractTerms = ContractTermsPoly Double LocalTime
type ContractTermsMarlowe = ContractTermsPoly (Marlowe.Value Marlowe.Observation) (Marlowe.Value Marlowe.Observation)

setDefaultContractTermValues :: ContractTerms -> ContractTerms
setDefaultContractTermValues ct@ContractTermsPoly {..} =
  ct
    { scheduleConfig =
        scheduleConfig
          { endOfMonthConvention = applyDefault EOMC_SD (endOfMonthConvention scheduleConfig),
            businessDayConvention = applyDefault BDC_NULL (businessDayConvention scheduleConfig),
            calendar = applyDefault CLDR_NC (calendar scheduleConfig)
          },
      contractPerformance            = applyDefault PRF_PF contractPerformance,
      interestCalculationBase        = applyDefault IPCB_NT interestCalculationBase,
      premiumDiscountAtIED           = applyDefault 0.0 premiumDiscountAtIED,
      scalingEffect                  = applyDefault SE_OOO scalingEffect,
      penaltyRate                    = applyDefault 0.0 penaltyRate,
      penaltyType                    = applyDefault PYTP_O penaltyType,
      prepaymentEffect               = applyDefault PPEF_N prepaymentEffect,
      rateSpread                     = applyDefault 0.0 rateSpread,
      rateMultiplier                 = applyDefault 1.0 rateMultiplier,
      feeAccrued                     = applyDefault 0.0 feeAccrued,
      feeRate                        = applyDefault 0.0 feeRate,
      accruedInterest                = applyDefault 0.0 accruedInterest,
      nominalInterestRate            = applyDefault 0.0 nominalInterestRate,
      priceAtPurchaseDate            = applyDefault 0.0 priceAtPurchaseDate,
      priceAtTerminationDate         = applyDefault 0.0 priceAtTerminationDate,
      scalingIndexAtContractDealDate = applyDefault 0.0 scalingIndexAtContractDealDate,
      periodFloor                    = applyDefault (- infinity) periodFloor,
      periodCap                      = applyDefault infinity periodCap,
      lifeCap                        = applyDefault infinity lifeCap,
      lifeFloor                      = applyDefault (- infinity) lifeFloor,
      interestCalculationBaseA       = applyDefault 0.0 interestCalculationBaseA
    }
  where
    infinity :: Double
    infinity = 1 / 0 :: Double

    applyDefault :: a -> Maybe a -> Maybe a
    applyDefault v o = o <|> Just v
