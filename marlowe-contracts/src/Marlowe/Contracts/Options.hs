{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Marlowe.Contracts.Options
  (
  -- * Options
    option
  , OptionType(..)
  , ExerciseType(..)
  -- * Option Strategies
  -- ** Fully collateralized
  , coveredCall
  -- ** Partially collateralized
  , callSpread
  -- ** Not collateralized
  , barrierOption
  , strangle
  , straddle
  )
where

import Language.Marlowe.Extended
import Marlowe.Contracts.Common

-- |Option type
data OptionType =
      Call -- ^ Call, the right to buy the Option
    | Put  -- ^ Put, the right to sell the Option
  deriving Show

-- |Exercise type
data ExerciseType =
      European -- ^ Execution at expiry
    | American -- ^ Execution anytime before expiry
  deriving Show

-- |Barrier type
data BarrierType =
      DownAndIn  -- ^ Knock in
    | DownAndOut -- ^ Knock out
    | UpAndIn    -- ^ Knock in
    | UpAndOut   -- ^ Knock out

-- |An /option/ is a financial instrument that is based on the value of underlying
-- securities. An options contract offers the buyer the opportunity but not
-- the obligation to buy or sell the underlying asset.
option ::
     ExerciseType   -- ^ Exercise Type
  -> OptionType     -- ^ Type of Option
  -> Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> (Token, Value) -- ^ Underlying
  -> (Token, Value) -- ^ Strike
  -> Timeout        -- ^ Expiry
  -> Timeout        -- ^ Settlement date
  -> Contract       -- ^ Option Contract
option American optionType buyer seller priceFeed asset strike expiry settlement =
  exercise
    optionType
    buyer
    seller
    priceFeed
    asset
    strike
    settlement
    expiry
    Close
option European optionType buyer seller priceFeed asset strike expiry settlement =
  waitUntil expiry $
    exercise
      optionType
      buyer
      seller
      priceFeed
      asset
      strike
      settlement
      settlement
      Close

-- |Choose whether to exercise an option and transfer the assets or close
-- the contract
exercise ::
     OptionType     -- ^ Type of Option
  -> Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for underlying
  -> (Token, Value) -- ^ Underlying
  -> (Token, Value) -- ^ Strike
  -> Timeout        -- ^ Timeout
  -> Timeout        -- ^ Expiry
  -> Contract       -- ^ Continuation
  -> Contract       -- ^ Contract
exercise Call buyer seller Nothing asset strike timeout =
  When
    [ choose (ChoiceId "Exercise Call" buyer) Close
    $ depositAndPay buyer seller strike asset timeout
    ]
exercise Call buyer seller (Just choiceId) asset strike timeout =
  When
    [ chooseOracle choiceId strike ValueLT Close
    $ depositAndPay buyer seller strike asset timeout
    ]
exercise Put buyer seller Nothing asset strike timeout =
  When
    [ choose (ChoiceId "Exercise Put" buyer) Close
    $ depositAndPay buyer seller asset strike timeout
    ]
exercise Put buyer seller (Just choiceId) asset strike timeout =
  When
    [ chooseOracle choiceId strike ValueGT Close
    $ depositAndPay buyer seller asset strike timeout
    ]

-- |Deposit an asset and swap
depositAndPay ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> (Token, Value) -- ^ Underlying asset
  -> (Token, Value) -- ^ Strike price
  -> Timeout        -- ^ Timeout for deposit
  -> Contract       -- ^ Contract
depositAndPay buyer seller asset strike timeout =
    deposit buyer buyer asset timeout Close
  $ pay seller buyer strike
  $ pay buyer seller asset
    Close

-- |Choosing explicitly
choose ::
     ChoiceId -- ^ Choice id, references the party to chose
  -> Contract -- ^ Continuation Contract if 0 chosen
  -> Contract -- ^ Continuation Contract if 1 chosen
  -> Case     -- ^ Case expression with continuation
choose choiceId continuation0 continuation1 =
  Case
    (Choice choiceId [Bound 0 1])
    (If (ValueEQ (ChoiceValue choiceId) (Constant 0)) continuation0 continuation1)

-- |Choosing based on a price feed
chooseOracle ::
     ChoiceId                        -- ^ Price feed
  -> (Token,Value)                   -- ^ Strike price
  -> (Value -> Value -> Observation) -- ^ Comparison function
  -> Contract                        -- ^ Continuation Contract if condition holds
  -> Contract                        -- ^ Continuation Contract if condition does not hold
  -> Case                            -- ^ Case expression with continuation
chooseOracle choiceId (_, strike) comparision continuation0 continuation1 =
   Case
      (Choice choiceId [Bound 0 100_000_000_000])
      (If (ChoiceValue choiceId `comparision` strike) continuation0 continuation1)

-- |A /Covered Call/ is an option strategy constructed by writing a call on a token
-- and in addition providing the token as cover/collateral as part of the contract
coveredCall ::
     Party          -- ^ Issuer of the covered Call
  -> Party          -- ^ Counter-party
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Strike price (in currency)
  -> Value          -- ^ Amount of underlying tokens per contract
  -> Timeout        -- ^ Issue date
  -> Timeout        -- ^ Maturity
  -> Timeout        -- ^ Settlement date
  -> Contract       -- ^ Covered Call Contract
coveredCall issuer counterparty priceFeed currency underlying strike ratio issue maturity settlement =
    deposit issuer issuer (underlying, ratio) issue Close
  $ option European Call counterparty issuer priceFeed (underlying, ratio) (currency, strike) maturity settlement

-- |A /Straddle/ involves simultaneously buying a call and a put option
-- for the same underlying with the same strike and the same expiry.
straddle ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Ratio
  -> Value          -- ^ Strike
  -> Timeout        -- ^ Maturity
  -> Timeout        -- ^ Settlement date
  -> Contract       -- ^ Straddle Contract
straddle buyer seller priceFeed currency underlying ratio strike maturity settlement =
  let c = option European Call buyer seller priceFeed (underlying, ratio) (currency, strike) maturity settlement
      p = option European Put  buyer seller priceFeed (underlying, ratio) (currency, strike) maturity settlement
   in c `both` p

-- |A /Strangle/ involves simultaneously buying a call and a put option
-- for the same underlying with different strikes, but with the same expiry.
strangle ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Ratio
  -> Value          -- ^ Lower Strike
  -> Value          -- ^ Upper Strike
  -> Timeout        -- ^ Maturity
  -> Timeout        -- ^ Settlement date
  -> Contract       -- ^ Straddle Contract
strangle buyer seller priceFeed currency underlying ratio strike1 strike2 maturity settlement =
  let c = option European Call buyer seller priceFeed (underlying, ratio) (currency, strike1) maturity settlement
      p = option European Put  buyer seller priceFeed (underlying, ratio) (currency, strike2) maturity settlement
   in c `both` p

-- |A /Call Spread/ involves simultaneously buying two call options
-- for the same underlying with different strikes, but with the same expiry.
callSpread ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Strike price (in currency) for the long position
  -> Value          -- ^ Strike price (in currency) for the short position
  -> Value          -- ^ Amount of underlying tokens per contract
  -> Timeout        -- ^ Maturity
  -> Timeout        -- ^ Settlement date
  -> Contract       -- ^ Call Spread Contract
callSpread buyer seller priceFeed currency underlying strike1 strike2 ratio maturity settlement =
  let l = option European Call buyer seller priceFeed (underlying, ratio) (currency, strike1) maturity settlement
      s = option European Call seller buyer priceFeed (underlying, ratio) (currency, strike2) maturity settlement
   in l `both` s

-- |A /Barrier Option/ is a path dependent option. The option gets activated or deactivated depending on current
-- market conditions for the underlying instrument.
barrierOption ::
     ExerciseType   -- ^ Exercise Type
  -> OptionType     -- ^ Type of Option
  -> BarrierType    -- ^ Barrier Type
  -> Party          -- ^ Issuer of the covered Call
  -> Party          -- ^ Counter-party
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Strike price (in currency)
  -> Value          -- ^ Amount of underlying tokens per contract
  -> Value          -- ^ Barrier
  -> [Timeout]      -- ^ Barrier observation dates
  -> Timeout        -- ^ Maturity
  -> Timeout        -- ^ Settlement date
  -> Contract       -- ^ Covered Call Contract
barrierOption exerciseType optionType barrierType issuer counterparty priceFeed currency underlying strike ratio barrier observationDates maturity settlement =
  foldl checkBarrierCondition initialContract observationDates
  where
    optionContract = option exerciseType optionType issuer counterparty priceFeed (underlying, ratio) (currency, strike) maturity settlement

    initialContract = initialContract' barrierType
      where
        initialContract' DownAndIn  = Close
        initialContract' DownAndOut = optionContract
        initialContract' UpAndIn    = Close
        initialContract' UpAndOut   = optionContract

    checkBarrierCondition continuation timeout =
      When [checkBarrierCondition' priceFeed barrierType] timeout Close
      where
        checkBarrierCondition' :: Maybe ChoiceId -> BarrierType -> Case
        checkBarrierCondition' Nothing _                  = choose (ChoiceId "Barrier Breach" issuer) continuation optionContract
        checkBarrierCondition' (Just choiceId) DownAndIn  = chooseOracle choiceId (currency, barrier) ValueGT continuation optionContract
        checkBarrierCondition' (Just choiceId) DownAndOut = chooseOracle choiceId (currency, barrier) ValueGT continuation Close
        checkBarrierCondition' (Just choiceId) UpAndIn    = chooseOracle choiceId (currency, barrier) ValueLT continuation optionContract
        checkBarrierCondition' (Just choiceId) UpAndOut   = chooseOracle choiceId (currency, barrier) ValueLT continuation Close
