{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Marlowe.Contracts.Options
  (
  -- * Options
    option
  , OptionType(..)
  , ExerciseType(..)
  , BarrierType(..)
  -- * Option Strategies
  -- ** Fully collateralized
  , coveredCall
  -- ** Partially collateralized
  , callSpread
  , cliquetOption
  -- ** Not collateralized
  , barrierOption
  , chooserOption
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
      DownAndIn  -- ^ Knock in, downwards crossing of barrier
    | DownAndOut -- ^ Knock out, downwards crossing of barrier
    | UpAndIn    -- ^ Knock in, upwards crossing of barrier
    | UpAndOut   -- ^ Knock out, upwards crossing of barrier

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
  -> Contract       -- ^ Continuation
  -> Contract       -- ^ Option Contract
option American optionType buyer seller priceFeed asset strike expiry _ continuation =
  exercise
    optionType
    buyer
    seller
    priceFeed
    asset
    strike
    expiry
    continuation
option European optionType buyer seller priceFeed asset strike expiry settlement continuation =
  waitUntil expiry $
    exercise
      optionType
      buyer
      seller
      priceFeed
      asset
      strike
      settlement
      continuation

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
  -> Contract       -- ^ Continuation
  -> Contract       -- ^ Contract
exercise optionType buyer seller priceFeed asset strike timeout continuation =
  When
    [ exercise'
        optionType
        priceFeed
    ]
    timeout
    Close
  where
    -- without a price-feed (oracle)
    exercise' Call Nothing = choose (ChoiceId "Exercise Call" buyer) continuation $ depositAndPay buyer seller strike asset timeout continuation
    exercise' Put Nothing  = choose (ChoiceId "Exercise Put"  buyer) continuation $ depositAndPay buyer seller asset strike timeout continuation

    -- with a price-feed (oracle)
    exercise' Call (Just choiceId) = chooseOracle choiceId strike ValueLT continuation $ depositAndPay buyer seller strike asset timeout continuation
    exercise' Put  (Just choiceId) = chooseOracle choiceId strike ValueGT continuation $ depositAndPay buyer seller asset strike timeout continuation

-- |Deposit an asset and swap
depositAndPay ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> (Token, Value) -- ^ Underlying asset
  -> (Token, Value) -- ^ Strike price
  -> Timeout        -- ^ Timeout for deposit
  -> Contract       -- ^ Continuation
  -> Contract       -- ^ Contract
depositAndPay buyer seller asset strike timeout continuation =
    deposit buyer buyer asset timeout Close
  $ pay seller buyer strike
  $ pay buyer seller asset
    continuation

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

-- |A /Chooser Option/ allows the holder of the option to decide prior to the expiration
-- if the option is a call or put. Strike and expiration date are the same in either case.
chooserOption ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Strike price (in currency)
  -> Value          -- ^ Amount of underlying tokens per contract
  -> Timeout        -- ^ Maturity
  -> Timeout        -- ^ Settlement date
  -> Contract       -- ^ Chooser Option Contract
chooserOption buyer seller priceFeed currency underlying strike ratio maturity settlement =
  let c = option European Call buyer seller priceFeed (underlying, ratio) (currency, strike) maturity settlement Close
      p = option European Put buyer seller priceFeed (underlying, ratio) (currency, strike) maturity settlement Close
   in When [ choose (ChoiceId "0:Call, 1:Put" buyer) c p ] maturity Close

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
  $ option European Call counterparty issuer priceFeed (underlying, ratio) (currency, strike) maturity settlement Close

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
  let c = option European Call buyer seller priceFeed (underlying, ratio) (currency, strike) maturity settlement Close
      p = option European Put  buyer seller priceFeed (underlying, ratio) (currency, strike) maturity settlement Close
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
  let c = option European Call buyer seller priceFeed (underlying, ratio) (currency, strike1) maturity settlement Close
      p = option European Put  buyer seller priceFeed (underlying, ratio) (currency, strike2) maturity settlement Close
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
  let l = option European Call buyer seller priceFeed (underlying, ratio) (currency, strike1) maturity settlement Close
      s = option European Call seller buyer priceFeed (underlying, ratio) (currency, strike2) maturity settlement Close
   in l `both` s

-- |A /Cliquet Option/ consists of a series of consecutive options. The options are executed in sequence, the strike price
-- of the next option is always the current price of the underlying. The option type can be either Call or Put.
cliquetOption ::
     OptionType     -- ^ Type of Option
  -> Party          -- ^ Issuer
  -> Party          -- ^ Counter-party
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Ratio
  -> [Timeout]      -- ^ Expiries
  -> Integer        -- ^ Settlement delay
  -> Contract       -- ^ Cliquet Option Contract
cliquetOption optionType issuer counterparty priceFeed currency underlying ratio expiries delay =
  foldr f Close expiries
  where
    f expiry continuation =
      When
        [ spot priceFeed $
            option
              European
              optionType
              issuer
              counterparty
              priceFeed
              (underlying, ratio)
              (currency, UseValue price)
              expiry
              (expiry + POSIXTime delay)
              continuation
        ]
        (last expiries)
        Close

    spot :: Maybe ChoiceId -> Contract -> Case
    spot (Just choiceId) continuation =
      Case
        (Choice choiceId [Bound 0 100_000_000_000])
        (Let price (ChoiceValue choiceId) continuation)
    spot Nothing continuation = spot (Just $ ChoiceId "manual price" issuer) continuation

    price :: ValueId
    price = ValueId "price"

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
    -- The option contract, that can be activated (knock-in) or deactivated (knock-out) over time
    optionContract = option exerciseType optionType issuer counterparty priceFeed (underlying, ratio) (currency, strike) maturity settlement Close

    -- Knock-in options are set to the Close contract initially, knock-out options to the option contract
    initialContract = initialContract' barrierType
      where
        initialContract' DownAndIn  = Close
        initialContract' DownAndOut = optionContract
        initialContract' UpAndIn    = Close
        initialContract' UpAndOut   = optionContract

    -- Check for knock-in or knock-out events. If an oracle is provided, the decision is based on the current value of the price feed of the underlying
    -- otherwise the decision has to be done manually
    checkBarrierCondition continuation timeout =
      When [checkBarrierCondition' priceFeed barrierType] timeout Close
      where
        checkBarrierCondition' :: Maybe ChoiceId -> BarrierType -> Case
        checkBarrierCondition' (Just choiceId) DownAndIn  = chooseOracle choiceId (currency, barrier) ValueGT continuation optionContract
        checkBarrierCondition' (Just choiceId) DownAndOut = chooseOracle choiceId (currency, barrier) ValueGT continuation Close
        checkBarrierCondition' (Just choiceId) UpAndIn    = chooseOracle choiceId (currency, barrier) ValueLT continuation optionContract
        checkBarrierCondition' (Just choiceId) UpAndOut   = chooseOracle choiceId (currency, barrier) ValueLT continuation Close
        checkBarrierCondition' Nothing _                  = choose (ChoiceId "Barrier Breach" issuer) continuation optionContract
