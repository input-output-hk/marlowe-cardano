{-# LANGUAGE OverloadedStrings #-}
module Options where

import Common
import Language.Marlowe

-- = Options

data OptionType =
      Call -- ^ Call, the right to buy the Option
    | Put  -- ^ Put, the right to sell the Option
  deriving Show

data ExerciseType =
      European -- ^ Execution at expiry
    | American -- ^ Execution anytime before expiry
  deriving Show

-- |"The term option refers to a financial instrument that is based on the value of underlying
-- securities such as stocks. An options contract offers the buyer the opportunity to buy or
-- sell - depending on the type of contract they hold - the underlying asset" -- investopedia
option ::
     ExerciseType               -- ^ Exercise Type
  -> OptionType                 -- ^ Type of Option
  -> Party                      -- ^ Buyer
  -> Party                      -- ^ Seller
  -> (Token, Value Observation) -- ^ Underlying
  -> (Token, Value Observation) -- ^ Strike
  -> Timeout                    -- ^ Expiry
  -> Timeout                    -- ^ Settlement Date
  -> Contract                   -- ^ Option Contract
option American optionType buyer seller asset strike expiry settlement =
  exercise
    optionType
    buyer
    seller
    asset
    strike
    settlement
    expiry
    Close
option European optionType buyer seller asset strike expiry settlement =
  waitUntil expiry $
    exercise
      optionType
      buyer
      seller
      asset
      strike
      settlement
      settlement
      Close

-- |Choose whether to exercise an option and transfer the assets or close
-- the contract
exercise ::
     OptionType                 -- ^ Type of Option
  -> Party                      -- ^ Buyer
  -> Party                      -- ^ Seller
  -> (Token, Value Observation) -- ^ Underlying
  -> (Token, Value Observation) -- ^ Strike
  -> Timeout                    -- ^ Timeout
  -> Timeout                    -- ^ Expiry
  -> Contract                   -- ^ Continuation
  -> Contract                   -- ^ Contract
exercise Call buyer seller asset strike timeout =
  When
    [ choose (ChoiceId "Exercise Call" buyer)
    $ deposit buyer buyer strike timeout Close
    $ pay seller buyer asset
    $ pay buyer seller strike
      Close
    ]
exercise Put buyer seller asset strike timeout =
  When
    [ choose (ChoiceId "Exercise Put" buyer)
    $ deposit buyer buyer asset timeout Close
    $ pay seller buyer strike
    $ pay buyer seller asset
      Close
    ]

choose ::
     ChoiceId      -- ^ Choice id, references the party to chose
  -> Contract      -- ^ Continuation Contract if the option is exercised
  -> Case Contract -- ^ Case expression with continuation
choose choiceId cont =
  Case
    (Choice choiceId [Bound 0 1])
    ( If
        (ValueEQ (ChoiceValue choiceId) (Constant 0))
        Close
        cont
    )

-- = Options Strategies

-- |A Covered Call is an option strategie constructed by writing a call on a token
-- and in addition providing the token as cover/collateral as part of the contract
coveredCall ::
     Party             -- ^ Buyer
  -> Party             -- ^ Seller
  -> Token             -- ^ Currency
  -> Token             -- ^ Underlying
  -> Value Observation -- ^ Strike price (in currency)
  -> Value Observation -- ^ Amount of underlying tokens per contract
  -> Timeout           -- ^ Issue Date
  -> Timeout           -- ^ Maturity
  -> Timeout           -- ^ Settlement Date
  -> Contract          -- ^ Covered Call Contract
coveredCall buyer seller currency underlying strike ratio issue maturity settlement =
    deposit buyer buyer (underlying, ratio) issue Close
  $ option European Call seller buyer (currency, strike) (underlying, ratio) maturity settlement

-- |"A straddle is a neutral options strategy that involves simultaneously buying both
-- a put option and a call option for the underlying security with the same strike price
-- and the same expiration date." -- investopedia
straddle ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value Observation    -- ^ Ratio
  -> Value Observation    -- ^ Strike
  -> Timeout  -- ^ Maturity
  -> Timeout           -- ^ Settlement Date
  -> Contract -- ^ Straddle Contract
straddle buyer seller currency underlying ratio strike maturity settlement =
  let c = option European Call buyer seller (currency, strike) (underlying, ratio) maturity settlement
      p = option European Put  buyer seller (currency, strike) (underlying, ratio) maturity settlement
   in c `both` p

-- |"A strangle is an options strategy in which the investor holds a position in both
-- a call and a put option with different strike prices, but with the same expiration
-- date and underlying asset." -- investopedia
strangle ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value Observation    -- ^ Ratio
  -> Value Observation    -- ^ Lower Strike
  -> Value Observation    -- ^ Upper Strike
  -> Timeout  -- ^ Maturity
  -> Timeout           -- ^ Settlement Date
  -> Contract -- ^ Straddle Contract
strangle buyer seller currency underlying ratio strike1 strike2 maturity settlement =
  let c = option European Call buyer seller (currency, strike1) (underlying, ratio) maturity settlement
      p = option European Put  buyer seller (currency, strike2) (underlying, ratio) maturity settlement
   in c `both` p

-- |"A bull call spread is an options trading strategy designed to benefit from a stock's
-- limited increase in price. The strategy uses two call options to create a range consisting
-- of a lower strike price and an upper strike price." -- investopedia
callSpread ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value Observation    -- ^ Strike price (in currency) for the long position
  -> Value Observation    -- ^ Strike price (in currency) for the short position
  -> Value Observation    -- ^ Amount of underlying tokens per contract
  -> Timeout  -- ^ Maturity
  -> Timeout           -- ^ Settlement Date
  -> Contract -- ^ Call Spread Contract
callSpread buyer seller currency underlying strike1 strike2 ratio maturity settlement =
  let s = option European Call buyer seller (currency, strike1) (underlying, ratio) maturity settlement
      l = option European Call seller buyer (currency, strike2) (underlying, ratio) maturity settlement
   in s `both` l
