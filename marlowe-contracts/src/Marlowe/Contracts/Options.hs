{-# LANGUAGE OverloadedStrings #-}
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

-- |An /option/ is a financial instrument that is based on the value of underlying
-- securities. An options contract offers the buyer the opportunity but not
-- the obligation to buy or sell the underlying asset.
option ::
     ExerciseType   -- ^ Exercise Type
  -> OptionType     -- ^ Type of Option
  -> Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> (Token, Value) -- ^ Underlying
  -> (Token, Value) -- ^ Strike
  -> Timeout        -- ^ Expiry
  -> Timeout        -- ^ Settlement Date
  -> Contract       -- ^ Option Contract
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
     OptionType     -- ^ Type of Option
  -> Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> (Token, Value) -- ^ Underlying
  -> (Token, Value) -- ^ Strike
  -> Timeout        -- ^ Timeout
  -> Timeout        -- ^ Expiry
  -> Contract       -- ^ Continuation
  -> Contract       -- ^ Contract
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
     ChoiceId -- ^ Choice id, references the party to chose
  -> Contract -- ^ Continuation Contract if the option is exercised
  -> Case     -- ^ Case expression with continuation
choose choiceId cont =
  Case
    (Choice choiceId [Bound 0 1])
    ( If
        (ValueEQ (ChoiceValue choiceId) (Constant 0))
        Close
        cont
    )

-- |A /Covered Call/ is an option strategy constructed by writing a call on a token
-- and in addition providing the token as cover/collateral as part of the contract
coveredCall ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Strike price (in currency)
  -> Value    -- ^ Amount of underlying tokens per contract
  -> Timeout  -- ^ Issue Date
  -> Timeout  -- ^ Maturity
  -> Timeout  -- ^ Settlement Date
  -> Contract -- ^ Covered Call Contract
coveredCall buyer seller currency underlying strike ratio issue maturity settlement =
    deposit buyer buyer (underlying, ratio) issue Close
  $ option European Call seller buyer (underlying, ratio) (currency, strike) maturity settlement

-- |A /Straddle/ involves simultaneously buying a call and a put option
-- for the same underlying with the same strike and the same expiry.
straddle ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Ratio
  -> Value    -- ^ Strike
  -> Timeout  -- ^ Maturity
  -> Timeout  -- ^ Settlement Date
  -> Contract -- ^ Straddle Contract
straddle buyer seller currency underlying ratio strike maturity settlement =
  let c = option European Call buyer seller (underlying, ratio) (currency, strike) maturity settlement
      p = option European Put  buyer seller (underlying, ratio) (currency, strike) maturity settlement
   in c `both` p

-- |A /Strangle/ involves simultaneously buying a call and a put option
-- for the same underlying with different strikes, but with the same expiry.
strangle ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Ratio
  -> Value    -- ^ Lower Strike
  -> Value    -- ^ Upper Strike
  -> Timeout  -- ^ Maturity
  -> Timeout  -- ^ Settlement Date
  -> Contract -- ^ Straddle Contract
strangle buyer seller currency underlying ratio strike1 strike2 maturity settlement =
  let c = option European Call buyer seller (underlying, ratio) (currency, strike1) maturity settlement
      p = option European Put  buyer seller (underlying, ratio) (currency, strike2) maturity settlement
   in c `both` p

-- |A /Call Spread/ involves simultaneously buying two call options
-- for the same underlying with different strikes, but with the same expiry.
callSpread ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Strike price (in currency) for the long position
  -> Value    -- ^ Strike price (in currency) for the short position
  -> Value    -- ^ Amount of underlying tokens per contract
  -> Timeout  -- ^ Maturity
  -> Timeout  -- ^ Settlement Date
  -> Contract -- ^ Call Spread Contract
callSpread buyer seller currency underlying strike1 strike2 ratio maturity settlement =
  let l = option European Call buyer seller (underlying, ratio) (currency, strike1) maturity settlement
      s = option European Call seller buyer (underlying, ratio) (currency, strike2) maturity settlement
   in l `both` s
