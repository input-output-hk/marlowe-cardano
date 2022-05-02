{-# LANGUAGE OverloadedStrings #-}
module Option where

import Language.Marlowe.Extended

main :: IO ()
main = printJSON $
  coveredCall
    (Role "Party")
    (Role "Counterparty")
    ada
    (Token "" "Underlying")
    (ConstantParam "Strike")
    (ConstantParam "Ratio")
    (TimeParam "Issue Date")
    (TimeParam "Expiry")
    (TimeParam "Settlement Date")

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
  -> Case     -- ^
choose choiceId cont =
  Case
    (Choice choiceId [Bound 0 1])
    ( If
        (ValueEQ (ChoiceValue choiceId) (Constant 0))
        Close
        cont
    )

-- |Wait until timeout
waitUntil ::
     Timeout  -- ^ Timeout
  -> Contract -- ^ Continuation Contract
  -> Contract -- ^ Combined Contract
waitUntil = When []

-- |Pay
pay ::
     Party          -- ^ Payer
  -> Party          -- ^ Payee
  -> (Token, Value) -- ^ Token and Value
  -> Contract       -- ^ Continuation Contract
  -> Contract       -- ^ Combined Contract
pay fromParty toParty (token, value) =
  Pay
    fromParty
    (Party toParty)
    token
    value

-- |Deposit
deposit ::
     Party          -- ^ Payer
  -> Party          -- ^ Payee
  -> (Token, Value) -- ^ Token and Value
  -> Timeout        -- ^ Timeout for deposit
  -> Contract       -- ^ Continuation Contract in case of timeout of deposit
  -> Contract       -- ^ Continuation Contract after deposit
  -> Contract       -- ^ Combined Contract
deposit fromParty toParty (token, value) timeout timeoutContinuation continuation =
  When
    [Case (Deposit toParty fromParty token value) continuation]
    timeout
    timeoutContinuation

-- = Options Strategies

-- |A Covered Call is an option strategie constructed by writing a call on a token
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
coveredCall buyer seller currency underlying strike ratio issue maturity settlment =
  deposit buyer buyer (underlying, ratio) issue Close $
    option
      European
      Call
      seller
      buyer
      (currency, strike)
      (underlying, ratio)
      maturity
      settlment
