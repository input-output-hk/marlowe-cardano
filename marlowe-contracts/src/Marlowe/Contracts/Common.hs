{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Marlowe.Contracts.Common where

import Language.Marlowe.Extended

-- |Role for oracle
oracle :: Party
oracle = Role "kraken"

-- |Exchange rates
dirRate, invRate :: ChoiceId
dirRate = ChoiceId "dir-adausd" oracle -- USD/ADA
invRate = ChoiceId "inv-adausd" oracle -- ADA/USD

-- |Oracle input
oracleInput ::
     ChoiceId  -- ^ Oracle selector
  -> Timeout   -- ^ Timeout for oracle input
  -> Contract  -- ^ Continuation in case of timeout
  -> Contract  -- ^ Continuation contract
  -> Contract  -- ^ Composed contract
oracleInput choiceId timeout timeoutContinuation continuation =
  When
    [Case (Choice choiceId [Bound 0 100_000_000_000]) continuation]
    timeout
    timeoutContinuation

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
pay from to (token, value) =
  Pay
    from
    (Party to)
    token
    value

-- |Deposit
deposit ::
     Party          -- ^ Party to receive the deposit
  -> Party          -- ^ Party that deposits
  -> (Token, Value) -- ^ Token and Value
  -> Timeout        -- ^ Timeout for deposit
  -> Contract       -- ^ Continuation Contract in case of timeout of deposit
  -> Contract       -- ^ Continuation Contract after deposit
  -> Contract       -- ^ Combined Contract
deposit to from (token, value) timeout timeoutContinuation continuation =
  When
    [ Case
        (Deposit to from token value)
        continuation
    ]
    timeout
    timeoutContinuation

-- |Transfer, i.e. Deposit and Pay
transfer ::
     Party          -- ^ Payer
  -> Party          -- ^ Payee
  -> (Token, Value) -- ^ Token and Value
  -> Timeout        -- ^ Timeout for transfer
  -> Contract       -- ^ Continuation Contract in case of timeout of deposit
  -> Contract       -- ^ Continuation Contract
  -> Contract       -- ^ Combined Contract
transfer from to tokenValue timeout timeoutContinuation continuation =
    deposit from from tokenValue timeout timeoutContinuation
  $ pay from to tokenValue continuation
