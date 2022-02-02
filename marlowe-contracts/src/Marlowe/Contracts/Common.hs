{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Marlowe.Contracts.Common where

import Language.Marlowe

-- |Role for oracle
oracle :: Party
oracle = Role "kraken"

-- |Exchange rates
dirRate, invRate :: ChoiceId
dirRate = ChoiceId "dir-adausd" oracle -- USC/ADA
invRate = ChoiceId "inv-adausd" oracle -- ADA/UCS

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
     Party                      -- ^ Payer
  -> Party                      -- ^ Payee
  -> (Token, Value Observation) -- ^ Token and Value
  -> Contract                   -- ^ Continuation Contract
  -> Contract                   -- ^ Combined Contract
pay from to (token, value) =
  Pay
    from
    (Party to)
    token
    value

-- |Deposit
deposit ::
     Party                      -- ^ Payer
  -> Party                      -- ^ Payee
  -> (Token, Value Observation) -- ^ Token and Value
  -> Timeout                    -- ^ Timeout for deposit
  -> Contract                   -- ^ Continuation Contract in case of timeout of deposit
  -> Contract                   -- ^ Continuation Contract after deposit
  -> Contract                   -- ^ Combined Contract
deposit from to (token, value) timeout timeoutContinuation continuation =
  When
    [ Case
        (Deposit to from token value)
        continuation
    ]
    timeout
    timeoutContinuation

-- |Transfer
transfer ::
     Party                      -- ^ Payer
  -> Party                      -- ^ Payee
  -> (Token, Value Observation) -- ^ Value
  -> Timeout                    -- ^ Timeout for transfer
  -> Contract                   -- ^ Continuation Contract
  -> Contract                   -- ^ Combined Contract
transfer from to (token, value) timeout continuation =
  When
    [ Case
        (Deposit from from ada value)
        (Pay from (Party to) ada value continuation)
    ]
    timeout
    Close
