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
pay fromParty toParty (token, value) =
  Pay
    fromParty
    (Party toParty)
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
deposit fromParty toParty (token, value) timeout timeoutContinuation continuation =
  When
    [ Case
        (Deposit toParty fromParty token value)
        continuation
    ]
    timeout
    timeoutContinuation

-- |Transfer
transfer ::
     Timeout
  -> Party
  -> Party
  -> Value Observation
  -> Contract
  -> Contract
transfer timeout from to amount continuation =
  When
    [ Case
        (Deposit from from ada amount)
        (Pay from (Party to) ada amount continuation)
    ]
    timeout
    Close
