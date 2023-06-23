{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Common where

import Language.Marlowe.Extended.V1

-- | Compose two contracts
both :: Contract -> Contract -> Contract
both Close b = b
both a Close = a
both a@(When cases1 (POSIXTime timeout1) cont1) b@(When cases2 (POSIXTime timeout2) cont2) =
  When
    ( [Case a1 (both c1 b) | Case a1 c1 <- cases1]
        ++ [Case a2 (both a c2) | Case a2 c2 <- cases2]
    )
    (POSIXTime (min timeout1 timeout2))
    ( both
        (if timeout1 > timeout2 then a else cont1)
        (if timeout2 > timeout1 then b else cont2)
    )
both a@When{} b = advanceTillWhenAndThen b (both a)
both a b = advanceTillWhenAndThen a (`both` b)

advanceTillWhenAndThen :: Contract -> (Contract -> Contract) -> Contract
advanceTillWhenAndThen Close f = f Close
advanceTillWhenAndThen w@When{} f = f w
advanceTillWhenAndThen (Pay accId p tok val cont) f = Pay accId p tok val (f cont)
advanceTillWhenAndThen (If obs cont1 cont2) f = If obs (f cont1) (f cont2)
advanceTillWhenAndThen (Let vId val cont) f = Let vId val (f cont)
advanceTillWhenAndThen (Assert obs cont) f = Assert obs (f cont)

-- | Role for oracle
kraken :: Party
kraken = Role "kraken"

-- | Exchange rates
dirRate, invRate :: ChoiceId
dirRate = ChoiceId "dir-adausd" kraken -- USD/ADA
invRate = ChoiceId "inv-adausd" kraken -- ADA/USD

-- | Oracle input
oracleInput
  :: ChoiceId
  -- ^ Oracle selector
  -> Timeout
  -- ^ Timeout for oracle input
  -> Contract
  -- ^ Continuation in case of timeout
  -> Contract
  -- ^ Continuation contract
  -> Contract
  -- ^ Composed contract
oracleInput choiceId timeout timeoutContinuation continuation =
  When
    [Case (Choice choiceId [Bound 0 100_000_000_000]) continuation]
    timeout
    timeoutContinuation

-- | Wait until timeout
waitUntil
  :: Timeout
  -- ^ Timeout
  -> Contract
  -- ^ Continuation Contract
  -> Contract
  -- ^ Combined Contract
waitUntil = When []

-- | Pay
pay
  :: Party
  -- ^ Payer
  -> Party
  -- ^ Payee
  -> (Token, Value)
  -- ^ Token and Value
  -> Contract
  -- ^ Continuation Contract
  -> Contract
  -- ^ Combined Contract
pay from to (token, value) =
  Pay
    from
    (Party to)
    token
    value

-- | Deposit
deposit
  :: Party
  -- ^ Party to receive the deposit
  -> Party
  -- ^ Party that deposits
  -> (Token, Value)
  -- ^ Token and Value
  -> Timeout
  -- ^ Timeout for deposit
  -> Contract
  -- ^ Continuation Contract in case of timeout of deposit
  -> Contract
  -- ^ Continuation Contract after deposit
  -> Contract
  -- ^ Combined Contract
deposit to from (token, value) timeout timeoutContinuation continuation =
  When
    [ Case
        (Deposit to from token value)
        continuation
    ]
    timeout
    timeoutContinuation

-- | Transfer, i.e. Deposit and Pay
transfer
  :: Party
  -- ^ Payer
  -> Party
  -- ^ Payee
  -> (Token, Value)
  -- ^ Token and Value
  -> Timeout
  -- ^ Timeout for transfer
  -> Contract
  -- ^ Continuation Contract in case of timeout of deposit
  -> Contract
  -- ^ Continuation Contract
  -> Contract
  -- ^ Combined Contract
transfer from to tokenValue timeout timeoutContinuation continuation =
  deposit from from tokenValue timeout timeoutContinuation $
    pay from to tokenValue continuation
