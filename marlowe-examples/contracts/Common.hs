{-# LANGUAGE OverloadedStrings #-}
module Common where

import Language.Marlowe

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
    [Case (Deposit toParty fromParty token value) continuation]
    timeout
    timeoutContinuation
