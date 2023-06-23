{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Trivial where

import Language.Marlowe.Extended.V1

-- | A trivial contract, for testing.
trivial
  :: Party
  -- ^ The party.
  -> Integer
  -- ^ Lovelace in the deposit.
  -> Integer
  -- ^ Lovelace in the withdrawal.
  -> Timeout
  -- ^ The timeout.
  -> Contract
  -- ^ The escrow contract and initial state.
trivial party deposit withdrawal timeout =
  When
    [ Case (Deposit party party ada (Constant deposit)) $
        When
          [ Case (Notify TrueObs) $
              Pay party (Party party) ada (Constant withdrawal) $
                When
                  [ Case
                      (Notify TrueObs)
                      Close
                  ]
                  timeout
                  Close
          ]
          (timeout - 1000)
          Close
    ]
    (timeout - 2000)
    Close
