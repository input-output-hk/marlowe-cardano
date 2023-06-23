{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Escrow (
  escrow,
) where

import Language.Marlowe.Extended.V1

-- | An escrow contract with mediation
escrow
  :: Value
  -- ^ Price of the item, in lovelace
  -> Party
  -- ^ The seller
  -> Party
  -- ^ The buyer
  -> Party
  -- ^ The mediator
  -> Timeout
  -- ^ The deadline for the buyer to pay
  -> Timeout
  -- ^ The deadline for the buyer to complain
  -> Timeout
  -- ^ The deadline for the seller to dispute a complaint
  -> Timeout
  -- ^ The deadline for the mediator to decide
  -> Contract
  -- ^ The escrow contract and initial state
escrow price seller buyer mediator paymentDeadline complaintDeadline disputeDeadline mediationDeadline =
  When
    [ Case (Deposit seller buyer ada price) $
        When
          [ Case
              (Choice (ChoiceId "Everything is alright" buyer) [Bound 0 0])
              Close
          , Case (Choice (ChoiceId "Report problem" buyer) [Bound 1 1]) $
              Pay seller (Account buyer) ada price $
                When
                  [ Case
                      (Choice (ChoiceId "Confirm problem" seller) [Bound 1 1])
                      Close
                  , Case (Choice (ChoiceId "Dispute problem" seller) [Bound 0 0]) $
                      When
                        [ Case (Choice (ChoiceId "Dismiss claim" mediator) [Bound 0 0]) $
                            Pay
                              buyer
                              (Account seller)
                              ada
                              price
                              Close
                        , Case
                            (Choice (ChoiceId "Confirm claim" mediator) [Bound 1 1])
                            Close
                        ]
                        mediationDeadline
                        Close
                  ]
                  disputeDeadline
                  Close
          ]
          complaintDeadline
          Close
    ]
    paymentDeadline
    Close
