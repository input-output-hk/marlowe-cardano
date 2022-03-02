-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Escrow contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}


module Language.Marlowe.CLI.Examples.Escrow (
  -- * Contract
  makeEscrowContract
) where


import Language.Marlowe.Extended
import Language.Marlowe.Semantics (MarloweData (..))
import Language.Marlowe.SemanticsTypes (State (..))
import Marlowe.Contracts.Escrow

import qualified PlutusTx.AssocMap as AM (empty, singleton)


-- | An escrow contract with mediation.
makeEscrowContract :: Integer     -- ^ Lovelace in the initial state.
                   -> Integer     -- ^ Price of the item, in lovelace.
                   -> Party       -- ^ The seller.
                   -> Party       -- ^ The buyer.
                   -> Party       -- ^ The mediator.
                   -> Timeout     -- ^ The deadline for the buyer to pay.
                   -> Timeout     -- ^ The deadline for the buyer to complain.
                   -> Timeout     -- ^ The deadline for the seller to dispute a complaint.
                   -> Timeout     -- ^ The deadline for the mediator to decide.
                   -> MarloweData -- ^ The escrow contract and initial state.
makeEscrowContract minAda price seller buyer mediator paymentDeadline complaintDeadline disputeDeadline mediationDeadline =
  let
    marloweState =
      State
      {
        accounts    = AM.singleton (mediator, ada) minAda
      , choices     = AM.empty
      , boundValues = AM.empty
      , minTime     = 1
      }
    Just marloweContract = toCore $
      escrow
        (Constant price)
        seller
        buyer
        mediator
        paymentDeadline
        complaintDeadline
        disputeDeadline
        mediationDeadline
  in
    MarloweData{..}
