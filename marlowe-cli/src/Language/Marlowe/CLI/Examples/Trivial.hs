-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Trivial contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Examples.Trivial (
  -- * Contract
  makeTrivialContract
) where


import Language.Marlowe.Semantics (MarloweData (..))
import Language.Marlowe.SemanticsTypes (Action (..), Case (..), Contract (..), Observation (TrueObs), Party (..),
                                        Payee (..), State (..), Token (..), Value (..))
import Ledger (POSIXTime)
import Ledger.Ada (adaSymbol, adaToken)

import qualified PlutusTx.AssocMap as AM (empty, singleton)


-- | A trivial contract, for testing.
makeTrivialContract :: Party        -- ^ The party providing the min-ADA.
                    -> Integer      -- ^ Lovelace in the initial state.
                    -> POSIXTime    -- ^ The minimum POSIX time.
                    -> Party        -- ^ The party.
                    -> Integer      -- ^ Lovelace in the deposit.
                    -> Integer      -- ^ Lovelace in the withdrawal.
                    -> POSIXTime         -- ^ The timeout.
                    -> MarloweData  -- ^ The escrow contract and initial state.
makeTrivialContract bystander minAda minTime party deposit withdrawal timeout =
  let
    ada = Token adaSymbol adaToken
    marloweState =
      State
      {
        accounts    = AM.singleton (bystander, ada) minAda
      , choices     = AM.empty
      , boundValues = AM.empty
      , minTime     = minTime
      }
    marloweContract =
      When
        [
          Case (Deposit party party ada (Constant deposit))
            $ When
              [
                Case (Notify TrueObs)
                  $ Pay party (Party party) ada (Constant withdrawal)
                  $ When
                    [
                      Case (Notify TrueObs)
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
  in
    MarloweData{..}
