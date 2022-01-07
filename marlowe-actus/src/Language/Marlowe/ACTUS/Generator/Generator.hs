{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS Generator

-}

module Language.Marlowe.ACTUS.Generator.Generator
  ( invoice
  )
where

import Data.String (IsString (fromString))
import Language.Marlowe (Action (..), Case (..), Contract (..), Observation (..), Party (..), Payee (..), Slot (..),
                         Value (..), ada)
import Ledger.Value (TokenName (TokenName))

invoice :: String -> String -> Value Observation -> Slot -> Contract -> Contract
invoice from to amount timeout continue =
  let party = Role $ TokenName $ fromString from
      counterparty = Role $ TokenName $ fromString to
   in When
        [ Case
            (Deposit party party ada amount)
            ( Pay
                party
                (Party counterparty)
                ada
                amount
                continue
            )
        ]
        timeout
        Close
