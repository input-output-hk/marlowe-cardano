{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS Generator

-}

module Language.Marlowe.ACTUS.Generator.Generator
  ( invoice
  )
where

import Data.String (IsString (fromString))
import Language.Marlowe (Action (..), Case_ (..), Contract (..), POSIXTime (..), Party (..), Payee (..), Token, Value,
                         ada)
import Ledger.Value (TokenName (TokenName))

invoice :: String -> String -> Value Token -> POSIXTime -> Contract Token -> Contract Token
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
