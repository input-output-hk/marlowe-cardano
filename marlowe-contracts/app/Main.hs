{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Marlowe
import Marlowe.Contracts.Futures
import Marlowe.Contracts.ReverseConvertible

-- |main
main :: IO ()
main = writeFile "future.marlowe" (show . pretty $ fut)

fut :: Contract
fut = future
        buyer
        seller
        (Constant forwardPrice)
        (Constant initialMargin)
        (Slot setup)
        (reverse $ map Slot [start, start + step .. delivery])
        (Slot delivery)
  where
    forwardPrice  = 90 -- Ada
    initialMargin = 20 -- Ada
    setup         = 1
    start         = 10
    step          = 10
    delivery      = 30

rc :: Contract
rc =
  reverseConvertible
    investor
    issuer
    (Slot 10)
    (Slot 10)
    ada
    (Token "" "usd")
    (Constant 100)
    (Constant 100)

-- |Roles for Buyer and Seller
buyer, seller :: Party
buyer  = Role "Buyer"
seller = Role "Seller"

-- |Roles for Investor and Issuer
investor, issuer :: Party
investor = Role "Investor"
issuer   = Role "Issuer"

-- |Roles for Borrower and Lender
borrower, lender :: Party
borrower = Role "Borrower"
lender   = Role "Lender"

