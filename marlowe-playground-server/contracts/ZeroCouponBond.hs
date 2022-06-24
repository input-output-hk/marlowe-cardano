{-# LANGUAGE OverloadedStrings #-}
module ZeroCouponBond where

import Language.Marlowe.Extended.V1

main :: IO ()
main = printJSON $ contract

discountedPrice, notionalPrice :: Value
discountedPrice = ConstantParam "Amount"
notionalPrice = AddValue (ConstantParam "Interest") discountedPrice

investor, issuer :: Party
investor = Role "Lender"
issuer = Role "Borrower"

initialExchange, maturityExchangeTimeout :: Timeout
initialExchange = TimeParam "Loan deadline"
maturityExchangeTimeout = TimeParam "Payback deadline"

transfer :: Timeout -> Party -> Party -> Value -> Contract -> Contract
transfer timeout from to amount continuation =
    When [ Case (Deposit from from ada amount)
                (Pay from (Party to) ada amount continuation) ]
         timeout
         Close

contract :: Contract
contract = transfer initialExchange investor issuer discountedPrice
         $ transfer maturityExchangeTimeout issuer investor notionalPrice
           Close
