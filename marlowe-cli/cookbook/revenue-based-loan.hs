{-# LANGUAGE OverloadedStrings #-}

module RevenueBasedLoan where

import Language.Marlowe.Extended.V1

main :: IO ()
main = printJSON $ contract

djed :: Token
djed = Token "f4cf384ddd1b1377b08302b17990e9618b62924f5705458c17ee4f7d" "DjedUSD"

lender :: Party
lender = Role "Lender"

borrower :: Party
borrower = Role "Borrower"

oracle :: Party
oracle = Role "Revenue Oracle"

revenueChoice :: ChoiceId
revenueChoice = ChoiceId "Revenue" oracle

revenueValue :: Value
revenueValue = ChoiceValue revenueChoice

revenueBounds :: Bound
revenueBounds = Bound 1 1000000

principal :: Value
principal = ConstantParam "Principal"

interest :: Value
interest = ConstantParam "Interest"

paymentPercent :: Value
paymentPercent = ConstantParam "Payment as Percent of Revenue"

remainingDue :: Value
remainingDue = UseValue "Remaining Due"

nextPayment :: Value
nextPayment = UseValue "Next Payment"

makePayment :: Int -> Contract -> Contract
makePayment period continuation =
  When
    [
      Case (Choice revenueChoice [revenueBounds])
        $ Let "Next Payment" (DivValue (MulValue revenueValue paymentPercent) (Constant 100))
        $ If (nextPayment `ValueGT` remainingDue)
            (
              When
                [
                  Case (Deposit lender borrower djed remainingDue)
                    $ Pay lender (Party lender) djed remainingDue
                      Close
                ]
                (TimeParam $ "Deadline " <> show period <> " for Payment")
                continuation
            )
            (
              When
                [
                  Case (Deposit lender borrower djed nextPayment)
                    $ Pay lender (Party lender) djed nextPayment
                    $ Let "Remaining Due" (SubValue remainingDue nextPayment)
                      continuation
                ]
                (TimeParam $ "Deadline " <> show period <> " for Payment")
                continuation
            )
    ]
    (TimeParam $ "Deadline " <> show period <> " for Oracle")
    continuation

collectRemainder :: Contract
collectRemainder =
  When
    [
      Case (Deposit lender borrower djed remainingDue)
        Close
    ]
    (TimeParam "End of Contract")
    Close

contract :: Contract
contract =
  When
    [
      Case (Deposit lender lender djed principal)
        $ Pay lender (Party borrower) djed principal
        $ Let "Remaining Due" (AddValue principal interest)
        $ foldr makePayment collectRemainder [1..5]
    ]
    (TimeParam "Loan Deadline")
    Close
