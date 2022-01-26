module Examples.Marlowe.Contracts where

example :: String
example = """Close"""

escrow :: String
escrow =
  """When [
  (Case
     (Deposit
        (Role "Seller")
        (Role "Buyer")
        (Token "" "")
        (ConstantParam "Price"))
     (When [
           (Case
              (Choice
                 (ChoiceId "Everything is alright"
                    (Role "Buyer")) [
                 (Bound 0 0)]) Close)
           ,
           (Case
              (Choice
                 (ChoiceId "Report problem"
                    (Role "Buyer")) [
                 (Bound 1 1)])
              (Pay
                 (Role "Seller")
                 (Account
                    (Role "Buyer"))
                 (Token "" "")
                 (ConstantParam "Price")
                 (When [
                       (Case
                          (Choice
                             (ChoiceId "Confirm problem"
                                (Role "Seller")) [
                             (Bound 1 1)]) Close)
                       ,
                       (Case
                          (Choice
                             (ChoiceId "Dispute problem"
                                (Role "Seller")) [
                             (Bound 0 0)])
                          (When [
                                (Case
                                   (Choice
                                      (ChoiceId "Dismiss claim"
                                         (Role "Mediator")) [
                                      (Bound 0 0)])
                                   (Pay
                                      (Role "Buyer")
                                      (Party
                                         (Role "Seller"))
                                      (Token "" "")
                                      (ConstantParam "Price") Close))
                                ,
                                (Case
                                   (Choice
                                      (ChoiceId "Confirm problem"
                                         (Role "Mediator")) [
                                      (Bound 1 1)]) Close)] (SlotParam "Mediation deadline") Close))] (SlotParam "Complaint deadline") Close)))] (SlotParam "Complaint response deadline") Close))] (SlotParam "Payment deadline") Close"""

escrowWithCollateral :: String
escrowWithCollateral =
  """When [
  (Case
     (Deposit
        (Role "Seller")
        (Role "Seller")
        (Token "" "")
        (ConstantParam "Collateral amount"))
     (When [
        (Case
           (Deposit
              (Role "Buyer")
              (Role "Buyer")
              (Token "" "")
              (ConstantParam "Collateral amount"))
           (When [
              (Case
                 (Deposit
                    (Role "Seller")
                    (Role "Buyer")
                    (Token "" "")
                    (ConstantParam "Price"))
                 (When [
                       (Case
                          (Choice
                             (ChoiceId "Everything is alright"
                                (Role "Buyer")) [
                             (Bound 0 0)]) Close)
                       ,
                       (Case
                          (Choice
                             (ChoiceId "Report problem"
                                (Role "Buyer")) [
                             (Bound 1 1)])
                          (Pay
                             (Role "Seller")
                             (Account
                                (Role "Buyer"))
                             (Token "" "")
                             (ConstantParam "Price")
                             (When [
                                   (Case
                                      (Choice
                                         (ChoiceId "Confirm problem"
                                            (Role "Seller")) [
                                         (Bound 1 1)]) Close)
                                   ,
                                   (Case
                                      (Choice
                                         (ChoiceId "Dispute problem"
                                            (Role "Seller")) [
                                         (Bound 0 0)])
                                      (Pay
                                         (Role "Seller")
                                         (Party
                                            (PK "0000000000000000000000000000000000000000000000000000000000000000"))
                                         (Token "" "")
                                         (ConstantParam "Collateral amount")
                                         (Pay
                                            (Role "Buyer")
                                            (Party
                                               (PK "0000000000000000000000000000000000000000000000000000000000000000"))
                                            (Token "" "")
                                            (ConstantParam "Collateral amount") Close)))] (SlotParam "Complaint deadline") Close)))] (SlotParam "Dispute by buyer timeout") Close))] (SlotParam "Deposit of price by buyer timeout") Close))] (SlotParam "Deposit of collateral by buyer timeout") Close))] (SlotParam "Collateral deposit by seller timeout") Close"""

zeroCouponBond :: String
zeroCouponBond =
  """When [
  (Case
     (Deposit
        (Role "Lender")
        (Role "Lender")
        (Token "" "")
        (ConstantParam "Amount"))
     (Pay
        (Role "Lender")
        (Party
           (Role "Borrower"))
        (Token "" "")
        (ConstantParam "Amount")
        (When [
           (Case
              (Deposit
                 (Role "Borrower")
                 (Role "Borrower")
                 (Token "" "")
                 (AddValue
                    (ConstantParam "Interest")
                    (ConstantParam "Amount")))
              (Pay
                 (Role "Borrower")
                 (Party
                    (Role "Lender"))
                 (Token "" "")
                 (AddValue
                    (ConstantParam "Interest")
                    (ConstantParam "Amount")) Close))] (SlotParam "Payback deadline") Close)))] (SlotParam "Loan deadline") Close"""

couponBondGuaranteed :: String
couponBondGuaranteed =
  """When [
  (Case
     (Deposit
        (Role "Lender")
        (Role "Guarantor")
        (Token "" "")
        (AddValue
           (MulValue
              (Constant 3)
              (ConstantParam "Interest instalment"))
           (ConstantParam "Principal")))
     (When [
        (Case
           (Deposit
              (Role "Borrower")
              (Role "Lender")
              (Token "" "")
              (ConstantParam "Principal"))
           (Pay
              (Role "Borrower")
              (Party
                 (Role "Borrower"))
              (Token "" "")
              (ConstantParam "Principal")
              (When [
                 (Case
                    (Deposit
                       (Role "Lender")
                       (Role "Borrower")
                       (Token "" "")
                       (ConstantParam "Interest instalment"))
                    (Pay
                       (Role "Lender")
                       (Party
                          (Role "Lender"))
                       (Token "" "")
                       (ConstantParam "Interest instalment")
                       (Pay
                          (Role "Lender")
                          (Party
                             (Role "Guarantor"))
                          (Token "" "")
                          (ConstantParam "Interest instalment")
                          (When [
                             (Case
                                (Deposit
                                   (Role "Lender")
                                   (Role "Borrower")
                                   (Token "" "")
                                   (ConstantParam "Interest instalment"))
                                (Pay
                                   (Role "Lender")
                                   (Party
                                      (Role "Lender"))
                                   (Token "" "")
                                   (ConstantParam "Interest instalment")
                                   (Pay
                                      (Role "Lender")
                                      (Party
                                         (Role "Guarantor"))
                                      (Token "" "")
                                      (ConstantParam "Interest instalment")
                                      (When [
                                         (Case
                                            (Deposit
                                               (Role "Lender")
                                               (Role "Borrower")
                                               (Token "" "")
                                               (AddValue
                                                  (ConstantParam "Interest instalment")
                                                  (ConstantParam "Principal")))
                                            (Pay
                                               (Role "Lender")
                                               (Party
                                                  (Role "Lender"))
                                               (Token "" "")
                                               (AddValue
                                                  (ConstantParam "Interest instalment")
                                                  (ConstantParam "Principal"))
                                               (Pay
                                                  (Role "Lender")
                                                  (Party
                                                     (Role "Guarantor"))
                                                  (Token "" "")
                                                  (AddValue
                                                     (ConstantParam "Interest instalment")
                                                     (ConstantParam "Principal")) Close)))] 1500 Close))))] 1200 Close))))] 900 Close)))] 600
        (Pay
           (Role "Lender")
           (Party
              (Role "Guarantor"))
           (Token "" "")
           (AddValue
              (MulValue
                 (Constant 3)
                 (ConstantParam "Interest instalment"))
              (ConstantParam "Principal")) Close)))] 300 Close"""

swap :: String
swap =
  """When [
  (Case
     (Deposit
        (Role "Ada provider")
        (Role "Ada provider")
        (Token "" "")
        (MulValue
           (Constant 1000000)
           (ConstantParam "Amount of Ada")))
     (When [
        (Case
           (Deposit
              (Role "Dollar provider")
              (Role "Dollar provider")
              (Token "85bb65" "dollar")
              (ConstantParam "Amount of dollars"))
           (Pay
              (Role "Ada provider")
              (Party
                 (Role "Dollar provider"))
              (Token "" "")
              (MulValue
                 (Constant 1000000)
                 (ConstantParam "Amount of Ada"))
              (Pay
                 (Role "Dollar provider")
                 (Party
                    (Role "Ada provider"))
                 (Token "85bb65" "dollar")
                 (ConstantParam "Amount of dollars") Close)))] (SlotParam "Timeout for dollar deposit") Close))] (SlotParam "Timeout for Ada deposit") Close"""

contractForDifferences :: String
contractForDifferences =
  """When [
  (Case
     (Deposit
        (Role "Party")
        (Role "Party")
        (Token "" "")
        (ConstantParam "Amount paid by party"))
     (When [
        (Case
           (Deposit
              (Role "Counterparty")
              (Role "Counterparty")
              (Token "" "")
              (ConstantParam "Amount paid by counterparty"))
           (When [] (SlotParam "First window beginning")
              (When [
                 (Case
                    (Choice
                       (ChoiceId "Price in first window"
                          (Role "Oracle")) [
                       (Bound 0 1000000000)])
                    (When [] (SlotParam "Second window beginning")
                       (When [
                          (Case
                             (Choice
                                (ChoiceId "Price in second window"
                                   (Role "Oracle")) [
                                (Bound 0 1000000000)])
                             (If
                                (ValueGT
                                   (ChoiceValue
                                      (ChoiceId "Price in first window"
                                         (Role "Oracle")))
                                   (ChoiceValue
                                      (ChoiceId "Price in second window"
                                         (Role "Oracle"))))
                                (Let "Decrease in price"
                                   (SubValue
                                      (ChoiceValue
                                         (ChoiceId "Price in first window"
                                            (Role "Oracle")))
                                      (ChoiceValue
                                         (ChoiceId "Price in second window"
                                            (Role "Oracle"))))
                                   (Pay
                                      (Role "Counterparty")
                                      (Account
                                         (Role "Party"))
                                      (Token "" "")
                                      (Cond
                                         (ValueLT
                                            (UseValue "Decrease in price")
                                            (ConstantParam "Amount paid by counterparty"))
                                         (UseValue "Decrease in price")
                                         (ConstantParam "Amount paid by counterparty")) Close))
                                (If
                                   (ValueLT
                                      (ChoiceValue
                                         (ChoiceId "Price in first window"
                                            (Role "Oracle")))
                                      (ChoiceValue
                                         (ChoiceId "Price in second window"
                                            (Role "Oracle"))))
                                   (Let "Increase in price"
                                      (SubValue
                                         (ChoiceValue
                                            (ChoiceId "Price in second window"
                                               (Role "Oracle")))
                                         (ChoiceValue
                                            (ChoiceId "Price in first window"
                                               (Role "Oracle"))))
                                      (Pay
                                         (Role "Party")
                                         (Account
                                            (Role "Counterparty"))
                                         (Token "" "")
                                         (Cond
                                            (ValueLT
                                               (UseValue "Increase in price")
                                               (ConstantParam "Amount paid by party"))
                                            (UseValue "Increase in price")
                                            (ConstantParam "Amount paid by party")) Close)) Close)))] (SlotParam "Second window deadline") Close)))] (SlotParam "First window deadline") Close)))] (SlotParam "Counterparty deposit deadline") Close))] (SlotParam "Party deposit deadline") Close"""

contractForDifferencesWithOracle :: String
contractForDifferencesWithOracle =
  """When [
  (Case
     (Deposit
        (Role "Party")
        (Role "Party")
        (Token "" "")
        (ConstantParam "Amount paid by party"))
     (When [
        (Case
           (Deposit
              (Role "Counterparty")
              (Role "Counterparty")
              (Token "" "")
              (ConstantParam "Amount paid by counterparty"))
           (When [] (SlotParam "First window beginning")
              (When [
                 (Case
                    (Choice
                       (ChoiceId "dir-adausd"
                          (Role "kraken")) [
                       (Bound 0 100000000000)])
                    (When [] (SlotParam "Second window beginning")
                       (When [
                          (Case
                             (Choice
                                (ChoiceId "inv-adausd"
                                   (Role "kraken")) [
                                (Bound 0 100000000000)])
                             (Let "Price in second window"
                                (DivValue
                                   (MulValue
                                      (ConstantParam "Amount of Ada to use as asset")
                                      (MulValue
                                         (ChoiceValue
                                            (ChoiceId "dir-adausd"
                                               (Role "kraken")))
                                         (ChoiceValue
                                            (ChoiceId "inv-adausd"
                                               (Role "kraken")))))
                                   (Constant 10000000000000000))
                                (If
                                   (ValueGT
                                      (ConstantParam "Amount of Ada to use as asset")
                                      (UseValue "Price in second window"))
                                   (Let "Decrease in price"
                                      (SubValue
                                         (ConstantParam "Amount of Ada to use as asset")
                                         (UseValue "Price in second window"))
                                      (Pay
                                         (Role "Counterparty")
                                         (Account
                                            (Role "Party"))
                                         (Token "" "")
                                         (Cond
                                            (ValueLT
                                               (UseValue "Decrease in price")
                                               (ConstantParam "Amount paid by counterparty"))
                                            (UseValue "Decrease in price")
                                            (ConstantParam "Amount paid by counterparty")) Close))
                                   (If
                                      (ValueLT
                                         (ConstantParam "Amount of Ada to use as asset")
                                         (UseValue "Price in second window"))
                                      (Let "Increase in price"
                                         (SubValue
                                            (UseValue "Price in second window")
                                            (ConstantParam "Amount of Ada to use as asset"))
                                         (Pay
                                            (Role "Party")
                                            (Account
                                               (Role "Counterparty"))
                                            (Token "" "")
                                            (Cond
                                               (ValueLT
                                                  (UseValue "Increase in price")
                                                  (ConstantParam "Amount paid by party"))
                                               (UseValue "Increase in price")
                                               (ConstantParam "Amount paid by party")) Close)) Close))))] (SlotParam "Second window deadline") Close)))] (SlotParam "First window deadline") Close)))] (SlotParam "Counterparty deposit deadline") Close))] (SlotParam "Party deposit deadline") Close"""