module Examples.Haskell.Contracts where

example :: String
example =
  """{-# LANGUAGE OverloadedStrings #-}
module Example where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}

contract :: Contract
contract = Close
"""

escrow :: String
escrow =
  """{-# LANGUAGE OverloadedStrings #-}
module Escrow where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

seller, buyer, arbiter :: Party
buyer = Role "Buyer"
seller = Role "Seller"
arbiter = Role "Mediator"

price :: Value
price = ConstantParam "Price"

depositTimeout, disputeTimeout, answerTimeout, arbitrageTimeout :: Timeout
depositTimeout = SlotParam "Payment deadline"
disputeTimeout = SlotParam "Complaint response deadline"
answerTimeout = SlotParam "Complaint deadline"
arbitrageTimeout = SlotParam "Mediation deadline"

choice :: ChoiceName -> Party -> Integer -> Contract -> Case
choice choiceName chooser choiceValue = Case (Choice (ChoiceId choiceName chooser)
                                                     [Bound choiceValue choiceValue])

deposit :: Timeout -> Contract -> Contract -> Contract
deposit timeout timeoutContinuation continuation =
    When [Case (Deposit seller buyer ada price) continuation]
         timeout
         timeoutContinuation

choices :: Timeout -> Party -> Contract -> [(Integer, ChoiceName, Contract)] -> Contract
choices timeout chooser timeoutContinuation list =
    When [choice choiceName chooser choiceValue continuation
          | (choiceValue, choiceName, continuation) <- list]
         timeout
         timeoutContinuation

sellerToBuyer, paySeller :: Contract -> Contract
sellerToBuyer = Pay seller (Account buyer) ada price
paySeller = Pay buyer (Party seller) ada price

refundBuyer :: Contract
refundBuyer
 | explicitRefunds = Pay buyer (Party buyer) ada price Close
 | otherwise = Close

refundSeller :: Contract
refundSeller
 | explicitRefunds = Pay seller (Party seller) ada price Close
 | otherwise = Close

contract :: Contract
contract = deposit depositTimeout Close $
           choices disputeTimeout buyer refundSeller
              [ (0, "Everything is alright"
                , refundSeller
                )
              , (1, "Report problem"
                , sellerToBuyer $
                  choices answerTimeout seller refundBuyer
                     [ (1, "Confirm problem"
                       , refundBuyer
                       )
                     , (0, "Dispute problem"
                       , choices arbitrageTimeout arbiter refundBuyer
                            [ (0, "Dismiss claim"
                              , paySeller
                                Close
                              )
                            , (1, "Confirm problem"
                              , refundBuyer
                              )
                            ]
                       )
                     ]
                )
              ]
"""

escrowWithCollateral :: String
escrowWithCollateral =
  """{-# LANGUAGE OverloadedStrings #-}
module EscrowWithCollateral where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

seller, buyer, burnAddress :: Party
buyer = Role "Buyer"
seller = Role "Seller"
burnAddress = PK "0000000000000000000000000000000000000000000000000000000000000000"

price, collateral :: Value
price = ConstantParam "Price"
collateral = ConstantParam "Collateral amount"

sellerCollateralTimeout, buyerCollateralTimeout, depositTimeout, disputeTimeout, answerTimeout :: Timeout
sellerCollateralTimeout = SlotParam "Collateral deposit by seller timeout"
buyerCollateralTimeout = SlotParam "Deposit of collateral by buyer timeout"
depositTimeout = SlotParam "Deposit of price by buyer timeout"
disputeTimeout = SlotParam "Dispute by buyer timeout"
answerTimeout = SlotParam "Complaint deadline"

depositCollateral :: Party -> Timeout -> Contract -> Contract -> Contract
depositCollateral party timeout timeoutContinuation continuation =
    When [Case (Deposit party party ada collateral) continuation]
         timeout
         timeoutContinuation

burnCollaterals :: Contract -> Contract
burnCollaterals =
    Pay seller (Party burnAddress) ada collateral
    . Pay buyer (Party burnAddress) ada collateral

deposit :: Timeout -> Contract -> Contract -> Contract
deposit timeout timeoutContinuation continuation =
    When [Case (Deposit seller buyer ada price) continuation]
         timeout
         timeoutContinuation

choice :: ChoiceName -> Party -> Integer -> Contract -> Case
choice choiceName chooser choiceValue = Case (Choice (ChoiceId choiceName chooser)
                                                     [Bound choiceValue choiceValue])

choices :: Timeout -> Party -> Contract -> [(Integer, ChoiceName, Contract)] -> Contract
choices timeout chooser timeoutContinuation list =
    When [choice choiceName chooser choiceValue continuation
          | (choiceValue, choiceName, continuation) <- list]
         timeout
         timeoutContinuation

sellerToBuyer :: Contract -> Contract
sellerToBuyer = Pay seller (Account buyer) ada price

refundSellerCollateral :: Contract -> Contract
refundSellerCollateral
  | explicitRefunds = Pay seller (Party seller) ada collateral
  | otherwise = id

refundBuyerCollateral :: Contract -> Contract
refundBuyerCollateral
  | explicitRefunds = Pay buyer (Party buyer) ada collateral
  | otherwise = id

refundCollaterals :: Contract -> Contract
refundCollaterals = refundSellerCollateral . refundBuyerCollateral

refundBuyer :: Contract
refundBuyer
 | explicitRefunds = Pay buyer (Party buyer) ada price Close
 | otherwise = Close

refundSeller :: Contract
refundSeller
 | explicitRefunds = Pay seller (Party seller) ada price Close
 | otherwise = Close

contract :: Contract
contract = depositCollateral seller sellerCollateralTimeout Close $
           depositCollateral buyer buyerCollateralTimeout (refundSellerCollateral Close) $
           deposit depositTimeout (refundCollaterals Close) $
           choices disputeTimeout buyer (refundCollaterals refundSeller)
              [ (0, "Everything is alright"
                , refundCollaterals refundSeller
                )
              , (1, "Report problem"
                , sellerToBuyer $
                  choices answerTimeout seller (refundCollaterals refundBuyer)
                     [ (1, "Confirm problem"
                       , refundCollaterals refundBuyer
                       )
                     , (0, "Dispute problem"
                       , burnCollaterals refundBuyer
                       )
                     ]
                )
              ]
"""

zeroCouponBond :: String
zeroCouponBond =
  """{-# LANGUAGE OverloadedStrings #-}
module ZeroCouponBond where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

discountedPrice, notionalPrice :: Value
discountedPrice = ConstantParam "Amount"
notionalPrice = AddValue (ConstantParam "Interest") discountedPrice

investor, issuer :: Party
investor = Role "Lender"
issuer = Role "Borrower"

initialExchange, maturityExchangeTimeout :: Timeout
initialExchange = SlotParam "Loan deadline"
maturityExchangeTimeout = SlotParam "Payback deadline"

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
"""

couponBondGuaranteed :: String
couponBondGuaranteed =
  """{-# LANGUAGE OverloadedStrings #-}
module CouponBondGuaranteed where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

guarantor, investor, issuer :: Party
guarantor = Role "Guarantor"
investor = Role "Lender"
issuer = Role "Borrower"

principal, instalment :: Value
principal = ConstantParam "Principal"
instalment = ConstantParam "Interest instalment"

guaranteedAmount :: Integer -> Value
guaranteedAmount instalments = AddValue (MulValue (Constant instalments) instalment) principal

lastInstalment :: Value
lastInstalment = AddValue instalment principal

deposit :: Value -> Party -> Party -> Timeout -> Contract -> Contract -> Contract
deposit amount by toAccount timeout timeoutContinuation continuation =
    When [Case (Deposit toAccount by ada amount) continuation]
         timeout
         timeoutContinuation

refundGuarantor :: Value -> Contract -> Contract
refundGuarantor = Pay investor (Party guarantor) ada

transfer :: Value -> Party -> Party -> Timeout -> Contract -> Contract -> Contract
transfer amount from to timeout timeoutContinuation continuation =
    deposit amount from to timeout timeoutContinuation
  $ Pay to (Party to) ada amount
    continuation

giveCollateralToLender :: Value -> Contract
giveCollateralToLender amount
  | explicitRefunds = Pay investor (Party investor) ada amount Close
  | otherwise = Close

contract :: Contract
contract = deposit (guaranteedAmount 3) guarantor investor
                   300 Close
         $ transfer principal investor issuer
                    600 (refundGuarantor (guaranteedAmount 3) Close)
         $ transfer instalment issuer investor
                    900 (giveCollateralToLender $ guaranteedAmount 3)
         $ refundGuarantor instalment
         $ transfer instalment issuer investor
                    1200 (giveCollateralToLender $ guaranteedAmount 2)
         $ refundGuarantor instalment
         $ transfer lastInstalment issuer investor
                    1500 (giveCollateralToLender $ guaranteedAmount 1)
         $ refundGuarantor lastInstalment
           Close
"""

swap :: String
swap =
  """{-# LANGUAGE OverloadedStrings #-}
module Swap where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

lovelacePerAda, amountOfAda, amountOfLovelace, amountOfDollars :: Value
lovelacePerAda = Constant 1000000
amountOfAda = ConstantParam "Amount of Ada"
amountOfLovelace = MulValue lovelacePerAda amountOfAda
amountOfDollars = ConstantParam "Amount of dollars"

adaDepositTimeout, dollarDepositTimeout :: Timeout
adaDepositTimeout = SlotParam "Timeout for Ada deposit"
dollarDepositTimeout = SlotParam "Timeout for dollar deposit"

dollars :: Token
dollars = Token "85bb65" "dollar"

data SwapParty = SwapParty { party    :: Party
                           , currency :: Token
                           , amount   :: Value
                           }

adaProvider, dollarProvider :: SwapParty
adaProvider = SwapParty { party = Role "Ada provider"
                        , currency = ada
                        , amount = amountOfLovelace
                        }
dollarProvider = SwapParty { party = Role "Dollar provider"
                           , currency = dollars
                           , amount = amountOfDollars
                           }

makeDeposit :: SwapParty -> Timeout -> Contract -> Contract -> Contract
makeDeposit src timeout timeoutContinuation continuation =
  When [ Case (Deposit (party src) (party src) (currency src) (amount src))
              continuation
       ] timeout
         timeoutContinuation

refundSwapParty :: SwapParty -> Contract
refundSwapParty swapParty
  | explicitRefunds = Pay (party swapParty) (Party (party swapParty)) (currency swapParty) (amount swapParty) Close
  | otherwise = Close

makePayment :: SwapParty -> SwapParty -> Contract -> Contract
makePayment src dest =
  Pay (party src) (Party $ party dest) (currency src) (amount src)

contract :: Contract
contract = makeDeposit adaProvider adaDepositTimeout Close
         $ makeDeposit dollarProvider dollarDepositTimeout (refundSwapParty adaProvider)
         $ makePayment adaProvider dollarProvider
         $ makePayment dollarProvider adaProvider
           Close
"""

contractForDifferences :: String
contractForDifferences =
  """{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module ContractForDifferences where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

party, counterparty, oracle :: Party
party = Role "Party"
counterparty = Role "Counterparty"
oracle = Role "Oracle"

partyDeposit, counterpartyDeposit, bothDeposits :: Value
partyDeposit = ConstantParam "Amount paid by party"
counterpartyDeposit = ConstantParam "Amount paid by counterparty"
bothDeposits = AddValue partyDeposit counterpartyDeposit

priceBeginning, priceEnd :: ChoiceId
priceBeginning = ChoiceId "Price in first window" oracle
priceEnd = ChoiceId "Price in second window" oracle

decreaseInPrice, increaseInPrice :: ValueId
decreaseInPrice = "Decrease in price"
increaseInPrice = "Increase in price"

initialDeposit :: Party -> Value -> Timeout -> Contract -> Contract -> Contract
initialDeposit by deposit timeout timeoutContinuation continuation =
  When [Case (Deposit by by ada deposit) continuation]
       timeout
       timeoutContinuation

oracleInput :: ChoiceId -> Timeout -> Contract -> Contract -> Contract
oracleInput choiceId timeout timeoutContinuation continuation =
  When [Case (Choice choiceId [Bound 0 1_000_000_000]) continuation]
       timeout
       timeoutContinuation

wait :: Timeout -> Contract -> Contract
wait = When []

gtLtEq :: Value -> Value -> Contract -> Contract -> Contract -> Contract
gtLtEq value1 value2 gtContinuation ltContinuation eqContinuation =
     If (ValueGT value1 value2) gtContinuation
   $ If (ValueLT value1 value2) ltContinuation
                                eqContinuation

recordDifference :: ValueId -> ChoiceId -> ChoiceId -> Contract -> Contract
recordDifference name choiceId1 choiceId2 =
   Let name (SubValue (ChoiceValue choiceId1) (ChoiceValue choiceId2))

transferUpToDeposit :: Party -> Value -> Party -> Value -> Contract -> Contract
transferUpToDeposit from payerDeposit to amount =
   Pay from (Account to) ada (Cond (ValueLT amount payerDeposit) amount payerDeposit)

refund :: Party -> Value -> Contract -> Contract
refund who amount
  | explicitRefunds = Pay who (Party who) ada amount
  | otherwise = id

refundBoth :: Contract
refundBoth = refund party partyDeposit (refund counterparty counterpartyDeposit Close)

refundIfGtZero :: Party -> Value -> Contract -> Contract
refundIfGtZero who amount continuation
  | explicitRefunds = If (ValueGT amount (Constant 0)) (refund who amount continuation) continuation
  | otherwise = continuation

refundUpToBothDeposits :: Party -> Value -> Contract -> Contract
refundUpToBothDeposits who amount
  | explicitRefunds = refund who $ Cond (ValueGT amount bothDeposits) bothDeposits amount
  | otherwise = id

refundAfterDifference :: Party -> Value -> Party -> Value -> Value -> Contract
refundAfterDifference payer payerDeposit payee payeeDeposit difference =
    refundIfGtZero payer (SubValue payerDeposit difference)
  $ refundUpToBothDeposits payee (AddValue payeeDeposit difference)
    Close

contract :: Contract
contract = initialDeposit party partyDeposit (SlotParam "Party deposit deadline") Close
         $ initialDeposit counterparty counterpartyDeposit (SlotParam "Counterparty deposit deadline") (refund party partyDeposit Close)
         $ wait (SlotParam "First window beginning")
         $ oracleInput priceBeginning (SlotParam "First window deadline") refundBoth
         $ wait (SlotParam "Second window beginning")
         $ oracleInput priceEnd (SlotParam "Second window deadline") refundBoth
         $ gtLtEq (ChoiceValue priceBeginning) (ChoiceValue priceEnd)
                  ( recordDifference decreaseInPrice priceBeginning priceEnd
                  $ transferUpToDeposit counterparty counterpartyDeposit party (UseValue decreaseInPrice)
                  $ refundAfterDifference counterparty counterpartyDeposit party partyDeposit (UseValue decreaseInPrice)
                  )
                  ( recordDifference increaseInPrice priceEnd priceBeginning
                  $ transferUpToDeposit party partyDeposit counterparty (UseValue increaseInPrice)
                  $ refundAfterDifference party partyDeposit counterparty counterpartyDeposit (UseValue increaseInPrice)
                  )
                  refundBoth
"""

contractForDifferencesWithOracle :: String
contractForDifferencesWithOracle =
  """{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module ContractForDifferencesWithOracle where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

party, counterparty, oracle :: Party
party = Role "Party"
counterparty = Role "Counterparty"
oracle = Role "kraken"

partyDeposit, counterpartyDeposit, bothDeposits :: Value
partyDeposit = ConstantParam "Amount paid by party"
counterpartyDeposit = ConstantParam "Amount paid by counterparty"
bothDeposits = AddValue partyDeposit counterpartyDeposit

priceBeginning :: Value
priceBeginning = ConstantParam "Amount of Ada to use as asset"

priceEnd :: ValueId
priceEnd = "Price in second window"

exchangeBeginning, exchangeEnd :: ChoiceId
exchangeBeginning = ChoiceId "dir-adausd" oracle
exchangeEnd = ChoiceId "inv-adausd" oracle

decreaseInPrice, increaseInPrice :: ValueId
decreaseInPrice = "Decrease in price"
increaseInPrice = "Increase in price"

initialDeposit :: Party -> Value -> Timeout -> Contract -> Contract -> Contract
initialDeposit by deposit timeout timeoutContinuation continuation =
  When [Case (Deposit by by ada deposit) continuation]
       timeout
       timeoutContinuation

oracleInput :: ChoiceId -> Timeout -> Contract -> Contract -> Contract
oracleInput choiceId timeout timeoutContinuation continuation =
  When [Case (Choice choiceId [Bound 0 100_000_000_000]) continuation]
       timeout
       timeoutContinuation

wait :: Timeout -> Contract -> Contract
wait = When []

gtLtEq :: Value -> Value -> Contract -> Contract -> Contract -> Contract
gtLtEq value1 value2 gtContinuation ltContinuation eqContinuation =
     If (ValueGT value1 value2) gtContinuation
   $ If (ValueLT value1 value2) ltContinuation
                                eqContinuation

recordEndPrice :: ValueId -> ChoiceId -> ChoiceId -> Contract -> Contract
recordEndPrice name choiceId1 choiceId2 =
    Let name (DivValue (MulValue priceBeginning (MulValue (ChoiceValue choiceId1) (ChoiceValue choiceId2)))
                       (Constant 10_000_000_000_000_000))

recordDifference :: ValueId -> Value -> Value -> Contract -> Contract
recordDifference name val1 val2 = Let name (SubValue val1 val2)

transferUpToDeposit :: Party -> Value -> Party -> Value -> Contract -> Contract
transferUpToDeposit from payerDeposit to amount =
   Pay from (Account to) ada (Cond (ValueLT amount payerDeposit) amount payerDeposit)

refund :: Party -> Value -> Contract -> Contract
refund who amount
  | explicitRefunds = Pay who (Party who) ada amount
  | otherwise = id

refundBoth :: Contract
refundBoth = refund party partyDeposit (refund counterparty counterpartyDeposit Close)

refundIfGtZero :: Party -> Value -> Contract -> Contract
refundIfGtZero who amount continuation
  | explicitRefunds = If (ValueGT amount (Constant 0)) (refund who amount continuation) continuation
  | otherwise = continuation

refundUpToBothDeposits :: Party -> Value -> Contract -> Contract
refundUpToBothDeposits who amount
  | explicitRefunds = refund who $ Cond (ValueGT amount bothDeposits) bothDeposits amount
  | otherwise = id

refundAfterDifference :: Party -> Value -> Party -> Value -> Value -> Contract
refundAfterDifference payer payerDeposit payee payeeDeposit difference =
    refundIfGtZero payer (SubValue payerDeposit difference)
  $ refundUpToBothDeposits payee (AddValue payeeDeposit difference)
    Close

contract :: Contract
contract = initialDeposit party partyDeposit (SlotParam "Party deposit deadline") Close
         $ initialDeposit counterparty counterpartyDeposit (SlotParam "Counterparty deposit deadline") (refund party partyDeposit Close)
         $ wait (SlotParam "First window beginning")
         $ oracleInput exchangeBeginning (SlotParam "First window deadline") refundBoth
         $ wait (SlotParam "Second window beginning")
         $ oracleInput exchangeEnd (SlotParam "Second window deadline") refundBoth
         $ recordEndPrice priceEnd exchangeBeginning exchangeEnd
         $ gtLtEq priceBeginning (UseValue priceEnd)
                  ( recordDifference decreaseInPrice priceBeginning (UseValue priceEnd)
                  $ transferUpToDeposit counterparty counterpartyDeposit party (UseValue decreaseInPrice)
                  $ refundAfterDifference counterparty counterpartyDeposit party partyDeposit (UseValue decreaseInPrice)
                  )
                  ( recordDifference increaseInPrice (UseValue priceEnd) priceBeginning
                  $ transferUpToDeposit party partyDeposit counterparty (UseValue increaseInPrice)
                  $ refundAfterDifference party partyDeposit counterparty counterpartyDeposit (UseValue increaseInPrice)
                  )
                  refundBoth
"""