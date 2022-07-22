{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Marlowe.Contracts.LoanPool where

import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Common
import qualified Plutus.V1.Ledger.Value as Ledger
import PlutusTx.Prelude (consByteString)

numberOfInvestorTokens :: Integer
numberOfInvestorTokens = 100_000_000_000

numberOfInvestors :: Int
numberOfInvestors = 2 -- 100

numberOfLenders :: Integer
numberOfLenders = 3 -- 1000

investor :: Party
investor = Role "Investor"

pool :: Party
pool = Role "LoanPool"

lenders :: [Party]
lenders =
  let base = 48
   in map (\i -> Role $ Ledger.TokenName $ consByteString i "L") [base..base+numberOfLenders]

investorToken :: Token
investorToken = Token tokSymbol tokName
  where
    tokSymbol = ""
    tokName = "InvestorToken"

loanPool :: Timeout -> Contract
loanPool
  timeout
  =
  -- pool setup (pre-minted tokens)
    setup

  -- investing
  $ investments

  -- applying for loans
  $ loans

  -- repayments of loans
  $ repayments

  -- refunding investors
  $ refunds

    Close

 where
   setup cont = deposit pool pool (investorToken, Constant numberOfInvestorTokens) timeout Close cont

   investments cont = foldr invest cont $ replicate numberOfInvestors investor
   invest party cont =
     let choiceId = ChoiceId "deposit" party
         amountId = ValueId "amount"
      in
      When
        [ Case
          (Choice choiceId [Bound 1_000_000 100_000_000_000])
          ( Let amountId (ChoiceValue choiceId)
          $ Pay pool (Account party) investorToken (UseValue amountId)
          $ deposit pool party (ada, UseValue amountId) (timeout + oneDay) Close cont)
        ] (timeout + oneDay) Close

   loans cont = foldr loan cont lenders
   loan party cont =
     let choiceId = ChoiceId "need" party
         amountId = ValueId "amount"
      in
      When
        [ Case
          (Choice choiceId [Bound 1_000_000 100_000_000])
          (Let amountId (ChoiceValue choiceId)
          $ Pay pool (Party party) ada (UseValue amountId) cont)
        ] (timeout + oneDay) Close

   repayments cont = foldr repayment cont lenders
   repayment party cont =
     let choiceId = ChoiceId "need" party
         amountId = ValueId "amount"
      in Let amountId (ChoiceValue choiceId)
       $ deposit pool party (ada, AddValue (UseValue amountId) surcharge) (timeout + oneMonth) Close cont

   refunds cont = Let "refunded" (AvailableMoney pool ada) $ Let "invested" (AvailableMoney investor investorToken) $ foldr refund cont $ replicate numberOfInvestors investor
   refund party cont =
     let choiceId = ChoiceId "deposit" party
         amountId = ValueId "amount"
      in
      When
        [ Case
          (Choice choiceId [Bound 1_000_000 100_000_000_000])
          ( Let amountId (ChoiceValue choiceId)
          $ deposit pool party (investorToken, UseValue amountId) (timeout + oneMonth + oneDay) Close
          $ Pay pool (Party party) ada (DivValue (MulValue (UseValue "refunded") (UseValue amountId)) (UseValue "invested"))
            cont)
        ] (timeout + oneMonth + oneDay) Close

surcharge :: Value
surcharge = Constant 1_000_000

oneDay :: Timeout
oneDay = 86400

oneMonth :: Timeout
oneMonth = 30 * oneDay
