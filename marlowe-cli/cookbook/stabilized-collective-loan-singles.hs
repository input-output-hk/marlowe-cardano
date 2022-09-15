{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Aeson (encodeFile, object, (.=))
import Data.Foldable (foldrM)
import Language.Marlowe.CLI.Merkle (deepMerkleize)
import Language.Marlowe.CLI.Types (Continuations)
import Language.Marlowe.Core.V1.Semantics.Types
import Plutus.V1.Ledger.Api (BuiltinByteString, POSIXTime(..), TokenName)



start = 0
delta = 1000
percentStabilization = Constant 40
percentInterest = Constant 10
ultimateDeadline = 100000000000000


main :: IO ()
main =
  let
    (contract, continuations) =
      runWriter
        $ makeContract
          (Constant 40)
          (Constant 10)
          [POSIXTime $ start + i * delta | i <- [0..10]]
  in
    encodeFile "stabilized-collective-loan.merkleization"
      $ object
        [
          "contract"      .= contract
        , "continuations" .= continuations
        ]


makeContract :: Value Observation
             -> Value Observation
             -> [Timeout]
             -> Writer Continuations Contract
makeContract percentStabilization percentInterest timeouts =
  foldrM (step percentStabilization percentInterest) Close timeouts


djed :: Token
djed = Token "00" "DjedUSD"


makeParty :: TokenName -> (Party, Value Observation)
makeParty name =
  let
    party = Role name
  in
    (party, AvailableMoney party djed)


makeChoiceInfo :: ChoiceName -> Party -> (ChoiceId, Value Observation)
makeChoiceInfo name role =
  let
    choiceId = ChoiceId name role
  in
    (choiceId, ChoiceValue choiceId)


makeValueInfo :: BuiltinByteString -> (ValueId, Value Observation)
makeValueInfo name =
  let
    valueId = ValueId name
  in
    (valueId, UseValue valueId)


stabilizer      :: Party
stabilizerFunds :: Value Observation
(stabilizer, stabilizerFunds) = makeParty "Stabilizer"


investor      :: Party
investorFunds :: Value Observation
(investor, investorFunds) = makeParty "Investor"


lender :: Party
lender = Role "Lender"


administrator :: Party
administrator = Role "Administrator"


defaultBound :: [Bound]
defaultBound = [Bound 1 1_000_000_000]


stabilizationBound :: [Bound]
stabilizationBound = [Bound 100_000 10_000_000]


investmentBound :: [Bound]
investmentBound = [Bound 50_000 5_000_000]


loanBound :: [Bound]
loanBound = [Bound 10_000 100_000]


merkleizeTimeout :: Contract -> Writer Continuations Contract
merkleizeTimeout continuation =
  deepMerkleize
    $ When
      [
        Case (Notify TrueObs )
        continuation
      ]
      (POSIXTime ultimateDeadline)
      Close


includeInterest :: Value Observation -> Value Observation -> Value Observation
includeInterest rate value = (value `MulValue` (Constant 100 `AddValue` rate)) `DivValue` Constant 100


computeInterest :: Value Observation -> Value Observation -> Value Observation
computeInterest rate value = (value `MulValue` rate) `DivValue` Constant 100


step :: Value Observation -> Value Observation -> Timeout -> Contract -> Writer Continuations Contract
step stabilizationRate interestRate timeout continuation =
  let
    (stabilizationId, stabilizationValue) = makeChoiceInfo "Stabile"  stabilizer
    (investmentId   , investmentValue   ) = makeChoiceInfo "Invest"   investor
    (withdrawalId   , withdrawalValue   ) = makeChoiceInfo "Withdraw" investor
    (loanId         , loanValue         ) = makeChoiceInfo "Loan"     administrator
    (repaymentId    , repaymentValue    ) = makeChoiceInfo "Repay"    lender
    (investmentsId, investmentsValue) = makeValueInfo "Investments"
    (debtId, debtValue)               = makeValueInfo "Debt"
  in
    deepMerkleize
      . When
        [
          -- The Stabilizer chooses how much to deposit.
          Case (Choice stabilizationId stabilizationBound)
            continuation
          -- The Stabilizer makes a deposit.
        , Case (Deposit stabilizer stabilizer djed stabilizationValue)
            continuation
          -- The Investor chooses how much to deposit.
        , Case (Choice investmentId investmentBound)
            continuation
          -- The Investor makes a deposit.
        , Case (Deposit investor investor djed investmentValue)
            $ Let investmentsId (investmentsValue `AddValue` investmentValue)
              continuation
          -- The Investor makes a withdrawal.
        , Case (Choice withdrawalId defaultBound)
            $ If (withdrawalValue `ValueLE` investorFunds)
                (
                  Pay investor (Party investor) djed withdrawalValue
                    $ Let investmentsId (investmentsValue `SubValue` withdrawalValue)
                      continuation
                )
                (
                  Pay investor (Party investor) djed investorFunds
                    $ Let investmentsId (Constant 0)
                      continuation
                )
          -- The Administrator makes a loan to the Lender.
        , Case (Choice loanId loanBound)
            $ let
                debt' = debtValue `AddValue` includeInterest interestRate loanValue
              in
                If ((loanValue `ValueLE` investmentsValue) `AndObs` (stabilizerFunds `ValueGE` computeInterest stabilizationRate debt'))
                  (
                    Let debtId debt'
                      $ Let investmentsId (investmentsValue `SubValue` loanValue)
                      $ Pay investor (Party lender) djed loanValue
                        continuation
                  )
                  continuation
          -- The Lender chooses to repay a loan.
        , Case (Choice repaymentId defaultBound)
            continuation
          -- The Lender repays a loan.
        , Case (Deposit investor lender djed repaymentValue)
            $ If (repaymentValue `ValueLE` debtValue)
               (
                 Let investmentsId (investmentsValue `AddValue` repaymentValue)
                   $ Let debtId (debtValue `SubValue` repaymentValue)
                     continuation
               )
               (
                 Pay investor (Account stabilizer) djed (repaymentValue `SubValue` debtValue)
                   $ Let investmentsId (investmentsValue `AddValue` debtValue)
                   $ Let debtId (Constant 0)
                     continuation
               )
          -- Administrator chooses to end the contract.
        , Case (Choice (ChoiceId "Terminate" administrator) [Bound 1 1])
            $ Pay stabilizer (Account investor) djed debtValue
              Close
      ]
      timeout
    =<< merkleizeTimeout continuation
