{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Language.Marlowe.CLI.Test.TestCase
  where

import Cardano.Api (Lovelace(Lovelace))
import GHC.Generics (Generic)
import Ledger.Orphans ()

import Contrib.Cardano.Api (lovelaceFromInt, lovelaceToInt)
import Data.Maybe (isNothing)
import Language.Marlowe.CLI.Test.CLI.Types (CLIOperation(..), MarloweValidators(..))
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeOperation(..))
import Language.Marlowe.CLI.Test.Types
  ( FaucetsNumber(FaucetsNumber)
  , TestCase(TestCase, operations)
  , TestOperation(CLIOperation, Fail, RuntimeOperation, Sleep, WalletOperation)
  , TestSuite(TestSuite)
  )
import Language.Marlowe.CLI.Test.Wallet.Types as Wallet
  ( WalletOperation(BurnAll, CheckBalance, CreateWallet, FundWallets, Mint, ReturnFunds, SplitWallet, woIssuer, woWalletNickname)
  , faucetNickname
  )

data TransactionCostUpperBound = TransactionCostUpperBound
  { _tcTxCostUpperBound :: Lovelace
  , _tcPublishingUTxOMinAda :: Lovelace
  }
  deriving stock (Eq, Generic, Show)

-- A quick estimate of the budget required to run an test operation from a faucet perspective.
operationFaucetBudget :: TransactionCostUpperBound -> TestOperation  -> Lovelace
operationFaucetBudget (TransactionCostUpperBound _ publishingMinAda) = lovelaceFromInt . operationBudget'
  where
  fauceOperation possibleNickname = possibleNickname == Just faucetNickname || isNothing possibleNickname

  operationBudget' (WalletOperation (FundWallets nicknames values issuer)) = (sum . map lovelaceToInt $ values) * length nicknames
  operationBudget' (WalletOperation (Mint _ issuer _ tokenDistribution minLovelace)) =
    if fauceOperation issuer
      then lovelaceToInt minLovelace * length tokenDistribution
      else 0
  -- FIXME: Publishing should be managed by the test runner probably separately...
  operationBudget' (CLIOperation Initialize {coMarloweValidators = marloweValidators, coSubmitter=submitter}) =
    if fauceOperation submitter
      then case marloweValidators of
        InTxCurrentValidators -> 0
        ReferenceCurrentValidators {} -> lovelaceToInt publishingMinAda
        ReferenceRuntimeValidators -> 0
    else
      0
  -- FIXME: We should also consider the cost of Marlowe deposits performed by Facuet so:
  -- `Runtime.RuntimeApplyInputs` and `CLI.Prepare` should be analyzed.
  operationBudget' _ = 0

-- | A *realy* rough and quick estimation of the budget required to run a test case.
testFaucetBudgetUpperBound :: TransactionCostUpperBound -> TestCase -> Lovelace
testFaucetBudgetUpperBound tub@(TransactionCostUpperBound txCost txPublishingMinAda) testCase@TestCase {operations} =
  sum (map (operationFaucetBudget tub) operations) <> faucetTxsFees
  where
  faucetTxsFees =
    lovelaceFromInt (lovelaceToInt txCost * length (filter possibleFaucetTx operations))
    where
    -- This is also really quick estimate.
    possibleFaucetTx :: TestOperation -> Bool
    possibleFaucetTx (WalletOperation CreateWallet {}) = False
    possibleFaucetTx (WalletOperation CheckBalance {}) = False
    possibleFaucetTx (WalletOperation ReturnFunds {}) = False
    possibleFaucetTx (WalletOperation FundWallets {}) = True
    possibleFaucetTx (WalletOperation BurnAll {}) = True
    possibleFaucetTx (WalletOperation Mint { woIssuer=issuer}) = maybe True ((==) faucetNickname) issuer
    possibleFaucetTx (WalletOperation SplitWallet { woWalletNickname=walletNickname }) = walletNickname == faucetNickname
    possibleFaucetTx (CLIOperation Initialize { coSubmitter=submitter }) = maybe True ((==) faucetNickname) submitter
    possibleFaucetTx (CLIOperation Prepare {}) = True
    possibleFaucetTx (CLIOperation Publish { coPublisher=publisher }) =  maybe True ((==) faucetNickname) publisher
    possibleFaucetTx (CLIOperation AutoRun {}) = False
    possibleFaucetTx (CLIOperation Withdraw { coWalletNickname=walletNickname }) = walletNickname == faucetNickname
    possibleFaucetTx (RuntimeOperation RuntimeCreateContract { roSubmitter=submitter }) = maybe True ((==) faucetNickname) submitter
    possibleFaucetTx (RuntimeOperation RuntimeApplyInputs { roSubmitter=submitter }) = maybe True ((==) faucetNickname) submitter
    possibleFaucetTx (RuntimeOperation RuntimeWithdraw { roWallets=wallets }) = case wallets of
      Nothing -> True
      Just ws -> faucetNickname `elem` ws
    possibleFaucetTx (RuntimeOperation RuntimeAwaitTxsConfirmed {}) = False
    possibleFaucetTx (RuntimeOperation RuntimeAwaitClosed {}) = False
    possibleFaucetTx Sleep {} = False
    possibleFaucetTx Fail {} = False

-- FIXME: We should probably include permanent lose when we publish marlowe on unspendable address.
testTxsFeesUpperBound :: TransactionCostUpperBound -> TestCase -> Lovelace
testTxsFeesUpperBound (TransactionCostUpperBound txCost _) TestCase {operations} =
  lovelaceFromInt (lovelaceToInt txCost * length (filter possibleTx operations))
  where
  -- This is also really quick estimate.
  possibleTx :: TestOperation -> Bool
  possibleTx (WalletOperation CreateWallet {}) = False
  possibleTx (WalletOperation CheckBalance {}) = False
  possibleTx (WalletOperation ReturnFunds {}) = False
  possibleTx (WalletOperation FundWallets {}) = True
  possibleTx (WalletOperation BurnAll {}) = True
  possibleTx (WalletOperation Mint {}) = True
  possibleTx (WalletOperation SplitWallet {}) = True
  possibleTx (CLIOperation Initialize {}) = True
  possibleTx (CLIOperation Prepare {}) = True
  possibleTx (CLIOperation Publish {}) =  True
  possibleTx (CLIOperation AutoRun {}) = False
  possibleTx (CLIOperation Withdraw {}) = True
  possibleTx (RuntimeOperation RuntimeCreateContract {}) = True
  possibleTx (RuntimeOperation RuntimeApplyInputs {}) = True
  possibleTx (RuntimeOperation RuntimeWithdraw {}) = True
  possibleTx (RuntimeOperation RuntimeAwaitTxsConfirmed {}) = False
  possibleTx (RuntimeOperation RuntimeAwaitClosed {}) = False
  possibleTx Sleep {} = False
  possibleTx Fail {} = False

testsFaucetBudgetUpperBound :: TransactionCostUpperBound -> [TestCase] -> Lovelace
testsFaucetBudgetUpperBound txCosts tests = do
  let
    maximumTestBudget = maximum (map (testFaucetBudgetUpperBound txCosts) tests)
    totalTxCost = sum (testTxsFeesUpperBound txCosts <$> tests)
  -- This is pretty rough estimate but if we assume a single subfaucet and a most
  -- expensive test case as the last one then we can estimate the budget as:
  maximumTestBudget + totalTxCost

