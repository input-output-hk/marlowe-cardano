{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.CLI.Test.TestCase where

import Cardano.Api (Lovelace)
import GHC.Generics (Generic)
import Ledger.Orphans ()

import Contrib.Cardano.Api (lovelaceFromInt, lovelaceToInt)
import Data.Maybe (isNothing)
import Language.Marlowe.CLI.Test.CLI.Types (CLIOperation (..), MarloweValidators (..))
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeOperation (..))
import Language.Marlowe.CLI.Test.Types (
  TestCase (TestCase, operations),
  TestOperation (CLIOperation, Comment, Fail, RuntimeOperation, ShouldFail, Sleep, WalletOperation),
 )
import Language.Marlowe.CLI.Test.Wallet.Types as Wallet (
  WalletOperation (..),
  faucetNickname,
 )

data TxCostsUpperBounds = TxCostsUpperBounds
  { _tcTxMaximumFee :: Lovelace
  , _tcPublishingUTxOMinAda :: Lovelace
  }
  deriving stock (Eq, Generic, Show)

-- A quick estimate of the budget required to run an test operation from a faucet perspective.
operationFaucetBudget :: TxCostsUpperBounds -> TestOperation -> Lovelace
operationFaucetBudget (TxCostsUpperBounds _ publishingMinAda) = lovelaceFromInt . operationBudget'
  where
    faucetOperation possibleNickname = possibleNickname == Just faucetNickname || isNothing possibleNickname

    operationBudget' (WalletOperation (CreateWallet _ possibleValues)) = sum . maybe [] (map lovelaceToInt) $ possibleValues
    operationBudget' (WalletOperation (Fund nicknames values)) = (sum . map lovelaceToInt $ values) * length nicknames
    operationBudget' (WalletOperation (Mint _ issuer _ tokenDistribution minLovelace)) =
      if faucetOperation issuer
        then lovelaceToInt minLovelace * length tokenDistribution
        else 0
    operationBudget' (ShouldFail operation) = operationBudget' operation
    -- FIXME: Publishing should be managed by the test runner probably separately...
    operationBudget' (CLIOperation Initialize{coMarloweValidators = marloweValidators}) =
      case marloweValidators of
        Just InTxCurrentValidators -> 0
        Just ReferenceRuntimeValidators -> 0
        Just (ReferenceCurrentValidators _ publisher) ->
          if faucetOperation publisher
            then lovelaceToInt publishingMinAda
            else 0
        -- By default we publish validators
        Nothing -> lovelaceToInt publishingMinAda
    -- FIXME: We should also consider the cost of Marlowe deposits performed by Facuet so:
    -- `Runtime.RuntimeApplyInputs` and `CLI.Prepare` should be analyzed.
    operationBudget' _ = 0

-- | A *really* rough and quick estimation of the budget required to run a test case.
testFaucetBudgetUpperBound :: TxCostsUpperBounds -> TestCase -> Lovelace
testFaucetBudgetUpperBound tub@(TxCostsUpperBounds txCost _) TestCase{operations} =
  sum (map (operationFaucetBudget tub) operations) <> faucetTxsFees
  where
    faucetTxsFees =
      lovelaceFromInt (lovelaceToInt txCost * length (filter possibleFaucetTx operations))
      where
        -- This is also really quick estimate.
        possibleFaucetTx :: TestOperation -> Bool
        possibleFaucetTx (WalletOperation CreateWallet{..}) = case woPossibleUTxOs of
          Nothing -> False
          Just [] -> False
          Just _ -> True
        possibleFaucetTx (WalletOperation CheckBalance{}) = False
        possibleFaucetTx (WalletOperation ReturnFunds{}) = False
        possibleFaucetTx (WalletOperation Fund{}) = True
        possibleFaucetTx (WalletOperation BurnAll{}) = True
        possibleFaucetTx (WalletOperation Mint{woIssuer = issuer}) = maybe True (faucetNickname ==) issuer
        possibleFaucetTx (WalletOperation SplitWallet{woWalletNickname = walletNickname}) = walletNickname == faucetNickname
        possibleFaucetTx (WalletOperation ExternalCurrency{}) = False
        possibleFaucetTx (WalletOperation ExternalWallet{}) = False
        possibleFaucetTx (CLIOperation Initialize{coSubmitter = submitter}) = maybe True (faucetNickname ==) submitter
        possibleFaucetTx (CLIOperation Prepare{}) = True
        possibleFaucetTx (CLIOperation Publish{coPublisher = publisher}) = maybe True (faucetNickname ==) publisher
        possibleFaucetTx (CLIOperation AutoRun{}) = False
        possibleFaucetTx (CLIOperation Withdraw{coWalletNickname = walletNickname}) = walletNickname == faucetNickname
        possibleFaucetTx (RuntimeOperation RuntimeCreateContract{roSubmitter = submitter}) = maybe True (faucetNickname ==) submitter
        possibleFaucetTx (RuntimeOperation RuntimeApplyInputs{roSubmitter = submitter}) = maybe True (faucetNickname ==) submitter
        possibleFaucetTx (RuntimeOperation RuntimeWithdraw{roWallets = wallets}) = case wallets of
          Nothing -> True
          Just ws -> faucetNickname `elem` ws
        possibleFaucetTx (RuntimeOperation RuntimeAwaitTxsConfirmed{}) = False
        possibleFaucetTx (RuntimeOperation RuntimeAwaitClosed{}) = False
        possibleFaucetTx Sleep{} = False
        possibleFaucetTx Fail{} = False
        possibleFaucetTx Comment{} = False
        possibleFaucetTx (ShouldFail operation) = possibleFaucetTx operation

-- FIXME: We should probably include permanent lose when we publish marlowe on unspendable address.
testTxsFeesUpperBound :: TxCostsUpperBounds -> TestCase -> Lovelace
testTxsFeesUpperBound (TxCostsUpperBounds txCost _) TestCase{operations} =
  lovelaceFromInt (lovelaceToInt txCost * length (filter possibleTx operations))
  where
    -- This is also really quick estimate.
    possibleTx :: TestOperation -> Bool
    possibleTx (WalletOperation CreateWallet{}) = False
    possibleTx (WalletOperation CheckBalance{}) = False
    possibleTx (WalletOperation ReturnFunds{}) = False
    possibleTx (WalletOperation Fund{}) = True
    possibleTx (WalletOperation BurnAll{}) = True
    possibleTx (WalletOperation Mint{}) = True
    possibleTx (WalletOperation SplitWallet{}) = True
    possibleTx (WalletOperation ExternalCurrency{}) = False
    possibleTx (WalletOperation ExternalWallet{}) = False
    possibleTx (CLIOperation Initialize{}) = True
    possibleTx (CLIOperation Prepare{}) = True
    possibleTx (CLIOperation Publish{}) = True
    possibleTx (CLIOperation AutoRun{}) = False
    possibleTx (CLIOperation Withdraw{}) = True
    possibleTx (RuntimeOperation RuntimeCreateContract{}) = True
    possibleTx (RuntimeOperation RuntimeApplyInputs{}) = True
    possibleTx (RuntimeOperation RuntimeWithdraw{}) = True
    possibleTx (RuntimeOperation RuntimeAwaitTxsConfirmed{}) = False
    possibleTx (RuntimeOperation RuntimeAwaitClosed{}) = False
    possibleTx Sleep{} = False
    possibleTx Fail{} = False
    possibleTx Comment{} = False
    possibleTx (ShouldFail operation) = possibleTx operation

testsFaucetBudgetUpperBound :: TxCostsUpperBounds -> [TestCase] -> Lovelace
testsFaucetBudgetUpperBound txCosts tests = do
  let maximumTestBudget = maximum (map (testFaucetBudgetUpperBound txCosts) tests)
      totalTxCost = sum (testTxsFeesUpperBound txCosts <$> tests)
  -- This is pretty rough estimate but if we assume a single subfaucet and a most
  -- expensive test case as the last one then we can estimate the budget as:
  maximumTestBudget + totalTxCost
