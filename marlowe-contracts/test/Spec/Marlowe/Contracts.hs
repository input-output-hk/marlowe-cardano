{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Spec.Marlowe.Contracts
where

import Control.Monad (void)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Language.Marlowe
import Ledger (PaymentPubKeyHash (..), PubKeyHash)
import Ledger.Ada
import Marlowe.Contracts.Options
import Marlowe.Contracts.Swap
import Marlowe.Contracts.ZeroCouponBond
import Plutus.Contract.Test as T
import Plutus.Trace.Emulator as Trace
import Plutus.V1.Ledger.Value as V
import qualified PlutusTx.AssocMap as AssocMap
import Test.Tasty

tests :: TestTree
tests = testGroup "Marlowe"
    [ zeroCouponBondTest
    , zeroCouponBondCombinationTest
    , americanCallOptionTest
    , europeanCallOptionTest
    , swapIdentityTest
    ]

reqId :: UUID
reqId = UUID.nil

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash

walletAssertions :: V.Value -> V.Value -> TracePredicate
walletAssertions x y =
  assertNoFailedTransactions
    T..&&. assertDone marlowePlutusContract (walletInstanceTag w1) (const True) "contract should close"
    T..&&. assertDone marlowePlutusContract (walletInstanceTag w2) (const True) "contract should close"
    T..&&. walletFundsChange w1 x
    T..&&. walletFundsChange w2 y
    T..&&. assertAccumState marlowePlutusContract (walletInstanceTag w1) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
    T..&&. assertAccumState marlowePlutusContract (walletInstanceTag w2) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"

w1Pk, w2Pk :: Party
w1Pk = PK (walletPubKeyHash w1)
w2Pk = PK (walletPubKeyHash w2)

-- |Zero coupon bond
zeroCouponBondTest :: TestTree
zeroCouponBondTest = checkPredicateOptions
  defaultCheckOptions
  "Zero Coupon Bond Contract"
  (walletAssertions (lovelaceValueOf 15_000_000) (lovelaceValueOf $ -15_000_000))
  $ do
    let params = defaultMarloweParams
    let zcb =
          zeroCouponBond
            w1Pk
            w2Pk
            (Slot 100)
            (Slot 200)
            (Constant 75_000_000)
            (Constant 90_000_000)
            ada
            Close

    w1Hdl <- activateContractWallet w1 marlowePlutusContract
    w2Hdl <- activateContractWallet w2 marlowePlutusContract

    callEndpoint @"create" w1Hdl (reqId, AssocMap.empty, zcb)
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w1Pk ada 75_000_000])
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w2Pk ada 90_000_000])
    void $ waitNSlots 2

    callEndpoint @"close" w1Hdl reqId
    callEndpoint @"close" w2Hdl reqId
    void $ waitNSlots 2

-- |Combination of two zero coupon bonds
zeroCouponBondCombinationTest :: TestTree
zeroCouponBondCombinationTest = checkPredicateOptions
  defaultCheckOptions
  "Combination of two Zero Coupon Bond Contracts"
  (walletAssertions mempty mempty)
  $ do
    let params = defaultMarloweParams
    let zcb1 =
          zeroCouponBond
            w1Pk
            w2Pk
            (Slot 100)
            (Slot 200)
            (Constant 75_000_000)
            (Constant 90_000_000)
            ada
            Close
    let zcb2 =
          zeroCouponBond
            w2Pk
            w1Pk
            (Slot 100)
            (Slot 200)
            (Constant 75_000_000)
            (Constant 90_000_000)
            ada
            Close

    w1Hdl <- activateContractWallet w1 marlowePlutusContract
    w2Hdl <- activateContractWallet w2 marlowePlutusContract

    callEndpoint @"create" w1Hdl (reqId, AssocMap.empty, zcb1 `both` zcb2)
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w1Pk ada 75_000_000])
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w2Pk ada 90_000_000])
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w2Pk w2Pk ada 75_000_000])
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w2Pk w1Pk ada 90_000_000])
    void $ waitNSlots 2

    callEndpoint @"close" w1Hdl reqId
    callEndpoint @"close" w2Hdl reqId
    void $ waitNSlots 2

-- |American Call option
--
-- Choose not to Exercise
americanCallOptionTest :: TestTree
americanCallOptionTest = checkPredicateOptions
  defaultCheckOptions
  "American Call Option Contract"
  (walletAssertions mempty mempty)
  $ do
    let params = defaultMarloweParams
    let americanCall =
          option
            American
            Call
            w1Pk
            w2Pk
            (ada, Constant 75_000_000)
            (ada, Constant 90_000_000)
            (Slot 100)
            (Slot 200)

    w1Hdl <- activateContractWallet w1 marlowePlutusContract
    w2Hdl <- activateContractWallet w2 marlowePlutusContract

    callEndpoint @"create" w2Hdl (reqId, AssocMap.empty, americanCall)
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 0])
    void $ waitNSlots 2

    callEndpoint @"close" w1Hdl reqId
    callEndpoint @"close" w2Hdl reqId
    void $ waitNSlots 2

-- |European Call option
--
-- Choose not to Exercise
europeanCallOptionTest :: TestTree
europeanCallOptionTest = checkPredicateOptions
  defaultCheckOptions
  "European Call Option Contract"
  (walletAssertions mempty mempty)
  $ do
    let params = defaultMarloweParams
    let americanCall =
          option
            European
            Call
            w1Pk
            w2Pk
            (ada, Constant 75_000_000)
            (ada, Constant 90_000_000)
            (Slot 100)
            (Slot 200)

    w1Hdl <- activateContractWallet w1 marlowePlutusContract
    w2Hdl <- activateContractWallet w2 marlowePlutusContract

    callEndpoint @"create" w2Hdl (reqId, AssocMap.empty, americanCall)
    void $ waitNSlots 100

    callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IChoice (ChoiceId "Exercise Call" w1Pk) 0])
    void $ waitNSlots 2

    callEndpoint @"close" w1Hdl reqId
    callEndpoint @"close" w2Hdl reqId
    void $ waitNSlots 2

swapIdentityTest :: TestTree
swapIdentityTest = checkPredicateOptions
  (changeInitialWalletValue w2 (\v -> v <> singleton "" "testcoin" 300) defaultCheckOptions)
  "Swap and swap back Contract"
  (walletAssertions mempty mempty)
  $ do
    let params = defaultMarloweParams
    let tok = Token "" "testcoin"

    let contract =
          swap w1Pk ada (Constant 10_000_000) w2Pk tok (Constant 30) (Slot 100) $
          swap w2Pk ada (Constant 10_000_000) w1Pk tok (Constant 30) (Slot 200) Close

    w1Hdl <- activateContractWallet w1 marlowePlutusContract
    w2Hdl <- activateContractWallet w2 marlowePlutusContract

    callEndpoint @"create" w1Hdl (reqId, AssocMap.empty, contract)
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w1Pk ada 10_000_000])
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w2Pk w2Pk tok 30])
    void $ waitNSlots 100

    callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w2Pk w2Pk ada 10_000_000])
    void $ waitNSlots 2

    callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w1Pk tok 30])
    void $ waitNSlots 2

    callEndpoint @"close" w1Hdl reqId
    callEndpoint @"close" w2Hdl reqId
    void $ waitNSlots 2
