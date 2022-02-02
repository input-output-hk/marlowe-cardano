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
import Language.Marlowe.Client
import Language.Marlowe.SemanticsTypes
import Language.Marlowe.Util
import Ledger (PaymentPubKeyHash (..), PubKeyHash, Slot (..))
import Ledger.Ada (lovelaceValueOf)
import Marlowe.Contracts.ZeroCouponBond
import Plutus.Contract.Test hiding ((.&&.))
import qualified Plutus.Contract.Test as T
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.AssocMap as AssocMap
import Test.Tasty

tests :: TestTree
tests = testGroup "Marlowe"
    [ zeroCouponBondTest
    , zeroCouponBondCombinationTest
    ]

reqId :: UUID
reqId = UUID.nil

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash

-- |Zero coupon bond
zeroCouponBondTest :: TestTree
zeroCouponBondTest = checkPredicateOptions defaultCheckOptions "Zero Coupon Bond Contract"
    (assertNoFailedTransactions
    T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag w1) (const True) "contract should close"
    T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag w2) (const True) "contract should close"
    T..&&. walletFundsChange w1 (lovelaceValueOf 15_000_000)
    T..&&. walletFundsChange w2 (lovelaceValueOf (-15_000_000))
    T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag w1) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
    T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag w2) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
    ) $ do
    -- Init a contract
    let w1Pk = PK (walletPubKeyHash w1)
        w2Pk = PK (walletPubKeyHash w2)

    let params = defaultMarloweParams
    let zcb = zeroCouponBond
                (Slot 100)
                (Slot 200)
                (Constant 75_000_000)
                (Constant 90_000_000)
                w1Pk
                w2Pk

    w1Hdl <- Trace.activateContractWallet w1 marlowePlutusContract
    w2Hdl <- Trace.activateContractWallet w2 marlowePlutusContract

    Trace.callEndpoint @"create" w1Hdl (reqId, AssocMap.empty, zcb)
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w1Pk ada 75_000_000])
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w2Pk ada 90_000_000])
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"close" w1Hdl reqId
    Trace.callEndpoint @"close" w2Hdl reqId
    void $ Trace.waitNSlots 2

-- |Combination of two zero coupon bonds
zeroCouponBondCombinationTest :: TestTree
zeroCouponBondCombinationTest = checkPredicateOptions defaultCheckOptions "Zero Coupon Bond Combination Contract"
    (assertNoFailedTransactions
     T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag w1) (const True) "contract should close"
     T..&&. assertDone marlowePlutusContract (Trace.walletInstanceTag w2) (const True) "contract should close"
     T..&&. walletFundsChange w1 (lovelaceValueOf 0)
     T..&&. walletFundsChange w2 (lovelaceValueOf 0)
     T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag w1) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
     T..&&. assertAccumState marlowePlutusContract (Trace.walletInstanceTag w2) ((==) (Just $ EndpointSuccess reqId CloseResponse)) "should be OK"
    ) $ do
    -- Init a contract
    let w1Pk = PK (walletPubKeyHash w1)
        w2Pk = PK (walletPubKeyHash w2)

    let params = defaultMarloweParams
    let zcb1 = zeroCouponBond
                (Slot 100)
                (Slot 200)
                (Constant 75_000_000)
                (Constant 90_000_000)
                w1Pk
                w2Pk
    let zcb2 = zeroCouponBond
                (Slot 100)
                (Slot 200)
                (Constant 75_000_000)
                (Constant 90_000_000)
                w2Pk
                w1Pk

    w1Hdl <- Trace.activateContractWallet w1 marlowePlutusContract
    w2Hdl <- Trace.activateContractWallet w2 marlowePlutusContract

    Trace.callEndpoint @"create" w1Hdl (reqId, AssocMap.empty, zcb1 `both` zcb2)
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w1Pk ada 75_000_000])
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w1Pk w2Pk ada 90_000_000])
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"apply-inputs" w2Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w2Pk w2Pk ada 75_000_000])
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"apply-inputs" w1Hdl (reqId, params, Nothing, [ClientInput $ IDeposit w2Pk w1Pk ada 90_000_000])
    void $ Trace.waitNSlots 2

    Trace.callEndpoint @"close" w1Hdl reqId
    Trace.callEndpoint @"close" w2Hdl reqId
    void $ Trace.waitNSlots 2

