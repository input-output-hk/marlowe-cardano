{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -w #-}
module Spec.Marlowe.AutoExecute
    ( tests
    )
where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import Language.Marlowe.Analysis.FSSemantics
import Language.Marlowe.Client
import Language.Marlowe.Semantics
import Language.Marlowe.Semantics.Types
import Language.Marlowe.Util

import Data.Aeson (decode, encode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.Ratio ((%))
import Data.String
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import qualified Codec.CBOR.Write as Write
import qualified Codec.Serialise as Serialise
import Language.Haskell.Interpreter (Extension (OverloadedStrings), MonadInterpreter, OptionVal ((:=)), as, interpret,
                                     languageExtensions, runInterpreter, set, setImports)
import Plutus.Contract.Test as T
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Lattice

import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import Ledger hiding (Value)
import qualified Ledger
import Ledger.Ada (adaValueOf, lovelaceValueOf)
import Ledger.Typed.Scripts (validatorScript)
import Plutus.V1.Ledger.Ada (Ada (..), fromValue)
import qualified PlutusTx.Prelude as P
import Spec.Marlowe.Common
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant if" -}

tests :: TestTree
tests = testGroup "Marlowe Auto Execution"
    [ awaitUntilTimeoutTest
    , autoexecZCBTest
    , autoexecZCBTestAliceWalksAway
    , autoexecZCBTestBobWalksAway
    ]


alice, bob, carol :: Wallet
alice = T.w1
bob = T.w2
carol = T.w3

reqId :: UUID
reqId = UUID.nil

almostAll :: Ledger.Value
almostAll = defaultLovelaceAmount <> P.inv (lovelaceValueOf 6_000_000)

autoexecZCBTest :: TestTree
autoexecZCBTest = checkPredicate "ZCB Auto Execute Contract"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag alice) "contract should not have any errors"
    T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag bob) "contract should not have any errors"
    T..&&. walletFundsChange alice (lovelaceValueOf 15_000_000)
    T..&&. walletFundsChange bob (lovelaceValueOf (-15_000_000))
    T..&&. walletFundsChange carol (lovelaceValueOf 0)
    ) $ do

    bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
    aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
    slotCfg <- Trace.getSlotConfig

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (reqId, params, bobPk, contractLifespan slotCfg)

    -- Init a contract
    Trace.callEndpoint @"create" aliceHdl (reqId, AssocMap.empty, zeroCouponBond slotCfg)
    Trace.waitNSlots 1

    -- Move all Alice's money to Carol, so she can't make a payment
    logInfo @String "Move all Alice's money to Carol, so she can't make a payment"
    Trace.payToWallet alice carol almostAll
    Trace.waitNSlots 1

    Trace.callEndpoint @"auto" aliceHdl (reqId, params, alicePk, contractLifespan slotCfg)
    Trace.waitNSlots 1

    -- Return money to Alice
    logInfo @String "Return money to Alice"
    Trace.payToWallet carol alice almostAll
    Trace.waitNSlots 2

    -- Now Alice should be able to retry and pay to Bob
    void $ Trace.waitNSlots 5


autoexecZCBTestAliceWalksAway :: TestTree
autoexecZCBTestAliceWalksAway = checkPredicate
    "ZCB Auto Execute Contract when Alice walks away"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag alice) "contract should not have any errors"
    T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag bob) "contract should not have any errors"
    T..&&. walletFundsChange alice (P.inv  almostAll)
    T..&&. walletFundsChange carol almostAll
    ) $ do
    bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
    aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
    slotCfg <- Trace.getSlotConfig

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (reqId, params, bobPk, contractLifespan slotCfg)

    -- Init a contract, 100 ADA - 5 ADA = 95 ADA.
    Trace.callEndpoint @"create" aliceHdl (reqId, AssocMap.empty, zeroCouponBond slotCfg)
    Trace.waitNSlots 1

    -- Move all Alice's money to Carol, so she can't make a payment, 95 ADA - 95 ADA = 0 ADA.
    Trace.payToWallet alice carol almostAll
    Trace.waitNSlots 1

    Trace.callEndpoint @"auto" aliceHdl (reqId, params, alicePk, contractLifespan slotCfg)
    Trace.waitNSlots 1
    -- Alice needs to stop auto-executing the contract when he walks away:
    -- otherwise, both Alice and Bob try to close the contract.
    Trace.freezeContractInstance $ Trace.chInstanceId aliceHdl
    Trace.waitNSlots 20
    -- Here Alice deposit timeout happened, so Bob should Close the contract
    void $ Trace.waitNSlots 1


autoexecZCBTestBobWalksAway :: TestTree
autoexecZCBTestBobWalksAway = checkPredicate
    "ZCB Auto Execute Contract when Bob walks away"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag alice) "contract should not have any errors"
    T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag bob) "contract should not have any errors"
    T..&&. walletFundsChange alice (lovelaceValueOf (-75_000_000))
    T..&&. walletFundsChange carol almostAll
    ) $ do
    bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
    aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
    slotCfg <- Trace.getSlotConfig

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (reqId, params, bobPk, contractLifespan slotCfg)

    -- Init a contract
    Trace.callEndpoint @"create" aliceHdl (reqId, AssocMap.empty, zeroCouponBond slotCfg)
    Trace.waitNSlots 1

    Trace.payToWallet bob carol almostAll
    Trace.waitNSlots 1

    Trace.callEndpoint @"auto" aliceHdl (reqId, params, alicePk, contractLifespan slotCfg)
    Trace.waitNSlots 1 -- Alice pays to Bob
    Trace.waitNSlots 15 -- Bob can't pay back
    Trace.waitNSlots 15 -- Bob can't pay back
    -- Bob needs to stop auto-executing the contract when he walks away:
    -- otherwise, both Alice and Bob try to close the contract.
    Trace.freezeContractInstance $ Trace.chInstanceId bobHdl
    void $ Trace.waitNSlots 15 -- Bob can't pay back, walks away


awaitUntilTimeoutTest :: TestTree
awaitUntilTimeoutTest = checkPredicate "Party waits for contract to appear on chain until timeout"
    (assertNoFailedTransactions
    -- /\ emulatorLog (const False) ""
    T..&&. assertNotDone marlowePlutusContract (Trace.walletInstanceTag bob) "contract should close"
    ) $ do

    bobHdl <- Trace.activateContractWallet bob marlowePlutusContract
    aliceHdl <- Trace.activateContractWallet alice marlowePlutusContract
    slotCfg <- Trace.getSlotConfig

    -- Bob will wait for the contract to appear on chain
    Trace.callEndpoint @"auto" bobHdl (reqId, params, bobPk, contractLifespan slotCfg)

    Trace.waitNSlots 15
    Trace.waitNSlots 15
    -- here Bob gets Timeout and closes the contract
    void $ Trace.waitNSlots 15

alicePk = PK . unPaymentPubKeyHash . mockWalletPaymentPubKeyHash $ alice
bobPk   = PK . unPaymentPubKeyHash . mockWalletPaymentPubKeyHash $ bob

params = defaultMarloweParams

zeroCouponBond slotCfg = do
    let seconds = secondsSinceShelley slotCfg
    When [ Case
        (Deposit alicePk alicePk ada (Constant 75_000_000))
        (Pay alicePk (Party bobPk) ada (Constant 75_000_000)
            (When
                [ Case (Deposit alicePk bobPk ada (Constant 90_000_000)) Close] (seconds 40) Close
            ))] (seconds 20) Close

contractLifespan slotCfg = contractLifespanUpperBound (zeroCouponBond slotCfg)

defaultLovelaceAmount :: Ledger.Value
defaultLovelaceAmount = defaultDist Map.! alice
