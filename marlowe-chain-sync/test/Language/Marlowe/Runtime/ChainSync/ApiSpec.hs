module Language.Marlowe.Runtime.ChainSync.ApiSpec
  ( spec
  ) where

import qualified Cardano.Api as C
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (Address(Address), TxId(TxId), TxIx(TxIx), TxOutRef(TxOutRef))
import Language.Marlowe.Runtime.Transaction.Api
  (ApplyInputsError, MarloweTxCommand(ApplyInputs), Tag(TagApplyInputs), WalletAddresses(WalletAddresses))
import Language.Marlowe.Runtime.Transaction.Arbitrary (genByteString, genSet)
import Network.Protocol.Job.Types (Command(tagFromCommand), getCommand, getResult, putCommand, putResult)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryPositiveInteger)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary), Gen)


spec :: Spec
spec = do
  prop "MsgRequestHandshake serialization roundtrip" do
    msg <- genMsgRequestHandshake
    prop_codecM codecJob (AnyMessageAndAgency (ClientAgency TokInit) msg)
