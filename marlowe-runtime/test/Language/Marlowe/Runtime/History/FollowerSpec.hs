{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Language.Marlowe.Runtime.History.FollowerSpec where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (atomically, newEmptyTMVar, putTMVar, takeTMVar)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, ChainSeekClient, Move (..), ScriptHash,
                                               TransactionOutput (..), TxError (TxNotFound), TxId, TxOutRef (..),
                                               WithGenesis (..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId (..), MarloweVersion (..), SomeMarloweVersion (SomeMarloweVersion),
                                          parseContractId)
import Language.Marlowe.Runtime.History.Follower (ContractHistoryError (..), ExtractCreationError (..), Follower (..),
                                                  FollowerDependencies (..), SomeContractChanges, mkFollower)
import Test.Hspec (Expectation, Spec, it, shouldBe)
import Test.Network.Protocol.ChainSeek (ChainSeekServerScript (..), ServerStIdleScript (..), ServerStNextScript (..),
                                        runClientWithScript)

spec :: Spec
spec = do
  it "terminates with HansdshakeFailed" checkHandshakeRejected
  it "terminates with FindTxFailed" checkFindTxFailed
  it "terminates with TxIxNotFound" checkTxIxNotFound
  it "terminates with ByronAddress" checkByronAddress
  it "terminates with NonScriptAddress" checkNonScriptAddress
  it "terminates with InvalidScriptHash" checkInvalidScriptHash
  it "terminates with NoDatum" checkNoDatum
  it "terminates with InvalidDatum" checkInvalidDatum
  it "terminates with NotCreationTransaction" checkNotCreationTransaction

testContractId :: ContractId
testContractId = fromJust $ parseContractId "036e9b4cfdd668f9682d9153950980d7b065455f29b3b47923b2572bdd791e69#0"

testScriptAddress :: Chain.Address
testScriptAddress = "7045da42055944c69f7b1c7840fc15bd0d05ff5e9097f5267b705acf8e"

testScriptHash :: Chain.ScriptHash
testScriptHash = "45da42055944c69f7b1c7840fc15bd0d05ff5e9097f5267b705acf8e"

marloweVersions :: [(ScriptHash, SomeMarloweVersion)]
marloweVersions = [(testScriptHash, SomeMarloweVersion MarloweV1)]

createTxId :: TxId
createTxId = txId $ unContractId testContractId

createTx :: Chain.Transaction
createTx =
  let
    txId = createTxId
    validityRange = Chain.Unbounded
    metadata = Nothing
    inputs = mempty
    outputs = [createOutput]
    mintedTokens = Chain.Tokens mempty
  in
    Chain.Transaction{..}

createOutput :: Chain.TransactionOutput
createOutput =
  let
    address = testScriptAddress
    assets = Chain.Assets
      { ada = 0
      , tokens = Chain.Tokens mempty
      }
    datumHash = Nothing
    datum = Nothing
  in
    Chain.TransactionOutput{..}

point1 :: ChainPoint
point1 = Chain.At $ Chain.BlockHeader 0 "" 0

checkHandshakeRejected :: Expectation
checkHandshakeRejected = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ RejectHandshake [] ()
  followerError `shouldBe` Just HansdshakeFailed
  followerChanges `shouldBe` Nothing

checkFindTxFailed :: Expectation
checkFindTxFailed = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx (txId $ unContractId testContractId))
    $ RejectQuery TxNotFound Genesis
    $ ExpectDone ()
  followerError `shouldBe` Just (FindTxFailed TxNotFound)
  followerChanges `shouldBe` Nothing

checkTxIxNotFound :: Expectation
checkTxIxNotFound = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed TxIxNotFound)
  followerChanges `shouldBe` Nothing

checkByronAddress :: Expectation
checkByronAddress = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [createOutput { address = "" }] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed ByronAddress)
  followerChanges `shouldBe` Nothing

checkNonScriptAddress :: Expectation
checkNonScriptAddress = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [createOutput { address = "6022c79fed0291c432b62f585d3f1074bf3a5f1df86f61fcca14a5d6d6" }] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed NonScriptAddress)
  followerChanges `shouldBe` Nothing

checkInvalidScriptHash :: Expectation
checkInvalidScriptHash = do
  FollowerTestResult{..} <- runFollowerTest []
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [createOutput] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed InvalidScriptHash)
  followerChanges `shouldBe` Nothing

checkNoDatum :: Expectation
checkNoDatum = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [createOutput { Chain.datum = Nothing }] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed NoDatum)
  followerChanges `shouldBe` Nothing

checkInvalidDatum :: Expectation
checkInvalidDatum = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [createOutput { Chain.datum = Just $ Chain.I 0 }] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed InvalidDatum)
  followerChanges `shouldBe` Nothing

checkNotCreationTransaction :: Expectation
checkNotCreationTransaction = do
  let txId = ""
  let txIx = 0
  let address = testScriptAddress
  let redeemer = Nothing
  let txIn = Chain.TransactionInput {..}
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.inputs = Set.singleton txIn } point1 point1
    $ ExpectDone ()
  -- Not a creation transaction because there is a txIn from the script
  -- address, implying that this is an apply-inputs transaction.
  followerError `shouldBe` Just (ExtractContractFailed NotCreationTransaction)
  followerChanges `shouldBe` Nothing

-- TODO move to a test module in marlowe-protocols and generalize Move -> query
data FollowerTestResult a = FollowerTestResult
  { followerError   :: Maybe ContractHistoryError
  , followerChanges :: Maybe SomeContractChanges
  , testResult      :: a
  }

runFollowerTest
  :: [(ScriptHash, SomeMarloweVersion)]
  -> ChainSeekServerScript Move ChainPoint ChainPoint IO a
  -> IO (FollowerTestResult a)
runFollowerTest marloweVersions' script = do
  (resultVar, Follower{..}) <- atomically do
    resultVar <- newEmptyTMVar
    let
      connectToChainSeek :: ChainSeekClient Move ChainPoint ChainPoint IO b -> IO b
      connectToChainSeek client = do
        (a, b) <- runClientWithScript show shouldBe script client
        atomically $ putTMVar resultVar a
        pure b
    let contractId = testContractId
    let getMarloweVersion scriptHash = lookup scriptHash marloweVersions'
    (resultVar,) <$> mkFollower FollowerDependencies{..}
  (followerResult, testResult) <- concurrently runFollower $ atomically $ takeTMVar resultVar
  let followerError = either Just (const Nothing) followerResult
  followerChanges <- atomically changes
  pure FollowerTestResult{..}
