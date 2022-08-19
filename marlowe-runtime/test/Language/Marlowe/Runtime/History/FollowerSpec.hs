{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE RecursiveDo    #-}

module Language.Marlowe.Runtime.History.FollowerSpec where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (atomically, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Exception (Exception, catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, ChainSeekClient, Move (..), ScriptHash,
                                               TransactionOutput (..), TxError (TxNotFound), TxId, TxOutRef (..),
                                               WithGenesis (..), toDatum)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId (..), MarloweVersion (..), SomeMarloweVersion (SomeMarloweVersion),
                                          parseContractId)
import Language.Marlowe.Runtime.History.Follower (ContractChanges (..), ContractHistoryError (..), ContractStep (..),
                                                  CreateStep (..), ExtractCreationError (..), Follower (..),
                                                  FollowerDependencies (..), SomeContractChanges (..), mkFollower)
import Network.Protocol.ChainSeek.Client (hoistChainSeekClient)
import qualified PlutusTx.AssocMap as AMap
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
  it "discovers a contract creation" checkCreation

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

createDatum :: V1.MarloweData
createDatum = V1.MarloweData
  { marloweState = createDatumState
  , marloweContract = createContract
  }

createContract :: V1.Contract
createContract = V1.Close

createDatumState :: V1.State
createDatumState = V1.State
  { accounts = AMap.empty
  , choices = AMap.empty
  , boundValues = AMap.empty
  , minTime = 0
  }

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
    datum = Just $ toDatum createDatum
  in
    Chain.TransactionOutput{..}

block1 :: Chain.BlockHeader
block1 = Chain.BlockHeader 0 "" 0

point1 :: ChainPoint
point1 = Chain.At block1

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

checkCreation :: Expectation
checkCreation = do
  let datum = createDatum
  let scriptHash = testScriptHash
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx point1 point1
    $ Assert
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block1 [Create CreateStep {..}]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  -- Should be empty because we already read them in the Assert above and it
  -- resets to empty each time it is read.
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

emptyChanges :: MarloweVersion v -> SomeContractChanges
emptyChanges version = SomeContractChanges version mempty

expectChanges :: MarloweVersion v -> ContractChanges v -> ReaderT Follower IO ()
expectChanges version expectedChanges = do
  Follower{..} <- ask
  liftIO do
    currentChanges <- atomically changes
    currentChanges `shouldBe` Just (SomeContractChanges version expectedChanges)

-- TODO move to a test module in marlowe-protocols and generalize Move -> query
data FollowerTestResult a = FollowerTestResult
  { followerError   :: Maybe ContractHistoryError
  , followerChanges :: Maybe SomeContractChanges
  , testResult      :: a
  }

runFollowerTest
  :: [(ScriptHash, SomeMarloweVersion)]
  -> ChainSeekServerScript Move ChainPoint ChainPoint (ReaderT Follower IO) a
  -> IO (FollowerTestResult a)
runFollowerTest marloweVersions' script = do
  (resultVar, Follower{..}) <- atomically mdo
    resultVar <- newEmptyTMVar
    let
      connectToChainSeek :: ChainSeekClient Move ChainPoint ChainPoint IO b -> IO b
      connectToChainSeek client = do
        (a, mb) <- runReaderT (runClientWithScript show shouldBe script $ hoistChainSeekClient lift client) follower
        atomically $ putTMVar resultVar a
        case mb of
          Just b  -> pure b
          Nothing -> throwIO HaltException
    let contractId = testContractId
    let getMarloweVersion scriptHash = lookup scriptHash marloweVersions'
    follower <- mkFollower FollowerDependencies{..}
    pure (resultVar, follower)
  let
    runFollowerAndSwallowHalt =
      fmap (either Just (const Nothing)) runFollower
        `catch` \HaltException -> pure Nothing
  (followerError, testResult) <- concurrently runFollowerAndSwallowHalt $ atomically $ takeTMVar resultVar
  followerChanges <- atomically changes
  pure FollowerTestResult{..}

data HaltException = HaltException
  deriving (Show, Eq)

instance Exception HaltException where
