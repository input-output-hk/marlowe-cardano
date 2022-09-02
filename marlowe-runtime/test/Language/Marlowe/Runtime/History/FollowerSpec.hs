{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE RecursiveDo    #-}

module Language.Marlowe.Runtime.History.FollowerSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (atomically, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Exception (Exception, catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Language.Marlowe (POSIXTime (..))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, ChainSeekClient, Move (..), ScriptHash, SlotConfig (..),
                                               TransactionOutput (address), TxError (..), TxId, TxOutRef (..),
                                               UTxOError (..), WithGenesis (..), hoistChainSeekClient, toDatum)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId (..), MarloweVersion (..), MarloweVersionTag (..), Payout (..),
                                          SomeMarloweVersion (..), Transaction (..), TransactionOutput (..),
                                          TransactionScriptOutput (..), parseContractId)
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.History.Follower
import qualified Plutus.V1.Ledger.Api as Plutus
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
  it "terminates with NoCreateDatum" checkNoCreateDatum
  it "terminates with InvalidCreateDatum" checkInvalidCreateDatum
  it "terminates with NotCreationTransaction" checkNotCreationTransaction
  it "discovers a contract creation" checkCreation
  it "discovers a contract creation in the same tx as a previous close" checkCreationWithClose
  it "terminates with FollowScriptUTxOFailed" checkFollowScriptUTxOFailed
  it "terminates with TxInNotFound" checkTxInNotFound
  it "terminates with NoRedeemer" checkNoRedeemer
  it "terminates with InvalidRedeemer" checkInvalidRedeemer
  it "terminates with InvalidValidityRange for Unbounded" checkInvalidValidityRangeUnbounded
  it "terminates with InvalidValidityRange for MinBound" checkInvalidValidityRangeMinBound
  it "terminates with InvalidValidityRange for MaxBound" checkInvalidValidityRangeMaxBound
  it "terminates with NoTransactionDatum" checkNoTransactionDatum
  it "terminates with InvalidTransactionDatum" checkInvalidTransactionDatum
  it "terminates with CreateTxRolledBack (to genesis, no inputs applied)" checkCreateTxRolledBackGenesisNoInputs
  it "terminates with CreateTxRolledBack (to genesis, inputs applied)" checkCreateTxRolledBackGenesisWithInputs
  it "terminates with CreateTxRolledBack (to genesis, closed)" checkCreateTxRolledBackGenesisClosed
  it "terminates with CreateTxRolledBack (no inputs applied)" checkCreateTxRolledBackNoInputs
  it "terminates with CreateTxRolledBack (inputs applied)" checkCreateTxRolledBackWithInputs
  it "terminates with CreateTxRolledBack (closed)" checkCreateTxRolledBackClosed
  it "handles rolling back to creation (inputs applied)" checkRollbackToCreationWithInputs
  it "handles rolling back to creation (closed)" checkRollbackToCreationClosed
  it "handles rolling back to transaction" checkRollbackToTransaction
  it "discovers a contract transaction (close)" checkCloseTransaction
  it "discovers a contract transaction (non-close)" checkNonCloseTransaction
  it "discovers a contract transaction (close, create new)" checkCloseAndCreateInSameTransaction
  it "discovers a payout (open, redeemed before next input))" checkPayoutOpenRedeemedBefore
  it "discovers a payout (open, redeemed after next input))" checkPayoutOpenRedeemedAfter
  it "discovers a payout (open, redeemed with next input))" checkPayoutOpenRedeemedTogether

testContractId :: ContractId
testContractId = fromJust $ parseContractId "036e9b4cfdd668f9682d9153950980d7b065455f29b3b47923b2572bdd791e69#0"

testScriptAddress :: Chain.Address
testScriptAddress = "7045da42055944c69f7b1c7840fc15bd0d05ff5e9097f5267b705acf8e"

testPayoutValidatorAddress :: Chain.Address
testPayoutValidatorAddress = "70da4542055944c69f7b1c7840fc15bd0d05ff5e9097f5267b705acf8e"

testPayoutValidatorHash :: Chain.ScriptHash
testPayoutValidatorHash = "da4542055944c69f7b1c7840fc15bd0d05ff5e9097f5267b705acf8e"

testScriptHash :: Chain.ScriptHash
testScriptHash = "45da42055944c69f7b1c7840fc15bd0d05ff5e9097f5267b705acf8e"

marloweVersions :: [(ScriptHash, (SomeMarloweVersion, ScriptHash))]
marloweVersions = [(testScriptHash, (SomeMarloweVersion MarloweV1, testPayoutValidatorHash))]

createUTxO :: TxOutRef
createUTxO = unContractId testContractId

createTxId :: TxId
createTxId = txId createUTxO

createDatum :: V1.MarloweData
createDatum = V1.MarloweData
  { marloweState = createDatumState
  , marloweContract = createContract
  }

createContract :: V1.Contract
createContract = V1.When
  [ V1.Case (V1.Notify V1.TrueObs) V1.Close
  ]
  (POSIXTime 100_001)
  V1.Close

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

closeTxIn :: Chain.TransactionInput
closeTxIn =
  let
    redeemer = Just $ Chain.Redeemer $ toDatum closeRedeemer
  in
    Chain.TransactionInput createTxId 0 testScriptAddress (Just $ toDatum createDatum) redeemer

closeRedeemer :: [V1.Input]
closeRedeemer = [ V1.NormalInput V1.INotify ]

closeTxId :: TxId
closeTxId = "0000000000000000000000000000000000000000000000000000000000000000"

closeTx :: Chain.Transaction
closeTx =
  let
    txId = closeTxId
    validityRange = Chain.MinMaxBound 0 100
    metadata = Nothing
    inputs = Set.singleton closeTxIn
    outputs = []
    mintedTokens = Chain.Tokens mempty
  in
    Chain.Transaction{..}

applyInputsRedeemer :: [V1.Input]
applyInputsRedeemer = []

applyInputsTxIn :: Chain.TransactionInput
applyInputsTxIn =
  let
    redeemer = Just $ Chain.Redeemer $ toDatum applyInputsRedeemer
  in
    Chain.TransactionInput createTxId 0 testScriptAddress (Just $ toDatum createDatum) redeemer

applyInputsTxId :: TxId
applyInputsTxId = "0000000000000000000000000000000000000000000000000000000000000001"

applyInputsUTxO :: TxOutRef
applyInputsUTxO = TxOutRef applyInputsTxId 0

applyInputsTx :: Chain.Transaction
applyInputsTx =
  let
    txId = applyInputsTxId
    validityRange = Chain.MinMaxBound 0 100
    metadata = Nothing
    inputs = Set.singleton applyInputsTxIn
    outputs = [applyInputsOutput]
    mintedTokens = Chain.Tokens mempty
  in
    Chain.Transaction{..}

applyInputsOutput :: Chain.TransactionOutput
applyInputsOutput =
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

applyInputsPayoutTx :: Chain.Transaction
applyInputsPayoutTx = applyInputsTx { Chain.outputs = [applyInputsOutput, payoutOutput] }

payout :: Payout 'V1
payout =
  let
    assets = Chain.Assets
      { ada = 100
      , tokens = Chain.Tokens mempty
      }
    datum = Chain.TokenName "test_role"
  in
    Payout{..}

payoutUTxO :: TxOutRef
payoutUTxO = TxOutRef applyInputsTxId 1

payoutOutput :: Chain.TransactionOutput
payoutOutput =
  let
    address = testPayoutValidatorAddress
    Payout{assets} = payout
    datumHash = Nothing
    datum = Just $ toDatum $ Plutus.TokenName "test_role"
  in
    Chain.TransactionOutput{..}

redeemPayoutTxId :: TxId
redeemPayoutTxId = "0000000000000000000000000000000000000000000000000000000000000003"

redeemPayoutTxIn :: Chain.TransactionInput
redeemPayoutTxIn =
  let
    Chain.TransactionOutput{datum} = payoutOutput
  in
    Chain.TransactionInput applyInputsTxId 1 testScriptAddress datum Nothing

redeemPayoutTx :: Chain.Transaction
redeemPayoutTx =
  let
    txId = redeemPayoutTxId
    validityRange = Chain.Unbounded
    metadata = Nothing
    inputs = Set.singleton redeemPayoutTxIn
    outputs = [redeemPayoutOutput]
    mintedTokens = Chain.Tokens mempty
  in
    Chain.Transaction{..}

redeemPayoutOutput :: Chain.TransactionOutput
redeemPayoutOutput =
  let
    address = "6022c79fed0291c432b62f585d3f1074bf3a5f1df86f61fcca14a5d6d6"
    Payout{assets} = payout
    datumHash = Nothing
    datum = Nothing
  in
    Chain.TransactionOutput{..}

close2TxIn :: Chain.TransactionInput
close2TxIn =
  let
    redeemer = Just $ Chain.Redeemer $ toDatum close2Redeemer
  in
    Chain.TransactionInput applyInputsTxId 0 testScriptAddress (Just $ toDatum createDatum) redeemer

close2Redeemer :: [V1.Input]
close2Redeemer = [ V1.NormalInput V1.INotify ]

close2TxId :: TxId
close2TxId = "0000000000000000000000000000000000000000000000000000000000000002"

close2Tx :: Chain.Transaction
close2Tx =
  let
    txId = close2TxId
    validityRange = Chain.MinMaxBound 0 100
    metadata = Nothing
    inputs = Set.singleton close2TxIn
    outputs = []
    mintedTokens = Chain.Tokens mempty
  in
    Chain.Transaction{..}

block1 :: Chain.BlockHeader
block1 = Chain.BlockHeader 0 "" 0

point1 :: ChainPoint
point1 = Chain.At block1

block2 :: Chain.BlockHeader
block2 = Chain.BlockHeader 1 "" 1

point2 :: ChainPoint
point2 = Chain.At block2

block3 :: Chain.BlockHeader
block3 = Chain.BlockHeader 3 "" 3

point3 :: ChainPoint
point3 = Chain.At block3

block4 :: Chain.BlockHeader
block4 = Chain.BlockHeader 4 "" 4

point4 :: ChainPoint
point4 = Chain.At block4

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

checkNoCreateDatum :: Expectation
checkNoCreateDatum = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [createOutput { Chain.datum = Nothing }] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed NoCreateDatum)
  followerChanges `shouldBe` Nothing

checkInvalidCreateDatum :: Expectation
checkInvalidCreateDatum = do
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx { Chain.outputs = [createOutput { Chain.datum = Just $ Chain.I 0 }] } point1 point1
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractContractFailed InvalidCreateDatum)
  followerChanges `shouldBe` Nothing

checkNotCreationTransaction :: Expectation
checkNotCreationTransaction = do
  let txId = ""
  let txIx = 0
  let address = testScriptAddress
  let redeemer = Nothing
  let datumBytes = Nothing
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
  let scriptAddress = testScriptAddress
  let payoutValidatorHash = testPayoutValidatorHash
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward createTx point1 point1
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block1 $ FromCreate CreateStep{..} []
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  -- Should be empty because we already read them in the Do above and it
  -- resets to empty each time it is read.
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCreationWithClose :: Expectation
checkCreationWithClose = do
  let datum = createDatum
  let scriptAddress = testScriptAddress
  let payoutValidatorHash = testPayoutValidatorHash
  FollowerTestResult{..} <- runFollowerTest marloweVersions
    $ ConfirmHandshake
    $ ExpectQuery (FindTx createTxId)
    $ RollForward (createTx { Chain.validityRange = Chain.MinMaxBound 0 100, Chain.inputs = Set.singleton closeTxIn }) point1 point1
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block1 $ FromCreate CreateStep{..} []
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkFollowScriptUTxOFailed :: Expectation
checkFollowScriptUTxOFailed = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RejectQuery (Map.singleton createUTxO UTxONotFound) point1
    $ ExpectDone ()
  followerError `shouldBe` Just (FollowScriptUTxOFailed UTxONotFound)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkTxInNotFound :: Expectation
checkTxInNotFound = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ closeTx { Chain.inputs = mempty }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed TxInNotFound)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkNoRedeemer :: Expectation
checkNoRedeemer = do
  let badInput = closeTxIn { Chain.redeemer = Nothing }
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ closeTx { Chain.inputs = Set.singleton badInput }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed NoRedeemer)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkInvalidRedeemer :: Expectation
checkInvalidRedeemer = do
  let badInput = closeTxIn { Chain.redeemer = Just $ Chain.toRedeemer () }
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ closeTx { Chain.inputs = Set.singleton badInput }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed InvalidRedeemer)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkInvalidValidityRangeUnbounded :: Expectation
checkInvalidValidityRangeUnbounded = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ closeTx { Chain.validityRange = Chain.Unbounded }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed InvalidValidityRange)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkInvalidValidityRangeMinBound :: Expectation
checkInvalidValidityRangeMinBound = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ closeTx { Chain.validityRange = Chain.MinBound 0 }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed InvalidValidityRange)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkInvalidValidityRangeMaxBound :: Expectation
checkInvalidValidityRangeMaxBound = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ closeTx { Chain.validityRange = Chain.MaxBound 0 }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed InvalidValidityRange)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkNoTransactionDatum :: Expectation
checkNoTransactionDatum = do
  let badOutput = applyInputsOutput { Chain.datum = Nothing }
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ applyInputsTx { Chain.outputs = [badOutput] }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed NoTransactionDatum)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkInvalidTransactionDatum :: Expectation
checkInvalidTransactionDatum = do
  let badOutput = applyInputsOutput { Chain.datum = Just $ Chain.toDatum () }
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ applyInputsTx { Chain.outputs = [badOutput] }) point2 point2
    $ ExpectDone ()
  followerError `shouldBe` Just (ExtractMarloweTransactionFailed InvalidTransactionDatum)
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCreateTxRolledBackGenesisNoInputs :: Expectation
checkCreateTxRolledBackGenesisNoInputs = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollBackward Genesis Genesis
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just Genesis
            }
        )
    $ ExpectDone ()
  followerError `shouldBe` Just CreateTxRolledBack
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCreateTxRolledBackGenesisWithInputs :: Expectation
checkCreateTxRolledBackGenesisWithInputs = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsTx) point2 point2
    $ ExpectQuery (FindConsumingTxs $ Set.singleton applyInputsUTxO)
    $ RollBackward Genesis Genesis
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just Genesis
            }
        )
    $ ExpectDone ()
  followerError `shouldBe` Just CreateTxRolledBack
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCreateTxRolledBackGenesisClosed :: Expectation
checkCreateTxRolledBackGenesisClosed = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO closeTx) point2 point2
    $ ExpectQuery (AdvanceBlocks 2160)
    $ RollBackward Genesis Genesis
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just Genesis
            }
        )
    $ ExpectDone ()
  followerError `shouldBe` Just CreateTxRolledBack
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCreateTxRolledBackNoInputs :: Expectation
checkCreateTxRolledBackNoInputs = do
  FollowerTestResult{..} <- runFollowerTestPostCreationFrom point2 point2 marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollBackward point1 point1
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just $ At 0
            }
        )
    $ ExpectDone ()
  followerError `shouldBe` Just CreateTxRolledBack
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCreateTxRolledBackWithInputs :: Expectation
checkCreateTxRolledBackWithInputs = do
  FollowerTestResult{..} <- runFollowerTestPostCreationFrom point2 point2 marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsTx) point3 point3
    $ ExpectQuery (FindConsumingTxs $ Set.singleton applyInputsUTxO)
    $ RollBackward point1 point1
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just $ At 0
            }
        )
    $ ExpectDone ()
  followerError `shouldBe` Just CreateTxRolledBack
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCreateTxRolledBackClosed :: Expectation
checkCreateTxRolledBackClosed = do
  FollowerTestResult{..} <- runFollowerTestPostCreationFrom point2 point2 marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO closeTx) point3 point3
    $ ExpectQuery (AdvanceBlocks 2160)
    $ RollBackward point1 point1
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just $ At 0
            }
        )
    $ ExpectDone ()
  followerError `shouldBe` Just CreateTxRolledBack
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkRollbackToCreationWithInputs :: Expectation
checkRollbackToCreationWithInputs = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsTx) point2 point2
    $ ExpectQuery (FindConsumingTxs $ Set.singleton applyInputsUTxO)
    $ RollBackward point1 point1
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just $ At 0
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsTx) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = applyInputsTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = applyInputsRedeemer
                    , output = TransactionOutput mempty $ Just TransactionScriptOutput
                        { utxo = Chain.TxOutRef applyInputsTxId 0
                        , datum = createDatum
                        }
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkRollbackToCreationClosed :: Expectation
checkRollbackToCreationClosed = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO closeTx) point2 point2
    $ ExpectQuery (AdvanceBlocks 2160)
    $ RollBackward point1 point1
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.empty
            , rollbackTo = Just $ At 0
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO closeTx) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = closeTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = closeRedeemer
                    , output = TransactionOutput mempty Nothing
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkRollbackToTransaction :: Expectation
checkRollbackToTransaction = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsTx) point2 point2
    $ ExpectQuery (FindConsumingTxs $ Set.singleton applyInputsUTxO)
    $ RollForward (Map.singleton applyInputsUTxO close2Tx) point3 point3
    $ ExpectQuery (AdvanceBlocks 2160)
    $ RollBackward point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = applyInputsTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = applyInputsRedeemer
                    , output = TransactionOutput mempty $ Just TransactionScriptOutput
                        { utxo = Chain.TxOutRef applyInputsTxId 0
                        , datum = createDatum
                        }
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.singleton applyInputsUTxO)
    $ RollForward (Map.singleton applyInputsUTxO close2Tx) point3 point3
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block3 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = close2TxId
                    , contractId = testContractId
                    , blockHeader = block3
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = closeRedeemer
                    , output = TransactionOutput mempty Nothing
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCloseTransaction :: Expectation
checkCloseTransaction = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO closeTx) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = closeTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = closeRedeemer
                    , output = TransactionOutput mempty Nothing
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkNonCloseTransaction :: Expectation
checkNonCloseTransaction = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsTx) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = applyInputsTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = applyInputsRedeemer
                    , output = TransactionOutput mempty $ Just TransactionScriptOutput
                        { utxo = Chain.TxOutRef applyInputsTxId 0
                        , datum = createDatum
                        }
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkCloseAndCreateInSameTransaction :: Expectation
checkCloseAndCreateInSameTransaction = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO $ closeTx { Chain.outputs = [createOutput] }) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = closeTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = closeRedeemer
                    , output = TransactionOutput mempty Nothing
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkPayoutOpenRedeemedBefore :: Expectation
checkPayoutOpenRedeemedBefore = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsPayoutTx) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = applyInputsTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = applyInputsRedeemer
                    , output = TransactionOutput (Map.singleton payoutUTxO payout) $ Just TransactionScriptOutput
                        { utxo = Chain.TxOutRef applyInputsTxId 0
                        , datum = createDatum
                        }
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.fromList [applyInputsUTxO, payoutUTxO])
    $ RollForward (Map.singleton payoutUTxO redeemPayoutTx) point3 point3
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block3 $ FromStep
                [ RedeemPayout RedeemStep
                    { utxo = payoutUTxO
                    , redeemingTx = redeemPayoutTxId
                    , datum = Chain.TokenName "test_role"
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.singleton applyInputsUTxO)
    $ RollForward (Map.singleton applyInputsUTxO close2Tx) point4 point4
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block4 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = close2TxId
                    , contractId = testContractId
                    , blockHeader = block4
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = closeRedeemer
                    , output = TransactionOutput mempty Nothing
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (AdvanceBlocks 2160)
    $ RollBackward point2 point2
    $ ExpectQuery (FindConsumingTxs $ Set.fromList [applyInputsUTxO, payoutUTxO])
    $ RollForward (Map.singleton payoutUTxO redeemPayoutTx) point3 point3
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block3 $ FromStep
                [ RedeemPayout RedeemStep
                    { utxo = payoutUTxO
                    , redeemingTx = redeemPayoutTxId
                    , datum = Chain.TokenName "test_role"
                    }
                ]
            , rollbackTo = Just $ At 1
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkPayoutOpenRedeemedAfter :: Expectation
checkPayoutOpenRedeemedAfter = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsPayoutTx) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = applyInputsTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = applyInputsRedeemer
                    , output = TransactionOutput (Map.singleton payoutUTxO payout) $ Just TransactionScriptOutput
                        { utxo = Chain.TxOutRef applyInputsTxId 0
                        , datum = createDatum
                        }
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.fromList [applyInputsUTxO, payoutUTxO])
    $ RollForward (Map.singleton applyInputsUTxO close2Tx) point3 point3
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block3 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = close2TxId
                    , contractId = testContractId
                    , blockHeader = block3
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = closeRedeemer
                    , output = TransactionOutput mempty Nothing
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.singleton payoutUTxO)
    $ RollForward (Map.singleton payoutUTxO redeemPayoutTx) point4 point4
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block4 $ FromStep
                [ RedeemPayout RedeemStep
                    { utxo = payoutUTxO
                    , redeemingTx = redeemPayoutTxId
                    , datum = Chain.TokenName "test_role"
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ Halt ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

checkPayoutOpenRedeemedTogether :: Expectation
checkPayoutOpenRedeemedTogether = do
  FollowerTestResult{..} <- runFollowerTestPostCreation marloweVersions
    $ ExpectQuery (FindConsumingTxs $ Set.singleton createUTxO)
    $ RollForward (Map.singleton createUTxO applyInputsPayoutTx) point2 point2
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block2 $ FromStep
                [ ApplyTransaction Transaction
                    { transactionId = let Chain.Transaction{..} = applyInputsTx in txId
                    , contractId = testContractId
                    , blockHeader = block2
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = applyInputsRedeemer
                    , output = TransactionOutput (Map.singleton payoutUTxO payout) $ Just TransactionScriptOutput
                        { utxo = Chain.TxOutRef applyInputsTxId 0
                        , datum = createDatum
                        }
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (FindConsumingTxs $ Set.fromList [applyInputsUTxO, payoutUTxO])
    $ RollForward (Map.fromList [(applyInputsUTxO, close2Tx), (payoutUTxO, redeemPayoutTx)]) point3 point3
    $ Do
        ( expectChanges MarloweV1 ContractChanges
            { steps = Map.singleton block3 $ FromStep
                [ RedeemPayout RedeemStep
                    { utxo = payoutUTxO
                    , redeemingTx = redeemPayoutTxId
                    , datum = Chain.TokenName "test_role"
                    }
                , ApplyTransaction Transaction
                    { transactionId = close2TxId
                    , contractId = testContractId
                    , blockHeader = block3
                    , validityLowerBound = posixSecondsToUTCTime 0
                    , validityUpperBound = posixSecondsToUTCTime 100
                    , redeemer = closeRedeemer
                    , output = TransactionOutput mempty Nothing
                    }
                ]
            , rollbackTo = Nothing
            }
        )
    $ ExpectQuery (AdvanceBlocks 2160)
    $ RollForward () point4 point4
    $ ExpectDone ()
  followerError `shouldBe` Nothing
  followerChanges `shouldBe` Just (emptyChanges MarloweV1)

emptyChanges :: MarloweVersion v -> SomeContractChanges
emptyChanges version = SomeContractChanges version mempty

expectChanges :: MarloweVersion v -> ContractChanges v -> ReaderT Follower IO ()
expectChanges version expectedChanges = do
  Follower{..} <- ask
  liftIO do
    currentChanges <- atomically changes
    currentChanges `shouldBe` Just (SomeContractChanges version expectedChanges)

readChanges :: ReaderT Follower IO ()
readChanges = do
  Follower{..} <- ask
  void $ liftIO $ atomically changes

-- TODO move to a test module in marlowe-protocols and generalize Move -> query
data FollowerTestResult a = FollowerTestResult
  { followerError   :: Maybe ContractHistoryError
  , followerChanges :: Maybe SomeContractChanges
  , testResult      :: a
  }

runFollowerTestPostCreationFrom
  :: ChainPoint
  -> ChainPoint
  -> [(ScriptHash, (SomeMarloweVersion, ScriptHash))]
  -> ServerStIdleScript Move ChainPoint ChainPoint (ReaderT Follower IO) a
  -> IO (FollowerTestResult a)
runFollowerTestPostCreationFrom point tip marloweVersions' = runFollowerTest marloweVersions'
  . ConfirmHandshake
  . ExpectQuery (FindTx createTxId)
  . RollForward createTx point tip
  . Do readChanges -- to empty them

runFollowerTestPostCreation
  :: [(ScriptHash, (SomeMarloweVersion, ScriptHash))]
  -> ServerStIdleScript Move ChainPoint ChainPoint (ReaderT Follower IO) a
  -> IO (FollowerTestResult a)
runFollowerTestPostCreation = runFollowerTestPostCreationFrom point1 point2

runFollowerTest
  :: [(ScriptHash, (SomeMarloweVersion, ScriptHash))]
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
    let slotZeroTime = posixSecondsToUTCTime 0
    let slotLength = secondsToNominalDiffTime 1
    let slotConfig = SlotConfig{..}
    let securityParameter = 2160
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
