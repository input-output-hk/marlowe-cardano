module Language.Marlowe.Runtime.Web.GetTransactions
  where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import Data.Proxy (Proxy(Proxy))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (Page(..), getTransactions)
import Language.Marlowe.Runtime.Web.Common (applyCloseTransaction, createCloseContract)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.StandardContract (createFullyExecutedStandardContract)
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Servant.Pagination (Range(..), RangeOrder(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "GET /contracts/{contractId}/transactions" do
  getTransactionsValidSpec
  getTransactionsInvalidSpec
  getTransactionsValidNextPageSpec

getTransactionsValidSpec :: Spec
getTransactionsValidSpec = describe "Valid GET /contracts/{contractId}/transactions" do
  noTransactionsValidSpec
  singleTransactionValidSpec
  multipleContractsSingleTransactionsValidSpec
  singleContractMultipleTransactionsValidSpec
  multipleContractsMultipleTransactionsValidSpec

getTransactionsInvalidSpec :: Spec
getTransactionsInvalidSpec = describe "Invalid GET /contracts/{contractId}/transactions" do
  invalidContractIdSpec
  invalidTxIdSpec

getTransactionsValidNextPageSpec :: Spec
getTransactionsValidNextPageSpec = describe "Valid Pagination of GET /contracts/{contractId}/transactions" do
  firstPageAscValidSpec
  secondPageAscValidSpec
  firstPageDescValidSpec
  secondPageDescValidSpec

firstPageAscValidSpec :: Spec
firstPageAscValidSpec = it "returns the first page of transactions in ascending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (sampleContractId, expectedTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case expectedTransactionIds of
      (txId1:txId2:_) -> do
        let
          testRange = Range
            {
              rangeValue = Nothing
            , rangeOffset = 0
            , rangeLimit = 2
            , rangeOrder = RangeAsc
            , rangeField = Proxy
            }
        Page {..} <- getTransactions sampleContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId1, txId2]

      _ -> do
        liftIO $ fail "Expected at least two transactions"

secondPageAscValidSpec :: Spec
secondPageAscValidSpec = it "returns the second page of transactions in ascending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (sampleContractId, expectedTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case reverse expectedTransactionIds of
      (txId4:txId3:_) -> do
        let
          testRange = Range
            {
              rangeValue = Nothing
            , rangeOffset = 2
            , rangeLimit = 2
            , rangeOrder = RangeAsc
            , rangeField = Proxy
            }
        Page {..} <- getTransactions sampleContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId3, txId4]

      _ -> do
        liftIO $ fail "Expected at least two transactions"

firstPageDescValidSpec :: Spec
firstPageDescValidSpec = it "returns the first page of transactions in descending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (sampleContractId, expectedTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case reverse expectedTransactionIds of
      (txId4:txId3:_) -> do
        let
          testRange = Range
            {
              rangeValue = Nothing
            , rangeOffset = 0
            , rangeLimit = 2
            , rangeOrder = RangeDesc
            , rangeField = Proxy
            }
        Page {..} <- getTransactions sampleContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId4, txId3]

      _ -> do
        liftIO $ fail "Expected at least two transactions"


secondPageDescValidSpec :: Spec
secondPageDescValidSpec = it "returns the second page of transactions in descending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (sampleContractId, expectedTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case expectedTransactionIds of
      (txId1:txId2:_) -> do
        let
          testRange = Range
            {
              rangeValue = Nothing
            , rangeOffset = 2
            , rangeLimit = 2
            , rangeOrder = RangeDesc
            , rangeField = Proxy
            }
        Page {..} <- getTransactions sampleContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId2, txId1]

      _ -> do
        liftIO $ fail "Expected at least two transactions"



noTransactionsValidSpec :: Spec
noTransactionsValidSpec = it "returns an empty list when no transactions exist" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet <- getGenesisWallet 0

  either throw pure =<< runWebClient do
    sampleContractId <- createCloseContract wallet
    Page {..}<- getTransactions sampleContractId Nothing
    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` []

singleTransactionValidSpec :: Spec
singleTransactionValidSpec  = it "returns a list with single transaction when there is a single contract on chain" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet <- getGenesisWallet 0

  either throw pure =<< runWebClient do
    sampleContractId <- createCloseContract wallet
    expectedTxId <- applyCloseTransaction wallet sampleContractId
    Page {..}<- getTransactions sampleContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [expectedTxId]

multipleContractsSingleTransactionsValidSpec :: Spec
multipleContractsSingleTransactionsValidSpec  =  it "returns a list with single transaction when there are multiple contracts on chain" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    _ <- applyCloseTransaction wallet1 expectedContractId1
    expectedContractId2 <- createCloseContract wallet2
    expectedTxId <- applyCloseTransaction wallet2 expectedContractId2
    Page {..} <- getTransactions expectedContractId2 Nothing
    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [expectedTxId]

singleContractMultipleTransactionsValidSpec :: Spec
singleContractMultipleTransactionsValidSpec  =  it "returns a list with multiple transaction when a single contract is on chain" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (createContractId, expectedTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2

    Page {..} <- getTransactions createContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` reverse expectedTransactionIds

multipleContractsMultipleTransactionsValidSpec :: Spec
multipleContractsMultipleTransactionsValidSpec  =  it "returns a list with multiple transaction when multiple contracts are on chain" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    _ <- createFullyExecutedStandardContract wallet1 wallet2
    (createContractId, expectedTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2

    Page {..} <- getTransactions createContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` reverse expectedTransactionIds

invalidContractIdSpec :: Spec
invalidContractIdSpec = it "responds with a 404 when the contractId cannot be found" $ withLocalMarloweRuntime $ runIntegrationTest do
  result <- runWebClient do
    let
      invalidContractId = Web.TxOutRef (toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000") 1
    let
      invalidRange =  Range
        {
          rangeValue = Just $ toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000"
        , rangeOffset = 0
        , rangeLimit = 1
        , rangeOrder = RangeAsc
        , rangeField = Proxy
        }
    getTransactions invalidContractId $ Just invalidRange


  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } } ) ->  pure ()
    _ -> fail $ "Expected 404 response code - got " <> show result

invalidTxIdSpec :: Spec
invalidTxIdSpec = it "responds with a 416 when the transactionId cannot be found" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  result <- runWebClient do
    (validContractId, _) <- createFullyExecutedStandardContract wallet1 wallet2
    let
      invalidRange =  Range
        {
          rangeValue = Just $ toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000"
        , rangeOffset = 0
        , rangeLimit = 1
        , rangeOrder = RangeAsc
        , rangeField = Proxy
        }
    getTransactions validContractId $ Just invalidRange


  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 416 } } ) ->  pure ()
    _ -> fail $ "Expected 416 response code - got " <> show result
