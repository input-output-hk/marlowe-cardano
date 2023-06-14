module Language.Marlowe.Runtime.Web.Contracts.Transactions.GetTransactions where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Reader as Reader

import Control.Exception (throw)
import Data.Proxy (Proxy(Proxy))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common (Wallet, getGenesisWallet, runIntegrationTest, runWebClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (Page(..), getTransactions)
import Language.Marlowe.Runtime.Web.Common (applyCloseTransaction, createCloseContract)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.StandardContract (createFullyExecutedStandardContract)
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Servant.Pagination (Range(..), RangeOrder(..))
import Test.Hspec (ActionWith, Spec, SpecWith, aroundAll, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (MarloweRuntime, withLocalMarloweRuntime)

spec :: Spec
spec = describe "GET /contracts/{contractId}/transactions" $ aroundAll setup do
  getTransactionsValidSpec
  getTransactionsInvalidSpec
  getTransactionsValidNextPageSpec

getTransactionsValidSpec :: SpecWith MarloweWebTestData
getTransactionsValidSpec = describe "Valid GET /contracts/{contractId}/transactions" do
  noTransactionsValidSpec
  singleTransactionValidSpec
  multipleContractsSingleTransactionsValidSpec
  singleContractMultipleTransactionsValidSpec
  multipleContractsMultipleTransactionsValidSpec

getTransactionsInvalidSpec :: SpecWith MarloweWebTestData
getTransactionsInvalidSpec = describe "Invalid GET /contracts/{contractId}/transactions" do
  invalidContractIdSpec
  invalidTxIdSpec

getTransactionsValidNextPageSpec :: SpecWith MarloweWebTestData
getTransactionsValidNextPageSpec = describe "Valid Pagination of GET /contracts/{contractId}/transactions" do
  firstPageAscValidSpec
  secondPageAscValidSpec
  firstPageDescValidSpec
  secondPageDescValidSpec

firstPageAscValidSpec :: SpecWith MarloweWebTestData
firstPageAscValidSpec = it "returns the first page of transactions in ascending order" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
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
        Page {..} <- getTransactions expectedContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId1, txId2]

      _ -> do
        liftIO $ fail "Expected at least two transactions"

secondPageAscValidSpec :: SpecWith MarloweWebTestData
secondPageAscValidSpec = it "returns the second page of transactions in ascending order" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
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
        Page {..} <- getTransactions expectedContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId3, txId4]

      _ -> do
        liftIO $ fail "Expected at least two transactions"

firstPageDescValidSpec :: SpecWith MarloweWebTestData
firstPageDescValidSpec = it "returns the first page of transactions in descending order" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
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
        Page {..} <- getTransactions expectedContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId4, txId3]

      _ -> do
        liftIO $ fail "Expected at least two transactions"


secondPageDescValidSpec :: SpecWith MarloweWebTestData
secondPageDescValidSpec = it "returns the second page of transactions in descending order" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
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
        Page {..} <- getTransactions expectedContractId (Just testRange)

        liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [txId2, txId1]

      _ -> do
        liftIO $ fail "Expected at least two transactions"


noTransactionsValidSpec :: SpecWith MarloweWebTestData
noTransactionsValidSpec = it "returns an empty list when no transactions exist" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    sampleContractId <- createCloseContract wallet1
    Page {..}<- getTransactions sampleContractId Nothing
    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` []

singleTransactionValidSpec :: SpecWith MarloweWebTestData
singleTransactionValidSpec  = it "returns a list with single transaction when there is a single contract on chain" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    sampleContractId <- createCloseContract wallet1
    expectedTxId <- applyCloseTransaction wallet1 sampleContractId
    Page {..}<- getTransactions sampleContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [expectedTxId]

multipleContractsSingleTransactionsValidSpec :: SpecWith MarloweWebTestData
multipleContractsSingleTransactionsValidSpec  =  it "returns a list with single transaction when there are multiple contracts on chain" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    expectedContractId2 <- createCloseContract wallet2
    expectedTxId <- applyCloseTransaction wallet2 expectedContractId2
    Page {..} <- getTransactions expectedContractId2 Nothing
    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [expectedTxId]

singleContractMultipleTransactionsValidSpec :: SpecWith MarloweWebTestData
singleContractMultipleTransactionsValidSpec  =  it "returns a list with multiple transaction when a single contract is on chain" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    Page {..} <- getTransactions expectedContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` reverse expectedTransactionIds

multipleContractsMultipleTransactionsValidSpec :: SpecWith MarloweWebTestData
multipleContractsMultipleTransactionsValidSpec  =  it "returns a list with multiple transaction when multiple contracts are on chain" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    (createContractId, testTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2

    Page {..} <- getTransactions createContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` reverse testTransactionIds

invalidContractIdSpec :: SpecWith MarloweWebTestData
invalidContractIdSpec = it "responds with a 404 when the contractId cannot be found" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
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

invalidTxIdSpec :: SpecWith MarloweWebTestData
invalidTxIdSpec = it "responds with a 416 when the transactionId cannot be found" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  result <- runWebClient do
    let
      invalidRange =  Range
        {
          rangeValue = Just $ toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000"
        , rangeOffset = 0
        , rangeLimit = 1
        , rangeOrder = RangeAsc
        , rangeField = Proxy
        }
    getTransactions expectedContractId $ Just invalidRange


  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 416 } } ) ->  pure ()
    _ -> fail $ "Expected 416 response code - got " <> show result

setup :: ActionWith MarloweWebTestData -> IO ()
setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- Reader.ask
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  either throw pure =<< runWebClient do
    (expectedContractId, expectedTransactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    liftIO $ runSpec MarloweWebTestData{..}

data MarloweWebTestData = MarloweWebTestData
  { runtime :: MarloweRuntime
  , expectedContractId :: Web.TxOutRef
  , expectedTransactionIds :: [Web.TxId]
  , wallet1 :: Wallet
  , wallet2 :: Wallet
  }

