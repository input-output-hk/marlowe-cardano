{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Marlowe.Runtime.Web.GetTransactions
  where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import Data.Proxy (Proxy(Proxy))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Web (ApplyInputsTxBody(ApplyInputsTxBody))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (Page(..), getContracts, getTransactions)
import Language.Marlowe.Runtime.Web.Common (applyCloseTransaction, createCloseContract)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.StandardContract
  ( StandardContractChoiceMade(..)
  , StandardContractClosed(..)
  , StandardContractFundsDeposited(..)
  , StandardContractInit(..)
  , StandardContractNotified(..)
  , createStandardContract
  )
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Servant.Pagination (Range(..), RangeOrder(..))
import Test.Hspec (Spec, describe, focus, it, shouldBe, shouldContain)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = focus $ describe "GET /contracts/{contractId}/transactions" do
  getTransactionsValidSpec
  -- getTransactionsInvalidSpec
  -- getTransactionsValidNextPageSpec

getTransactionsValidSpec :: Spec
getTransactionsValidSpec = describe "Valid GET /contracts/{contractId}/transactions" do
  noTransactionsValidSpec
  singleTransactionValidSpec
  multipleContractsSingleTransactionsValidSpec
  singleContractsMultipleTransactionsValidSpec

-- getTransactionsInvalidSpec :: Spec
-- getTransactionsInvalidSpec = describe "Invalid GET /contracts/{contractId}/transactions" do
  -- invalidTxIdSpec

-- getTransactionsValidNextPageSpec :: Spec
-- getTransactionsValidNextPageSpec = describe "Valid Pagination of GET /contracts/{contractId}/transactions" do
--   firstPageAscValidSpec
--   secondPageAscValidSpec
--   firstPageDescValidSpec
--   secondPageDescValidSpec

-- firstPageAscValidSpec :: Spec
-- firstPageAscValidSpec = it "returns the first page of contract headers in ascending order" $ withLocalMarloweRuntime $ runIntegrationTest do
--   wallet1 <- getGenesisWallet 0
--   wallet2 <- getGenesisWallet 1
--   wallet3 <- getGenesisWallet 2

--   either throw pure =<< runWebClient do
--     expectedContractId1 <- createCloseContract wallet1
--     expectedContractId2 <- createCloseContract wallet2
--     _ <- createCloseContract wallet3
--     let
--       testRange =  Range
--         {
--           rangeValue = Just expectedContractId1
--         , rangeOffset = 0
--         , rangeLimit = 2
--         , rangeOrder = RangeAsc
--         , rangeField = Proxy
--         }
--     Page {..} <-getContracts $ Just testRange

--     liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId1, expectedContractId2]

-- secondPageAscValidSpec :: Spec
-- secondPageAscValidSpec = it "returns the second page of contract headers in ascending order" $ withLocalMarloweRuntime $ runIntegrationTest do
--   wallet1 <- getGenesisWallet 0
--   wallet2 <- getGenesisWallet 1
--   wallet3 <- getGenesisWallet 2

--   either throw pure =<< runWebClient do
--     expectedContractId1 <- createCloseContract wallet1
--     expectedContractId2 <- createCloseContract wallet2
--     expectedContractId3 <- createCloseContract wallet3
--     let
--       testRange =  Range
--         {
--           rangeValue = Just expectedContractId1
--         , rangeOffset = 1
--         , rangeLimit = 2
--         , rangeOrder = RangeAsc
--         , rangeField = Proxy
--         }
--     Page {..} <-getContracts $ Just testRange

--     liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId2, expectedContractId3]

-- firstPageDescValidSpec :: Spec
-- firstPageDescValidSpec = it "returns the first page of contract headers in descending order" $ withLocalMarloweRuntime $ runIntegrationTest do
--   wallet1 <- getGenesisWallet 0
--   wallet2 <- getGenesisWallet 1
--   wallet3 <- getGenesisWallet 2

--   either throw pure =<< runWebClient do
--     _ <- createCloseContract wallet1
--     expectedContractId2 <- createCloseContract wallet2
--     expectedContractId3 <- createCloseContract wallet3
--     let
--       testRange =  Range
--         {
--           rangeValue = Just expectedContractId3
--         , rangeOffset = 0
--         , rangeLimit = 2
--         , rangeOrder = RangeDesc
--         , rangeField = Proxy
--         }
--     Page {..} <-getContracts $ Just testRange

--     liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId3, expectedContractId2]

-- secondPageDescValidSpec :: Spec
-- secondPageDescValidSpec = it "returns the second page of contract headers in descending order" $ withLocalMarloweRuntime $ runIntegrationTest do
--   wallet1 <- getGenesisWallet 0
--   wallet2 <- getGenesisWallet 1
--   wallet3 <- getGenesisWallet 2

--   either throw pure =<< runWebClient do
--     expectedContractId1 <- createCloseContract wallet1
--     expectedContractId2 <- createCloseContract wallet2
--     expectedContractId3 <- createCloseContract wallet3
--     let
--       testRange =  Range
--         {
--           rangeValue = Just expectedContractId3
--         , rangeOffset = 1
--         , rangeLimit = 2
--         , rangeOrder = RangeDesc
--         , rangeField = Proxy
--         }
--     Page {..} <-getContracts $ Just testRange

--     liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId2, expectedContractId1]


noTransactionsValidSpec :: Spec
noTransactionsValidSpec = it "returns an empty list when no transactions exist" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet <- getGenesisWallet 0

  either throw pure =<< runWebClient do
    sampleContractId <- createCloseContract wallet
    Page {..}<- getTransactions sampleContractId Nothing
    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` []

singleTransactionValidSpec :: Spec
singleTransactionValidSpec  = it "returns a list with single Tx header" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet <- getGenesisWallet 0

  either throw pure =<< runWebClient do
    sampleContractId <- createCloseContract wallet
    expectedTxId <- applyCloseTransaction wallet sampleContractId
    Page {..}<- getTransactions sampleContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [expectedTxId]

multipleContractsSingleTransactionsValidSpec :: Spec
multipleContractsSingleTransactionsValidSpec  =  it "returns a list with single Tx header" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    _ <- applyCloseTransaction wallet1 expectedContractId1
    expectedContractId2 <- createCloseContract wallet2
    expectedTxId <- applyCloseTransaction wallet2 expectedContractId2
    Page {..} <- getTransactions expectedContractId2 Nothing
    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [expectedTxId]

singleContractsMultipleTransactionsValidSpec :: Spec
singleContractsMultipleTransactionsValidSpec  =  focus $ it "returns a list with multiple Tx headers" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    StandardContractInit{contractCreated, makeInitialDeposit} <- createStandardContract wallet1 wallet2
    StandardContractFundsDeposited{initialFundsDeposited, chooseGimmeTheMoney} <- makeInitialDeposit
    StandardContractChoiceMade{gimmeTheMoneyChosen, sendNotify} <- chooseGimmeTheMoney
    StandardContractNotified{notified, makeReturnDeposit} <- sendNotify
    StandardContractClosed{returnDeposited, withdrawPartyAFunds} <- makeReturnDeposit
    (_, _) <- withdrawPartyAFunds
    createContractId <- case contractCreated of
      Web.CreateTxBody{contractId} -> pure contractId
    transactionId1 <- case initialFundsDeposited of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId
    transactionId2 <- case gimmeTheMoneyChosen of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId
    transactionId3 <- case notified of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId
    transactionId4 <- case returnDeposited of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId

    Page {..} <- getTransactions createContractId Nothing

    liftIO $ fmap (\Web.TxHeader{..} -> transactionId) items `shouldBe` [transactionId4,  transactionId3, transactionId2, transactionId1]


-- invalidTxIdSpec :: Spec
-- invalidTxIdSpec = it "returns an error message" $ withLocalMarloweRuntime $ runIntegrationTest do
--   result <- runWebClient do
--     let
--       invalidRange =  Range
--         {
--           rangeValue = Just $ Web.TxOutRef (toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000") 1
--         , rangeOffset = 0
--         , rangeLimit = 1
--         , rangeOrder = RangeAsc
--         , rangeField = Proxy
--         }
--     getContracts $ Just invalidRange

--   case result of
--     Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 416 } } ) ->  pure ()
--     _ -> fail $ "Expected 416 response code - got " <> show result


