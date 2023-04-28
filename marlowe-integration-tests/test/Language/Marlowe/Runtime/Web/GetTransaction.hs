{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Marlowe.Runtime.Web.GetTransaction
  where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common (getGenesisWallet, runIntegrationTest, runWebClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (getTransaction)
import Language.Marlowe.Runtime.Web.Common (MarloweWebTestData(..), createCloseContract, setup, waitUntilConfirmed)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.StandardContract (createFullyExecutedStandardContract)
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Test.Hspec (Spec, SpecWith, aroundAll, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "GET /contract/{contractId}/transactions/{transactionId}" $ aroundAll setup do
  getTransactionValidSpec
  getTransactionInvalidSpec

getTransactionValidSpec :: SpecWith MarloweWebTestData
getTransactionValidSpec = describe "Valid GET /contract/{contractId}/transactions/{transactionId}" do
  getsFirstTransactionValidSpec
  getsSecondTransactionValidSpec
  getsThirdTransactionValidSpec

getTransactionInvalidSpec :: SpecWith MarloweWebTestData
getTransactionInvalidSpec = describe "Invalid GET /contract/{contractId}/transactions/{transactionId}" do
  invalidTxIdSpec

getsFirstTransactionValidSpec :: SpecWith MarloweWebTestData
getsFirstTransactionValidSpec = it "returns the first transaction" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    (contractId, transactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case transactionIds of
      (txId1:_) -> do
        Web.Tx{transactionId = actualTransactionId } <- getTransaction contractId txId1

        liftIO $ actualTransactionId `shouldBe` txId1

      _ -> do
        liftIO $ fail "Expected at least two transactions"

getsSecondTransactionValidSpec :: SpecWith MarloweWebTestData
getsSecondTransactionValidSpec = it "returns the second transaction" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    (contractId, transactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case transactionIds of
      (_:txId2:_) -> do
        Web.Tx{transactionId = actualTransactionId } <- getTransaction contractId txId2

        liftIO $ actualTransactionId `shouldBe` txId2

      _ -> do
        liftIO $ fail "Expected at least two transactions"


getsThirdTransactionValidSpec :: SpecWith MarloweWebTestData
getsThirdTransactionValidSpec = it "returns the third transaction" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    (contractId, transactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case transactionIds of
      (_:_:txId3:_) -> do
        Web.Tx{transactionId = actualTransactionId } <- getTransaction contractId txId3

        liftIO $ actualTransactionId `shouldBe` txId3

      _ -> do
        liftIO $ fail "Expected at least three transactions"


invalidTxIdSpec :: SpecWith MarloweWebTestData
invalidTxIdSpec = it "returns not found for invalid transaction id" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  result <- runWebClient do
    (contractId, _) <- createFullyExecutedStandardContract wallet1 wallet2

    let
      invalidTxId = toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000"
    getTransaction contractId invalidTxId

  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } } ) ->  pure ()
    _ -> fail $ "Expected 404 response code - got " <> show result
