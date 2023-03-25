{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Marlowe.Runtime.Web.GetTransaction
  where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common (getGenesisWallet, runIntegrationTest, runWebClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (getTransaction)
import Language.Marlowe.Runtime.Web.Common (createCloseContract, waitUntilConfirmed)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.StandardContract (createFullyExecutedStandardContract)
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "GET /contract/{contractId}/transactions/{transactionId}" do
  getTransactionValidSpec
  -- getTransactionInvalidSpec

getTransactionValidSpec :: Spec
getTransactionValidSpec = describe "Valid GET /contract/{contractId}/transactions/{transactionId}" do
  getsFirstTransactionValidSpec
  getsSecondTransactionValidSpec
  getsThirdTransactionValidSpec

-- getTransactionInvalidSpec :: Spec
-- getTransactionInvalidSpec = describe "Invalid GET /contract/{contractId}/transactions/{transactionId}" do
  -- invalidTxIdSpec

getsFirstTransactionValidSpec :: Spec
getsFirstTransactionValidSpec = it "returns the first transaction" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (contractId, transactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case transactionIds of
      (txId1:_) -> do
        Web.Tx{transactionId = actualTransactionId } <- getTransaction contractId txId1

        liftIO $ actualTransactionId `shouldBe` txId1

      _ -> do
        liftIO $ fail "Expected at least two transactions"

getsSecondTransactionValidSpec :: Spec
getsSecondTransactionValidSpec = it "returns the second transaction" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (contractId, transactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case transactionIds of
      (_:txId2:_) -> do
        Web.Tx{transactionId = actualTransactionId } <- getTransaction contractId txId2

        liftIO $ actualTransactionId `shouldBe` txId2

      _ -> do
        liftIO $ fail "Expected at least two transactions"


getsThirdTransactionValidSpec :: Spec
getsThirdTransactionValidSpec = it "returns the third transaction" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    (contractId, transactionIds) <- createFullyExecutedStandardContract wallet1 wallet2
    case transactionIds of
      (_:_:txId3:_) -> do
        Web.Tx{transactionId = actualTransactionId } <- getTransaction contractId txId3

        liftIO $ actualTransactionId `shouldBe` txId3

      _ -> do
        liftIO $ fail "Expected at least three transactions"


-- invalidTxIdSpec :: Spec
-- invalidTxIdSpec = it "returns not found for invalid transaction id" $ withLocalMarloweRuntime $ runIntegrationTest do
--   result <- runWebClient do
--     let
--       invalidTxId = Web.TxOutRef (toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000") 1
--     getTransaction invalidTxId

--   case result of
--     Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } } ) ->  pure ()
--     _ -> fail $ "Expected 404 response code - got " <> show result
