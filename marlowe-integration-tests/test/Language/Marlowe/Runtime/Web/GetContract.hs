module Language.Marlowe.Runtime.Web.GetContract
  where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common (runIntegrationTest, runWebClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (getContract)
import Language.Marlowe.Runtime.Web.Common (MarloweWebTestData(..), createCloseContract, setup, waitUntilConfirmed)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Test.Hspec (Spec, SpecWith, aroundAll, describe, it, shouldBe)

spec :: Spec
spec = describe "GET /contract/{contractId}" $ aroundAll setup do
  getContractValidSpec
  getContractInvalidSpec

getContractValidSpec :: SpecWith MarloweWebTestData
getContractValidSpec = describe "Valid GET /contract" do
  getsFirstContractValidSpec
  getsSecondContractValidSpec
  getsThirdContractValidSpec

getContractInvalidSpec :: SpecWith MarloweWebTestData
getContractInvalidSpec = describe "Invalid GET /contract" do
  invalidTxIdSpec

getsFirstContractValidSpec :: SpecWith MarloweWebTestData
getsFirstContractValidSpec = it "returns the first contract" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    _ <- createCloseContract wallet2
    _ <- createCloseContract wallet3

    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId1

    liftIO $ contractId `shouldBe` expectedContractId1

getsSecondContractValidSpec :: SpecWith MarloweWebTestData
getsSecondContractValidSpec = it "returns the second contract" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do

  either throw pure =<< runWebClient do
    _ <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    _ <- createCloseContract wallet3

    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId2

    liftIO $ contractId `shouldBe` expectedContractId2

getsThirdContractValidSpec :: SpecWith MarloweWebTestData
getsThirdContractValidSpec = it "returns the third contract" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
    _ <- createCloseContract wallet1
    _ <- createCloseContract wallet2
    expectedContractId3 <- createCloseContract wallet3

    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId3

    liftIO $ contractId `shouldBe` expectedContractId3

invalidTxIdSpec :: SpecWith MarloweWebTestData
invalidTxIdSpec = it "returns not found for invalid contract id" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  result <- runWebClient do
    let
      invalidTxId = Web.TxOutRef (toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000") 1
    getContract invalidTxId

  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } } ) ->  pure ()
    _ -> fail $ "Expected 404 response code - got " <> show result
