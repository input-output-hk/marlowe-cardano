module Language.Marlowe.Runtime.Web.GetContract
  where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common (getGenesisWallet, runIntegrationTest, runWebClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (getContract)
import Language.Marlowe.Runtime.Web.Common (createCloseContract, waitUntilConfirmed)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "GET /contract/{contractId}" do
  getContractValidSpec
  getContractInvalidSpec

getContractValidSpec :: Spec
getContractValidSpec = describe "Valid GET /contract" do
  getsFirstContractValidSpec
  getsSecondContractValidSpec
  getsThirdContractValidSpec

getContractInvalidSpec :: Spec
getContractInvalidSpec = describe "Invalid GET /contract" do
  invalidTxIdSpec

getsFirstContractValidSpec :: Spec
getsFirstContractValidSpec = it "returns the first contract" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2

  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    _ <- createCloseContract wallet2
    _ <- createCloseContract wallet3

    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId1

    liftIO $ contractId `shouldBe` expectedContractId1

getsSecondContractValidSpec :: Spec
getsSecondContractValidSpec = it "returns the second contract" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2

  either throw pure =<< runWebClient do
    _ <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    _ <- createCloseContract wallet3

    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId2

    liftIO $ contractId `shouldBe` expectedContractId2

getsThirdContractValidSpec :: Spec
getsThirdContractValidSpec = it "returns the third contract" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2

  either throw pure =<< runWebClient do
    _ <- createCloseContract wallet1
    _ <- createCloseContract wallet2
    expectedContractId3 <- createCloseContract wallet3

    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId3

    liftIO $ contractId `shouldBe` expectedContractId3

invalidTxIdSpec :: Spec
invalidTxIdSpec = it "returns not found for invalid contract id" $ withLocalMarloweRuntime $ runIntegrationTest do
  result <- runWebClient do
    let
      invalidTxId = Web.TxOutRef (toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000") 1
    getContract invalidTxId

  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } } ) ->  pure ()
    _ -> fail $ "Expected 404 response code - got " <> show result
