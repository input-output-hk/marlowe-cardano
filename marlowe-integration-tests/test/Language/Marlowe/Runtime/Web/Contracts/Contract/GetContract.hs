module Language.Marlowe.Runtime.Web.Contracts.Contract.GetContract where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Reader as Reader
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common (Wallet, getGenesisWallet, runIntegrationTest, runWebClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (getContract)
import Language.Marlowe.Runtime.Web.Common (createCloseContract, waitUntilConfirmed)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Test.Hspec (ActionWith, Spec, SpecWith, aroundAll, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (MarloweRuntime, withLocalMarloweRuntime)

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
    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId1

    liftIO $ contractId `shouldBe` expectedContractId1

getsSecondContractValidSpec :: SpecWith MarloweWebTestData
getsSecondContractValidSpec = it "returns the second contract" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do

  either throw pure =<< runWebClient do
    Web.ContractState{..}<- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract expectedContractId2

    liftIO $ contractId `shouldBe` expectedContractId2

getsThirdContractValidSpec :: SpecWith MarloweWebTestData
getsThirdContractValidSpec = it "returns the third contract" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
  either throw pure =<< runWebClient do
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

setup :: ActionWith MarloweWebTestData -> IO ()
setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- Reader.ask
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2
  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    expectedContractId3 <- createCloseContract wallet3
    liftIO $ runSpec MarloweWebTestData{..}

data MarloweWebTestData = MarloweWebTestData
  { runtime :: MarloweRuntime
  , wallet1 :: Wallet
  , wallet2 :: Wallet
  , wallet3 :: Wallet
  , expectedContractId1 :: Web.TxOutRef
  , expectedContractId2 :: Web.TxOutRef
  , expectedContractId3 :: Web.TxOutRef
  }
