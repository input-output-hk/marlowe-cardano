module Language.Marlowe.Runtime.Web.Contracts.Contract.Next.Get
  ( spec
  ) where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Reader as Reader
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common (Wallet, getGenesisWallet, runIntegrationTest, runWebClient)
import qualified Language.Marlowe.Runtime.Web.Types as Web

import qualified Data.Time as Time
import Language.Marlowe.Runtime.Web.Client (getContract, getContractNext)
import Language.Marlowe.Runtime.Web.Common (createCloseContract, waitUntilConfirmed)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Test.Hspec (ActionWith, Spec, SpecWith, aroundAll, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (MarloweRuntime, withLocalMarloweRuntime)

import Language.Marlowe.Core.V1.Semantics.Next (Next(..))
import Language.Marlowe.Core.V1.Semantics.Next.Applicables (emptyApplicables)
import Language.Marlowe.Core.V1.Semantics.Next.CanReduce (CanReduce(..))

spec :: Spec
spec = describe "GET /contract/{contractId}/next" $ aroundAll setup do
  describe "Valid GET /contract/{contractId}/next" do
    nextCloseContractSpec
  describe "Invalid GET /contract/{contractId}/next" do
    invalidTxIdSpec

nextCloseContractSpec :: SpecWith MarloweWebTestData
nextCloseContractSpec
  = it "returns next (canReduce == True, emptyApplicables) for a close contract" \MarloweWebTestData{..}
      -> flip runIntegrationTest runtime do
        either throw pure =<< runWebClient do
          Web.ContractState{..} <- waitUntilConfirmed (\Web.ContractState{status} -> status)
            $ getContract expectedCloseContractId
          liftIO $ contractId `shouldBe` expectedCloseContractId
          now <- liftIO Time.getCurrentTime
          next <- getContractNext contractId now now
          liftIO $ next `shouldBe` Next (CanReduce True) emptyApplicables

invalidTxIdSpec :: SpecWith MarloweWebTestData
invalidTxIdSpec = it "returns not found for invalid contract id" \MarloweWebTestData{..}
  -> flip runIntegrationTest runtime do
    result <- runWebClient do
      let invalidTxId = Web.TxOutRef
                          (toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000")
                          1
      getContract invalidTxId

    case result of
      Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } } ) ->  pure ()
      _ -> fail $ "Expected 404 response code - got " <> show result

setup :: ActionWith MarloweWebTestData -> IO ()
setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- Reader.ask
  wallet1 <- getGenesisWallet 0

  either throw pure =<< runWebClient do
    expectedCloseContractId <- createCloseContract wallet1
    liftIO $ runSpec MarloweWebTestData{..}

data MarloweWebTestData = MarloweWebTestData
  { runtime :: MarloweRuntime
  , wallet1 :: Wallet
  , expectedCloseContractId :: Web.TxOutRef
  }
