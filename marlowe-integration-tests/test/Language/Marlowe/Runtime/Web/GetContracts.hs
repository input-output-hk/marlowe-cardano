module Language.Marlowe.Runtime.Web.GetContracts
  where

import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import Data.Proxy (Proxy(Proxy))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (Page(..), getContracts)
import Language.Marlowe.Runtime.Web.Common (createCloseContract)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse))
import Servant.Client.Streaming (ResponseF(Response, responseStatusCode))
import Servant.Pagination (Range(..), RangeOrder(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "GET /contracts" do
  getContractsValidSpec
  getContractsInvalidSpec
  getContractsValidNextPageSpec

getContractsValidSpec :: Spec
getContractsValidSpec = describe "Valid GET /contracts" do
  noContractsValidSpec
  singleContractValidSpec
  multipleContractValidSpec

getContractsInvalidSpec :: Spec
getContractsInvalidSpec = describe "Invalid GET /contracts" do
  invalidTxIdSpec

getContractsValidNextPageSpec :: Spec
getContractsValidNextPageSpec = describe "Valid Pagination of GET /contracts" do
  firstPageAscValidSpec
  secondPageAscValidSpec
  firstPageDescValidSpec
  secondPageDescValidSpec

firstPageAscValidSpec :: Spec
firstPageAscValidSpec = it "returns the first page of contract headers in ascending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2

  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    _ <- createCloseContract wallet3
    let
      testRange =  Range
        {
          rangeValue = Just expectedContractId1
        , rangeOffset = 0
        , rangeLimit = 2
        , rangeOrder = RangeAsc
        , rangeField = Proxy
        }
    Page {..} <- getContracts Nothing Nothing $ Just testRange

    liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId1, expectedContractId2]

secondPageAscValidSpec :: Spec
secondPageAscValidSpec = it "returns the second page of contract headers in ascending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2

  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    expectedContractId3 <- createCloseContract wallet3
    let
      testRange =  Range
        {
          rangeValue = Just expectedContractId1
        , rangeOffset = 1
        , rangeLimit = 2
        , rangeOrder = RangeAsc
        , rangeField = Proxy
        }
    Page {..} <- getContracts Nothing Nothing $ Just testRange

    liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId2, expectedContractId3]

firstPageDescValidSpec :: Spec
firstPageDescValidSpec = it "returns the first page of contract headers in descending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2

  either throw pure =<< runWebClient do
    _ <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    expectedContractId3 <- createCloseContract wallet3
    let
      testRange =  Range
        {
          rangeValue = Just expectedContractId3
        , rangeOffset = 0
        , rangeLimit = 2
        , rangeOrder = RangeDesc
        , rangeField = Proxy
        }
    Page {..} <- getContracts Nothing Nothing $ Just testRange

    liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId3, expectedContractId2]

secondPageDescValidSpec :: Spec
secondPageDescValidSpec = it "returns the second page of contract headers in descending order" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  wallet3 <- getGenesisWallet 2

  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    expectedContractId3 <- createCloseContract wallet3
    let
      testRange =  Range
        {
          rangeValue = Just expectedContractId3
        , rangeOffset = 1
        , rangeLimit = 2
        , rangeOrder = RangeDesc
        , rangeField = Proxy
        }
    Page {..} <- getContracts Nothing Nothing $ Just testRange

    liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId2, expectedContractId1]


noContractsValidSpec :: Spec
noContractsValidSpec = it "returns an empty list" $ withLocalMarloweRuntime $ runIntegrationTest do
  either throw pure =<< runWebClient do
    Page {..}<- getContracts Nothing Nothing Nothing
    liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` []

singleContractValidSpec :: Spec
singleContractValidSpec  = it "returns a list with single contract header" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet <- getGenesisWallet 0

  either throw pure =<< runWebClient do
    expectedContractId <- createCloseContract wallet
    Page {..}<- getContracts Nothing Nothing Nothing
    liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId]

multipleContractValidSpec :: Spec
multipleContractValidSpec  = it "returns a list with multiple contract headers" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1

  either throw pure =<< runWebClient do
    expectedContractId1 <- createCloseContract wallet1
    expectedContractId2 <- createCloseContract wallet2
    Page {..}<- getContracts Nothing Nothing Nothing
    liftIO $ fmap (\Web.ContractHeader{..} -> contractId) items `shouldBe` [expectedContractId2, expectedContractId1]


invalidTxIdSpec :: Spec
invalidTxIdSpec = it "returns an error message" $ withLocalMarloweRuntime $ runIntegrationTest do
  result <- runWebClient do
    let
      invalidRange =  Range
        {
          rangeValue = Just $ Web.TxOutRef (toDTO @Chain.TxId "0000000000000000000000000000000000000000000000000000000000000000") 1
        , rangeOffset = 0
        , rangeLimit = 1
        , rangeOrder = RangeAsc
        , rangeField = Proxy
        }
    getContracts Nothing Nothing $ Just invalidRange

  case result of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 416 } } ) ->  pure ()
    _ -> fail $ "Expected 416 response code - got " <> show result


