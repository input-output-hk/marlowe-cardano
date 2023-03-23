module Language.Marlowe.Runtime.Web.GetContracts
  where

import Cardano.Api
  ( AsType(..)
  , ShelleyWitnessSigningKey(..)
  , TextEnvelope(..)
  , TextEnvelopeType(..)
  , deserialiseFromTextEnvelope
  , serialiseToTextEnvelope
  , signShelleyTransaction
  )
import Cardano.Api.SerialiseTextEnvelope (TextEnvelopeDescr(..))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Control.Exception (throw)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.Marlowe as V1
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (Page(..), getContract, getContracts, postContract, putContract)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Network.HTTP.Types (Status(..))
import Servant.Client (ClientError(FailureResponse), ClientM)
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


createCloseContract :: Wallet -> ClientM Web.TxOutRef
createCloseContract Wallet{..}= do
  let WalletAddresses{..} = addresses
  let webChangeAddress = toDTO changeAddress
  let webExtraAddresses = Set.map toDTO extraAddresses
  let webCollateralUtxos = Set.map toDTO collateralUtxos

  Web.CreateTxEnvelope{txEnvelope = createTxBody, ..} <- postContract
    webChangeAddress
    (Just webExtraAddresses)
    (Just webCollateralUtxos)
    Web.PostContractsRequest
      { tags = mempty
      , metadata = mempty
      , version = Web.V1
      , roles = Nothing
      , contract = V1.Close
      , minUTxODeposit = 2_000_000
      }

  createTx <- liftIO $ signShelleyTransaction' createTxBody signingKeys
  putContract contractId createTx
  _ <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId
  pure contractId

signShelleyTransaction' :: Web.TextEnvelope -> [ShelleyWitnessSigningKey] -> IO Web.TextEnvelope
signShelleyTransaction' Web.TextEnvelope{..} wits = do
  let te = TextEnvelope { teType = TextEnvelopeType (T.unpack teType), teDescription = TextEnvelopeDescr (T.unpack teDescription), teRawCBOR = Web.unBase16 teCborHex }
  txBody <- case deserialiseFromTextEnvelope (AsTxBody AsBabbage) te of
    Left err -> fail $ show err
    Right a -> pure a
  pure case serialiseToTextEnvelope Nothing $ signShelleyTransaction txBody wits of
    TextEnvelope (TextEnvelopeType ty) _ bytes -> Web.TextEnvelope (T.pack ty) "" $ Web.Base16 bytes

waitUntilConfirmed :: MonadIO m => (a -> Web.TxStatus) -> m a -> m a
waitUntilConfirmed getStatus getResource = do
  resource <- getResource
  case getStatus resource of
    Web.Confirmed -> pure resource
    _ -> do
      liftIO $ threadDelay 1000
      waitUntilConfirmed getStatus getResource
