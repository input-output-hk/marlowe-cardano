{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Marlowe.Runtime.Integration.MarloweQuery
  where

import Cardano.Api (BabbageEra, TxBody)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Functor (void)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient, getContractHeaders)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, TxOutRef(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(..), MarloweVersionTag(..))
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract
import Test.Hspec
import Test.Integration.Marlowe (MarloweRuntime, withLocalMarloweRuntime)

spec :: Spec
spec = describe "MarloweQuery" $ aroundAll setup do
  getContractHeadersSpec

getContractHeadersSpec :: SpecWith MarloweQueryTestData
getContractHeadersSpec = describe "getContractHeaders" do
  let
    allContractNos = [Contract1, Contract2, Contract3, Contract4]

    contractNoToInt = \case
      Contract1 -> 0
      Contract2 -> 1
      Contract3 -> 2
      Contract4 -> 3

    allRanges :: [Range (WithUnknown ContractNo)]
    allRanges = do
      rangeStart <- Nothing : Just Unknown : (Just . Known <$> allContractNos)
      rangeOffset <- [0..5]
      rangeLimit <- [0..5]
      rangeDirection <- [Ascending, Descending]
      pure Range{..}

    contractNoToContractId :: MarloweQueryTestData -> WithUnknown ContractNo -> ContractId
    contractNoToContractId MarloweQueryTestData{..} = \case
      Unknown -> ContractId $ TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 1
      Known Contract1 -> standardContractId contract1
      Known Contract2 -> standardContractId contract2
      Known Contract3 -> standardContractId contract3
      Known Contract4 -> standardContractId contract4

    contractNoToContractHeader :: MarloweQueryTestData -> ContractNo -> ContractHeader
    contractNoToContractHeader MarloweQueryTestData{..} = \case
      Contract1 -> standardContractHeader contract1
      Contract2 -> standardContractHeader contract2
      Contract3 -> standardContractHeader contract3
      Contract4 -> standardContractHeader contract4

    getNext :: Order -> ContractNo -> Maybe ContractNo
    getNext Ascending = \case
      Contract1 -> Just Contract2
      Contract2 -> Just Contract3
      Contract3 -> Just Contract4
      Contract4 -> Nothing
    getNext Descending = \case
      Contract4 -> Just Contract3
      Contract3 -> Just Contract2
      Contract2 -> Just Contract1
      Contract1 -> Nothing


    -- rangeStart = Just (Known Contract4), rangeOffset = 3, rangeLimit = 5, rangeDirection = Descending
    expectedPage :: Range (WithUnknown ContractNo) -> Maybe (Page ContractNo ContractNo)
    expectedPage Range{..} = case rangeStart of
      Just Unknown -> Nothing
      Just (Known contractNo) -> expectedPage Range
        { rangeStart = Nothing
        , rangeOffset = rangeOffset + case rangeDirection of
            Ascending -> contractNoToInt contractNo
            Descending -> contractNoToInt Contract4 - contractNoToInt contractNo
        , ..
        }
      Nothing -> do
        let
          items = take rangeLimit $ drop rangeOffset $ case rangeDirection of
            Ascending -> allContractNos
            Descending -> reverse allContractNos
          nextRange = case (reverse items, rangeDirection) of
            ([], _) -> Nothing
            (lastItem : _, _) -> do
              nextItem <- getNext rangeDirection lastItem
              Just Range
                { rangeStart = Just nextItem
                , rangeOffset = 0
                , ..
                }
          totalCount = 4
        pure Page{..}

  for_ allRanges \range -> do
    let expectedSymbolic = expectedPage range
    it (show range <> " => " <> show expectedSymbolic) $ runMarloweQueryIntegrationTest \testData -> do
      actual <- getContractHeaders $ contractNoToContractId testData <$> range
      let
        expectedConcrete = fmap
          (bimap (contractNoToContractId testData . Known) (contractNoToContractHeader testData))
          expectedSymbolic
      liftIO $ actual `shouldBe` expectedConcrete

  for_ [1..4] \rangeLimit -> do
    let
      append prev range = do
        mPage <- getContractHeaders range
        case mPage of
          Nothing -> pure prev
          Just Page{..} -> case nextRange of
            Just next -> (prev <> items) `append` next
            Nothing -> pure $ prev <> items

    it ("Allows all contracts to be listed in ascending pages of size " <> show rangeLimit) $
      runMarloweQueryIntegrationTest \MarloweQueryTestData{..} -> do
        allContracts <- [] `append` Range{rangeStart = Nothing, rangeLimit, rangeOffset = 0, rangeDirection = Ascending}
        liftIO $ allContracts `shouldBe` (standardContractHeader <$> [contract1, contract2, contract3, contract4])

    it ("Allows all contracts to be listed in descending pages of size " <> show rangeLimit) $
      runMarloweQueryIntegrationTest \MarloweQueryTestData{..} -> do
        allContracts <- [] `append` Range{rangeStart = Nothing, rangeLimit, rangeOffset = 0, rangeDirection = Descending}
        liftIO $ allContracts `shouldBe` (standardContractHeader <$> [contract4, contract3, contract2, contract1])

setup :: ActionWith MarloweQueryTestData -> IO ()
setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- ask
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  contract1 <- createStandardContract wallet1 wallet2
  contract1Step1 <- makeInitialDeposit contract1
  contract1Step2 <- chooseGimmeTheMoney contract1Step1
  contract2 <- createStandardContract wallet1 wallet2
  contract1Step3 <- sendNotify contract1Step2
  contract1Step4 <- makeReturnDeposit contract1Step3
  contract1Step5 <- withdrawPartyAFunds contract1Step4
  contract2Step1 <- makeInitialDeposit contract2
  contract3 <- createStandardContract wallet1 wallet2
  contract4 <- createStandardContract wallet1 wallet2
  contract2Step2 <- chooseGimmeTheMoney contract2Step1
  contract2Step3 <- sendNotify contract2Step2
  contract2Step4 <- makeReturnDeposit contract2Step3
  contract3Step1 <- makeInitialDeposit contract3
  contract3Step2 <- chooseGimmeTheMoney contract3Step1
  contract3Step3 <- sendNotify contract3Step2
  liftIO $ runSpec MarloweQueryTestData{..}

data MarloweQueryTestData = MarloweQueryTestData
  { runtime :: MarloweRuntime
  , wallet1 :: Wallet
  , wallet2 :: Wallet
  , contract1 :: StandardContractInit 'V1
  , contract1Step1 :: StandardContractFundsDeposited 'V1
  , contract1Step2 :: StandardContractChoiceMade 'V1
  , contract1Step3 :: StandardContractNotified 'V1
  , contract1Step4 :: StandardContractClosed 'V1
  , contract1Step5 :: (TxBody BabbageEra, BlockHeader)
  , contract2 :: StandardContractInit 'V1
  , contract2Step1 :: StandardContractFundsDeposited 'V1
  , contract2Step2 :: StandardContractChoiceMade 'V1
  , contract2Step3 :: StandardContractNotified 'V1
  , contract2Step4 :: StandardContractClosed 'V1
  , contract3 :: StandardContractInit 'V1
  , contract3Step1 :: StandardContractFundsDeposited 'V1
  , contract3Step2 :: StandardContractChoiceMade 'V1
  , contract3Step3 :: StandardContractNotified 'V1
  , contract4 :: StandardContractInit 'V1
  }

data WithUnknown a = Unknown | Known a
  deriving (Show, Eq, Ord)

data ContractNo
  = Contract1
  | Contract2
  | Contract3
  | Contract4
  deriving (Show, Eq, Ord)

runMarloweQueryIntegrationTest :: (MarloweQueryTestData -> MarloweQueryClient Integration a) -> ActionWith MarloweQueryTestData
runMarloweQueryIntegrationTest test testData@MarloweQueryTestData{..} =
  void $ runIntegrationTest (runMarloweQueryClient $ test testData) runtime
