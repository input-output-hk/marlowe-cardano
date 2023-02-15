{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Marlowe.Runtime.Integration.MarloweQuery
  where

import Cardano.Api (BabbageEra, CardanoEra(BabbageEra), TxBody(..), TxBodyContent(..), getTxId)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList)
import Language.Marlowe.Protocol.Query.Client
  (MarloweQueryClient, getContractHeaders, getContractState, getTransaction, getTransactions)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId, fromCardanoTxOut)
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader, TransactionMetadata(..), TransactionOutput(..), TxId, TxOutRef(..))
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , TransactionScriptOutput(..)
  , fromChainPayoutDatum
  )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated(..), InputsApplied(..))
import Test.Hspec
import Test.Integration.Marlowe (MarloweRuntime, withLocalMarloweRuntime)

spec :: Spec
spec = describe "MarloweQuery" $ aroundAll setup do
  getContractHeadersSpec
  getContractStateSpec
  getTransactionsSpec
  getTransactionSpec

getContractHeadersSpec :: SpecWith MarloweQueryTestData
getContractHeadersSpec = describe "getContractHeaders" do
  let
    contractNoToInt = \case
      Contract1 -> 0
      Contract2 -> 1
      Contract3 -> 2
      Contract4 -> 3

    allRanges :: [Range (WithUnknown ContractNo)]
    allRanges = do
      rangeStart <- Nothing : Just Unknown : (Just . Known <$> allContractNos)
      rangeOffset <- [-1..5]
      rangeLimit <- [-1..5]
      rangeDirection <- [Ascending, Descending]
      pure Range{..}

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
    expectedPage Range{..}
      | rangeLimit <= 0 || rangeOffset < 0 = Nothing
      | otherwise = case rangeStart of
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

getContractStateSpec :: SpecWith MarloweQueryTestData
getContractStateSpec = describe "getContractState" do
  for_ (Unknown : (Known <$> allContractNos)) \contractNo -> do
    it (show contractNo) $ runMarloweQueryIntegrationTest \testData -> do
      let
        expected = SomeContractState MarloweV1 <$> case contractNo of
          Unknown -> Nothing
          Known contractNo' -> case contractNoToContractCreated testData contractNo' of
            ContractCreated{..} -> Just ContractState
              { contractId
              , roleTokenMintingPolicyId = rolesCurrency
              , metadata = TransactionMetadata metadata
              , initialBlock = contractNoToInitialBlock testData contractNo'
              , initialOutput = TransactionScriptOutput
                { address = marloweScriptAddress
                , assets
                , utxo = unContractId contractId
                , datum
                }
              , latestBlock = contractNoToLatestBlock testData contractNo'
              , latestOutput = contractNoToLatestOutput testData contractNo'
              , unclaimedPayouts = contractNoToUnclaimedPayouts testData contractNo'
              }
      actual <- getContractState $ contractNoToContractId testData contractNo
      liftIO $ actual `shouldBe` expected

getTransactionsSpec :: SpecWith MarloweQueryTestData
getTransactionsSpec = describe "getTransactions" do
  for_ (Unknown : (Known <$> allContractNos)) \contractNo -> do
    it (show contractNo) $ runMarloweQueryIntegrationTest \testData -> do
      let
        expected = SomeTransactions MarloweV1 <$> case contractNo of
          Unknown -> Nothing
          Known contractNo' -> Just $ contractNoToTransactions testData contractNo'
      actual <- getTransactions $ contractNoToContractId testData contractNo
      liftIO $ actual `shouldBe` expected

getTransactionSpec :: SpecWith MarloweQueryTestData
getTransactionSpec = describe "getTransaction" do
  for_ (Unknown : ((Known . Left <$> allContractNos) <> (Known . Right <$> allTxNos))) \txNo -> do
    it (show txNo) $ runMarloweQueryIntegrationTest \testData -> do
      let
        expected = case txNo of
          Unknown -> Nothing
          Known (Left _) -> Nothing
          Known (Right txNo') -> Just $ txNoToSomeTransaction testData txNo'
      actual <- getTransaction $ contractOrTxNoToTxId testData txNo
      liftIO $ actual `shouldBe` expected

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
  deriving (Show, Eq, Ord, Enum, Bounded)

data TxNo
  = Contract1Step1
  | Contract1Step2
  | Contract1Step3
  | Contract1Step4
  | Contract2Step1
  | Contract2Step2
  | Contract2Step3
  | Contract2Step4
  | Contract3Step1
  | Contract3Step2
  | Contract3Step3
  deriving (Show, Eq, Ord, Enum, Bounded)

allContractNos :: [ContractNo]
allContractNos = [minBound..maxBound]

allTxNos :: [TxNo]
allTxNos = [minBound..maxBound]

contractNoToContractId :: MarloweQueryTestData -> WithUnknown ContractNo -> ContractId
contractNoToContractId MarloweQueryTestData{..} = \case
  Unknown -> ContractId $ TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 1
  Known Contract1 -> standardContractId contract1
  Known Contract2 -> standardContractId contract2
  Known Contract3 -> standardContractId contract3
  Known Contract4 -> standardContractId contract4

contractOrTxNoToTxId :: MarloweQueryTestData -> WithUnknown (Either ContractNo TxNo) -> TxId
contractOrTxNoToTxId testData = \case
  Unknown -> "0000000000000000000000000000000000000000000000000000000000000000"
  Known (Left contractNo) -> case contractNoToContractId testData $ Known contractNo of
    ContractId TxOutRef{..} -> txId
  Known (Right txNo) -> txNoToTxId testData txNo

txNoToInputsApplied :: MarloweQueryTestData -> TxNo -> InputsApplied BabbageEra 'V1
txNoToInputsApplied MarloweQueryTestData{..} = \case
  Contract1Step1 -> initialFundsDeposited contract1Step1
  Contract1Step2 -> gimmeTheMoneyChosen contract1Step2
  Contract1Step3 -> notified contract1Step3
  Contract1Step4 -> returnDeposited contract1Step4
  Contract2Step1 -> initialFundsDeposited contract2Step1
  Contract2Step2 -> gimmeTheMoneyChosen contract2Step2
  Contract2Step3 -> notified contract2Step3
  Contract2Step4 -> returnDeposited contract2Step4
  Contract3Step1 -> initialFundsDeposited contract3Step1
  Contract3Step2 -> gimmeTheMoneyChosen contract3Step2
  Contract3Step3 -> notified contract3Step3

txNoToInput :: MarloweQueryTestData -> TxNo -> TxOutRef
txNoToInput MarloweQueryTestData{..} = \case
  Contract1Step1 -> unContractId $ standardContractId contract1
  Contract1Step2 -> utxo $ fromJust $ output $ initialFundsDeposited contract1Step1
  Contract1Step3 -> utxo $ fromJust $ output $ gimmeTheMoneyChosen contract1Step2
  Contract1Step4 -> utxo $ fromJust $ output $ notified contract1Step3
  Contract2Step1 -> unContractId $ standardContractId contract2
  Contract2Step2 -> utxo $ fromJust $ output $ initialFundsDeposited contract2Step1
  Contract2Step3 -> utxo $ fromJust $ output $ gimmeTheMoneyChosen contract2Step2
  Contract2Step4 -> utxo $ fromJust $ output $ notified contract2Step3
  Contract3Step1 -> unContractId $ standardContractId contract3
  Contract3Step2 -> utxo $ fromJust $ output $ initialFundsDeposited contract3Step1
  Contract3Step3 -> utxo $ fromJust $ output $ gimmeTheMoneyChosen contract3Step2

txNoToConsumer :: MarloweQueryTestData -> TxNo -> Maybe TxId
txNoToConsumer MarloweQueryTestData{..} = fmap inputsAppliedTxId . \case
  Contract1Step1 -> Just $ gimmeTheMoneyChosen contract1Step2
  Contract1Step2 -> Just $ notified contract1Step3
  Contract1Step3 -> Just $ returnDeposited contract1Step4
  Contract1Step4 -> Nothing
  Contract2Step1 -> Just $ gimmeTheMoneyChosen contract2Step2
  Contract2Step2 -> Just $ notified contract2Step3
  Contract2Step3 -> Just $ returnDeposited contract2Step4
  Contract2Step4 -> Nothing
  Contract3Step1 -> Just $ gimmeTheMoneyChosen contract3Step2
  Contract3Step2 -> Just $ notified contract3Step3
  Contract3Step3 -> Nothing

txNoToBlockHeader :: MarloweQueryTestData -> TxNo -> BlockHeader
txNoToBlockHeader MarloweQueryTestData{..} = \case
  Contract1Step1 -> initialDepositBlock contract1Step1
  Contract1Step2 -> choiceBlock contract1Step2
  Contract1Step3 -> notifiedBlock contract1Step3
  Contract1Step4 -> returnDepositBlock contract1Step4
  Contract2Step1 -> initialDepositBlock contract2Step1
  Contract2Step2 -> choiceBlock contract2Step2
  Contract2Step3 -> notifiedBlock contract2Step3
  Contract2Step4 -> returnDepositBlock contract2Step4
  Contract3Step1 -> initialDepositBlock contract3Step1
  Contract3Step2 -> choiceBlock contract3Step2
  Contract3Step3 -> notifiedBlock contract3Step3

txNoToTxId :: MarloweQueryTestData -> TxNo -> TxId
txNoToTxId testData = inputsAppliedTxId . txNoToInputsApplied testData

inputsAppliedTxId :: InputsApplied era v -> TxId
inputsAppliedTxId InputsApplied{..} = fromCardanoTxId $ getTxId txBody

contractNoToStandardContract :: MarloweQueryTestData -> ContractNo -> StandardContractInit 'V1
contractNoToStandardContract MarloweQueryTestData{..} = \case
  Contract1 -> contract1
  Contract2 -> contract2
  Contract3 -> contract3
  Contract4 -> contract4

contractNoToContractCreated :: MarloweQueryTestData -> ContractNo -> ContractCreated BabbageEra 'V1
contractNoToContractCreated testData = contractCreated . contractNoToStandardContract testData

contractNoToInitialBlock :: MarloweQueryTestData -> ContractNo -> BlockHeader
contractNoToInitialBlock testData = createdBlock . contractNoToStandardContract testData

contractNoToLatestBlock :: MarloweQueryTestData -> ContractNo -> BlockHeader
contractNoToLatestBlock MarloweQueryTestData{..} = \case
  Contract1 -> returnDepositBlock contract1Step4 -- note: _not_ `snd contract1Step5` (which is a withdrawal)
  Contract2 -> returnDepositBlock contract2Step4
  Contract3 -> notifiedBlock contract3Step3
  Contract4 -> createdBlock contract4

contractNoToLatestOutput :: MarloweQueryTestData -> ContractNo -> Maybe (TransactionScriptOutput 'V1)
contractNoToLatestOutput MarloweQueryTestData{..} = \case
  Contract1 -> Nothing
  Contract2 -> case returnDeposited contract2Step4 of
    InputsApplied{..} -> output
  Contract3 -> case notified contract3Step3 of
    InputsApplied{..} -> output
  Contract4 -> case contractCreated contract4 of
    ContractCreated{..} -> Just TransactionScriptOutput
      { address = marloweScriptAddress
      , assets
      , utxo = unContractId contractId
      , datum
      }

contractNoToUnclaimedPayouts :: MarloweQueryTestData -> ContractNo -> Map TxOutRef (Payout 'V1)
contractNoToUnclaimedPayouts MarloweQueryTestData{..} =  \case
  Contract1 -> mempty
  Contract2 -> case returnDeposited contract2Step4 of
    InputsApplied{..} -> Map.fromList do
      (ix, TransactionOutput{..}) <- case txBody of
        TxBody TxBodyContent{..} -> zip [0..] $ fromCardanoTxOut BabbageEra <$> txOuts
      guard $ address == payoutScriptAddress (contractCreated contract2)
      payout <- maybeToList $ Payout address assets <$> (fromChainPayoutDatum MarloweV1 =<< datum)
      pure (TxOutRef (fromCardanoTxId $ getTxId txBody) ix, payout)
  Contract3 -> mempty
  Contract4 -> mempty

contractNoToTransactions :: MarloweQueryTestData -> ContractNo -> [Core.Transaction 'V1]
contractNoToTransactions MarloweQueryTestData{..} = \case
  Contract1 ->
    [ inputsAppliedToTransaction (initialDepositBlock contract1Step1) (initialFundsDeposited contract1Step1)
    , inputsAppliedToTransaction (choiceBlock contract1Step2) (gimmeTheMoneyChosen contract1Step2)
    , inputsAppliedToTransaction (notifiedBlock contract1Step3) (notified contract1Step3)
    , inputsAppliedToTransaction (returnDepositBlock contract1Step4) (returnDeposited contract1Step4)
    ]
  Contract2 ->
    [ inputsAppliedToTransaction (initialDepositBlock contract2Step1) (initialFundsDeposited contract2Step1)
    , inputsAppliedToTransaction (choiceBlock contract2Step2) (gimmeTheMoneyChosen contract2Step2)
    , inputsAppliedToTransaction (notifiedBlock contract2Step3) (notified contract2Step3)
    , inputsAppliedToTransaction (returnDepositBlock contract2Step4) (returnDeposited contract2Step4)
    ]
  Contract3 ->
    [ inputsAppliedToTransaction (initialDepositBlock contract3Step1) (initialFundsDeposited contract3Step1)
    , inputsAppliedToTransaction (choiceBlock contract3Step2) (gimmeTheMoneyChosen contract3Step2)
    , inputsAppliedToTransaction (notifiedBlock contract3Step3) (notified contract3Step3)
    ]
  Contract4 -> mempty

txNoToSomeTransaction :: MarloweQueryTestData -> TxNo -> SomeTransaction
txNoToSomeTransaction testData txNo = SomeTransaction
  MarloweV1
  (txNoToInput testData txNo)
  (txNoToConsumer testData txNo)
  (inputsAppliedToTransaction (txNoToBlockHeader testData txNo) (txNoToInputsApplied testData txNo))

runMarloweQueryIntegrationTest :: (MarloweQueryTestData -> MarloweQueryClient Integration a) -> ActionWith MarloweQueryTestData
runMarloweQueryIntegrationTest test testData@MarloweQueryTestData{..} =
  void $ runIntegrationTest (runMarloweQueryClient $ test testData) runtime
