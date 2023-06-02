{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Integration.MarloweQuery
  where

import Cardano.Api (BabbageEra, TxBody(..), getTxId)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Data.Bifunctor (bimap)
import Data.Foldable (Foldable(fold), for_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Marlowe.Protocol.Query.Client
  ( MarloweQueryClient
  , getContractHeaders
  , getContractState
  , getTransaction
  , getTransactions
  , getWithdrawal
  , getWithdrawals
  )
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (AssetId(..), BlockHeader, PolicyId, TxId, TxOutRef(..))
import Language.Marlowe.Runtime.Client (runMarloweQueryClient)
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweMetadataTag(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , TransactionScriptOutput(..)
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
  describe "GetHeaders" $ paginatedQuerySpec GetHeaders runMarloweQueryIntegrationTest
  getContractStateSpec
  getTransactionsSpec
  getTransactionSpec
  describe "GetWithdrawals" $ paginatedQuerySpec GetWithdrawals runMarloweQueryIntegrationTest
  getWithdrawalSpec

data GetHeaders = GetHeaders
data GetWithdrawals = GetWithdrawals

instance PaginatedQuery GetHeaders where
  type Filter GetHeaders = ContractFilter
  type Ref GetHeaders = ContractId
  type Item GetHeaders = ContractHeader
  data RefSym GetHeaders
    = Contract1
    | Contract2
    | Contract3
    | Contract4
    deriving (Eq, Ord, Show, Enum, Bounded)
  data FilterSym GetHeaders = ContractFilterSym
    { tagsSym :: Set TestTag
    , roleCurrenciesSym :: Set (RefSym GetHeaders)
    }
    deriving (Eq, Ord, Show)
  applyFilter _ ContractFilterSym{..} ref =
    (Set.null tagsSym || not (Set.null $ Set.intersection tagsSym $ tagsForContract ref))
      && (Set.null roleCurrenciesSym || Set.member ref roleCurrenciesSym)
  toRef _ MarloweQueryTestData{..} = \case
    Unknown -> ContractId $ TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 1
    Known Contract1 -> standardContractId contract1
    Known Contract2 -> standardContractId contract2
    Known Contract3 -> standardContractId contract3
    Known Contract4 -> standardContractId contract4
  toItem _ MarloweQueryTestData{..} = \case
    Contract1 -> standardContractHeader contract1
    Contract2 -> standardContractHeader contract2
    Contract3 -> standardContractHeader contract3
    Contract4 -> standardContractHeader contract4
  toFilter _ MarloweQueryTestData{..} ContractFilterSym{..} = ContractFilter
    { tags = testTagsToTags tagsSym
    , roleCurrencies = flip Set.map roleCurrenciesSym \case
        Contract1 -> standardContractRoleCurrency contract1
        Contract2 -> standardContractRoleCurrency contract2
        Contract3 -> standardContractRoleCurrency contract3
        Contract4 -> standardContractRoleCurrency contract4
    }
  enumerateFilters q = do
    tagsSym <- Set.toList $ Set.powerSet $ Set.fromList [minBound..maxBound]
    roleCurrenciesSym <- Set.toList $ Set.powerSet $ Set.fromList $ allRefs q
    pure ContractFilterSym{..}
  runQuery _ = getContractHeaders

instance PaginatedQuery GetWithdrawals where
  type Filter GetWithdrawals = WithdrawalFilter
  type Ref GetWithdrawals = TxId
  type Item GetWithdrawals = Withdrawal
  data RefSym GetWithdrawals
    = Withdrawal1
    | Withdrawal2
    deriving (Eq, Ord, Show, Enum, Bounded)
  newtype FilterSym GetWithdrawals = WithdrawalFilterSym
    { withdrawalRoleCurrenciesSym :: Set (RefSym GetHeaders)
    }
    deriving (Eq, Ord, Show)
  applyFilter _ WithdrawalFilterSym{..} = (Set.null withdrawalRoleCurrenciesSym ||) . \case
    Withdrawal1 -> Set.member Contract1 withdrawalRoleCurrenciesSym
    Withdrawal2 -> Set.member Contract2 withdrawalRoleCurrenciesSym
  toRef _ MarloweQueryTestData{..} = \case
    Unknown -> "0000000000000000000000000000000000000000000000000000000000000000"
    Known Withdrawal1 -> fromCardanoTxId $ getTxId $ fst contract1Step5
    Known Withdrawal2 -> fromCardanoTxId $ getTxId $ fst contract2Step5
  toItem _ MarloweQueryTestData{..} ref = Withdrawal
      { block
      , withdrawnPayouts
      , withdrawalTx = fromCardanoTxId $ getTxId txBody
      }
      where
        ((txBody, block), withdrawnPayouts) = case ref of
          Withdrawal1 ->
            ( contract1Step5
            , case inputsAppliedToTransaction (returnDepositBlock contract1Step4) (returnDeposited contract1Step4) of
                Core.Transaction{output=Core.TransactionOutput{payouts}} -> flip Map.mapWithKey payouts \payout Payout{..} -> case datum of
                  AssetId{..} -> PayoutRef
                    { contractId = standardContractId contract1
                    , payout
                    , rolesCurrency = policyId
                    , role = tokenName
                    }
            )
          Withdrawal2 ->
            ( contract2Step5
            , case inputsAppliedToTransaction (returnDepositBlock contract2Step4) (returnDeposited contract2Step4) of
                Core.Transaction{output=Core.TransactionOutput{payouts}} -> flip Map.mapWithKey payouts \payout Payout{..} -> case datum of
                  AssetId{..} -> PayoutRef
                    { contractId = standardContractId contract2
                    , payout
                    , rolesCurrency = policyId
                    , role = tokenName
                    }

            )
  toFilter _ MarloweQueryTestData{..} WithdrawalFilterSym{..} = WithdrawalFilter
    { roleCurrencies = flip Set.map withdrawalRoleCurrenciesSym \case
        Contract1 -> standardContractRoleCurrency contract1
        Contract2 -> standardContractRoleCurrency contract2
        Contract3 -> standardContractRoleCurrency contract3
        Contract4 -> standardContractRoleCurrency contract4
    }
  enumerateFilters _ = WithdrawalFilterSym <$> Set.toList (Set.powerSet $ Set.fromList $ allRefs GetHeaders)
  runQuery _ = getWithdrawals

standardContractRoleCurrency :: StandardContractInit 'V1 -> PolicyId
standardContractRoleCurrency StandardContractInit{..} = case contractCreated of
  ContractCreated{..} -> rolesCurrency

data TestTag = Tag1 | Tag2
  deriving (Eq, Ord, Show, Enum, Bounded)

testTagsToTags :: Set TestTag -> Set MarloweMetadataTag
testTagsToTags = Set.map testTagToTag

testTagToTag :: TestTag -> MarloweMetadataTag
testTagToTag = MarloweMetadataTag . T.pack . show

tagsForContract :: RefSym GetHeaders -> Set TestTag
tagsForContract = \case
  Contract1 -> mempty
  Contract2 -> Set.singleton Tag1
  Contract3 -> Set.fromList [Tag1, Tag2]
  Contract4 -> Set.singleton Tag2

getContractStateSpec :: SpecWith MarloweQueryTestData
getContractStateSpec = describe "getContractState" do
  for_ (Unknown : (Known <$> allRefs GetHeaders)) \contractNo -> do
    it (show contractNo) $ runMarloweQueryIntegrationTest \testData -> do
      let
        expected = SomeContractState MarloweV1 <$> case contractNo of
          Unknown -> Nothing
          Known contractNo' -> case contractNoToContractCreated testData contractNo' of
            ContractCreated{..} -> Just ContractState
              { contractId
              , roleTokenMintingPolicyId = rolesCurrency
              , metadata
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
      actual <- getContractState $ toRef GetHeaders testData contractNo
      liftIO $ actual `shouldBe` expected

getTransactionsSpec :: SpecWith MarloweQueryTestData
getTransactionsSpec = describe "getTransactions" do
  for_ (Unknown : (Known <$> allRefs GetHeaders)) \contractNo -> do
    it (show contractNo) $ runMarloweQueryIntegrationTest \testData -> do
      let
        expected = SomeTransactions MarloweV1 <$> case contractNo of
          Unknown -> Nothing
          Known contractNo' -> Just $ contractNoToTransactions testData contractNo'
      actual <- getTransactions $ toRef GetHeaders testData contractNo
      liftIO $ actual `shouldBe` expected

getTransactionSpec :: SpecWith MarloweQueryTestData
getTransactionSpec = describe "getTransaction" do
  for_ (Unknown : ((Known . Left <$> allRefs GetHeaders) <> (Known . Right <$> allTxNos))) \txNo -> do
    it (show txNo) $ runMarloweQueryIntegrationTest \testData -> do
      let
        expected = case txNo of
          Unknown -> Nothing
          Known (Left _) -> Nothing
          Known (Right txNo') -> Just $ txNoToSomeTransaction testData txNo'
      actual <- getTransaction $ contractOrTxNoToTxId testData txNo
      liftIO $ actual `shouldBe` expected

getWithdrawalSpec :: SpecWith MarloweQueryTestData
getWithdrawalSpec = describe "getWithdrawal" do
  it "Returns Nothing for a fake txId" $ runMarloweQueryIntegrationTest \_ -> do
    actual <- getWithdrawal "0000000000000000000000000000000000000000000000000000000000000000"
    liftIO $ actual `shouldBe` Nothing
  it "Returns Nothing for a create txId" $ runMarloweQueryIntegrationTest \testData -> do
    actual <- getWithdrawal $ contractOrTxNoToTxId testData $ Known $ Left Contract1
    liftIO $ actual `shouldBe` Nothing
  it "Returns Nothing for an applyInputs txId" $ runMarloweQueryIntegrationTest \testData -> do
    actual <- getWithdrawal $ contractOrTxNoToTxId testData $ Known $ Right Contract1Step4
    liftIO $ actual `shouldBe` Nothing
  it "Returns Just for a withdrawal txId" $ runMarloweQueryIntegrationTest \testData@MarloweQueryTestData{..} -> do
    actual <- getWithdrawal $ fromCardanoTxId $ getTxId $ fst contract1Step5
    liftIO $ actual `shouldBe` Just (contract1Step5Withdrawal testData)

setup :: ActionWith MarloweQueryTestData -> IO ()
setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- ask
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  contract1 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract1) wallet1 wallet2
  contract1Step1 <- makeInitialDeposit contract1
  contract1Step2 <- chooseGimmeTheMoney contract1Step1
  contract2 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract2) wallet1 wallet2
  contract1Step3 <- sendNotify contract1Step2
  contract1Step4 <- makeReturnDeposit contract1Step3
  contract1Step5 <- withdrawPartyAFunds contract1Step4
  contract2Step1 <- makeInitialDeposit contract2
  contract3 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract3) wallet1 wallet2
  contract4 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract4) wallet1 wallet2
  contract2Step2 <- chooseGimmeTheMoney contract2Step1
  contract2Step3 <- sendNotify contract2Step2
  contract2Step4 <- makeReturnDeposit contract2Step3
  contract2Step5 <- withdrawPartyAFunds contract2Step4
  contract3Step1 <- makeInitialDeposit contract3
  contract3Step2 <- chooseGimmeTheMoney contract3Step1
  contract3Step3 <- sendNotify contract3Step2
  contract3Step4 <- makeReturnDeposit contract3Step3
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
  , contract2Step5 :: (TxBody BabbageEra, BlockHeader)
  , contract3 :: StandardContractInit 'V1
  , contract3Step1 :: StandardContractFundsDeposited 'V1
  , contract3Step2 :: StandardContractChoiceMade 'V1
  , contract3Step3 :: StandardContractNotified 'V1
  , contract3Step4 :: StandardContractClosed 'V1
  , contract4 :: StandardContractInit 'V1
  }

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
  | Contract3Step4
  deriving (Show, Eq, Ord, Enum, Bounded)

data WithUnknown a = Unknown | Known a
  deriving (Show, Eq, Ord)

class (Eq (RefSym q), Enum (RefSym q), Bounded (RefSym q)) => PaginatedQuery q where
  type Filter q :: *
  type Ref q :: *
  type Item q :: *
  data FilterSym q :: *
  data RefSym q :: *
  applyFilter :: q -> FilterSym q -> RefSym q -> Bool
  toRef :: q -> MarloweQueryTestData -> WithUnknown (RefSym q) -> Ref q
  toItem :: q -> MarloweQueryTestData -> RefSym q -> Item q
  toFilter :: q -> MarloweQueryTestData -> FilterSym q -> Filter q
  enumerateFilters :: q -> [FilterSym q]
  runQuery :: Applicative m => q -> Filter q -> Range (Ref q) -> MarloweQueryClient m (Maybe (Page (Ref q) (Item q)))

runQuerySym
  :: (PaginatedQuery q, Applicative m)
  => q
  -> MarloweQueryTestData
  -> FilterSym q
  -> Range (WithUnknown (RefSym q))
  -> MarloweQueryClient m (Maybe (Page (Ref q) (Item q)))
runQuerySym q testData filter_ range = runQuery q (toFilter q testData filter_) (toRef q testData <$> range)

paginatedQuerySpec
  :: (PaginatedQuery q, Show (RefSym q), Show (Item q), Show (Ref q), Eq (Item q), Eq (Ref q), Show (FilterSym q))
  => q
  -> ((MarloweQueryTestData -> MarloweQueryClient Integration ()) -> MarloweQueryTestData -> IO ())
  -> SpecWith MarloweQueryTestData
paginatedQuerySpec q runner = do
  describe "Range and filter combinations" do
    for_ (enumerateFilters q) \filter_ -> do
      for_ (allRanges q filter_) \range -> do
        let expectedSym = applyRange q filter_ range
        it (show filter_ <> "; " <> show range <> " => " <> show expectedSym) $ runner \testData -> do
          actual <- runQuerySym q testData filter_ range
          let expected = fmap (bimap (toRef q testData . Known) (toItem q testData)) expectedSym
          liftIO $ actual `shouldBe` expected

  describe "Page navigation" do
    for_ (enumerateFilters q) \filter_ -> do
      for_ (filter (isValidStartingRange q filter_) $ allRanges q filter_) \range -> do
        let expectedSym = getFilteredSortedRefs q filter_ range
        it (show filter_ <> "; pageSize = " <> show (rangeLimit range) <> "; start = " <> show (rangeStart range) <> "; order = " <> show (rangeDirection range) <> " => " <> show expectedSym) $ runner \testData -> do
          actual <- appendRange q testData filter_ [] $ toRef q testData <$> range
          let expected = toItem q testData <$> expectedSym
          liftIO $ actual `shouldBe` Just (length expected, expected)

isValidStartingRange :: PaginatedQuery q => q -> FilterSym q -> Range (WithUnknown (RefSym q)) -> Bool
isValidStartingRange q filter_ Range{..} = case getFilteredSortedRefs q filter_ Range{..} of
  [] -> False
  a : _ -> (isNothing rangeStart || rangeStart == Just (Known a))
    && rangeOffset == 0
    && rangeLimit > 0

appendRange
  :: Monad m
  => PaginatedQuery q
  => q
  -> MarloweQueryTestData
  -> FilterSym q
  -> [Item q]
  -> Range (Ref q)
  -> MarloweQueryClient m (Maybe (Int, [Item q]))
appendRange q testData filter_ prev range = runQuery q (toFilter q testData filter_) range >>= \case
  Nothing -> pure Nothing
  Just Page{..} -> case nextRange of
    Nothing -> pure $ Just (totalCount, prev <> items)
    Just range' -> appendRange q testData filter_ (prev <> items) range'

allRefs :: PaginatedQuery q => q -> [RefSym q]
allRefs _ = [minBound..maxBound]

getFilteredSortedRefs :: PaginatedQuery q => q -> FilterSym q -> Range a -> [RefSym q]
getFilteredSortedRefs q filter_ Range{..} = case rangeDirection of
  Ascending -> filter (applyFilter q filter_) $ allRefs q
  Descending -> reverse $ filter (applyFilter q filter_) $ allRefs q

getFilteredRefs :: PaginatedQuery q => q -> FilterSym q -> [RefSym q]
getFilteredRefs q filter_ = filter (applyFilter q filter_) $ allRefs q

allRanges :: PaginatedQuery q => q -> FilterSym q -> [Range (WithUnknown (RefSym q))]
allRanges q filter_ = fold
  [ allValidRanges
  , badBoundsRanges
  , notFoundRanges
  ]
  where
  allValidRanges = do
    let filteredRefs = getFilteredRefs q filter_
    rangeStart <- Nothing : (Just . Known <$> allRefs q)
    rangeOffset <- [0..(length filteredRefs)]
    rangeLimit <- [1..(length filteredRefs)]
    rangeDirection <- [Ascending, Descending]
    pure Range{..}

  badBoundsRanges = do
    let rangeStart = Nothing
    rangeOffset <- [-1, 0]
    rangeLimit <- [-1, 0, 1]
    guard $ rangeOffset == -1 || rangeLimit < 1
    let rangeDirection = Ascending
    pure Range{..}

  notFoundRanges = do
    let filteredRefs = getFilteredRefs q filter_
    rangeStart <- Just <$> (Unknown : (Known <$> filteredRefs))
    let rangeOffset = 0
    let rangeLimit = 1
    let rangeDirection = Ascending
    pure Range{..}

applyRange
  :: PaginatedQuery q
  => q
  -> FilterSym q
  -> Range (WithUnknown (RefSym q))
  -> Maybe (Page (RefSym q) (RefSym q))
applyRange q filter_ Range{..} = do
  guard $ rangeLimit > 0 && rangeOffset >= 0
  let sortedRefs = getFilteredSortedRefs q filter_ Range{..}
  trimmedRefs <- case rangeStart of
    Nothing -> pure sortedRefs
    Just Unknown -> Nothing
    Just (Known start) -> from start sortedRefs
  let itemsWithNext = take (rangeLimit + 1) $ drop rangeOffset trimmedRefs
  pure Page
    { items = take rangeLimit itemsWithNext
    , nextRange = case drop rangeLimit itemsWithNext of
        [] -> Nothing
        a : _ -> Just $ Range
          { rangeStart = Just a
          , rangeOffset = 0
          , ..
          }
    , totalCount = length sortedRefs
    }

from :: Eq a => a -> [a] -> Maybe [a]
from _ [] = Nothing
from start (a : as)
  | a == start = Just $ a : as
  | otherwise = from start as

allTxNos :: [TxNo]
allTxNos = [minBound..maxBound]

contractOrTxNoToTxId :: MarloweQueryTestData -> WithUnknown (Either (RefSym GetHeaders) TxNo) -> TxId
contractOrTxNoToTxId testData = \case
  Unknown -> "0000000000000000000000000000000000000000000000000000000000000000"
  Known (Left contractNo) -> case toRef GetHeaders testData $ Known contractNo of
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
  Contract3Step4 -> returnDeposited contract3Step4

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
  Contract3Step4 -> utxo $ fromJust $ output $ notified contract3Step3

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
  Contract3Step3 -> Just $ returnDeposited contract3Step4
  Contract3Step4 -> Nothing

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
  Contract3Step4 -> returnDepositBlock contract3Step4

txNoToTxId :: MarloweQueryTestData -> TxNo -> TxId
txNoToTxId testData = inputsAppliedTxId . txNoToInputsApplied testData

inputsAppliedTxId :: InputsApplied era v -> TxId
inputsAppliedTxId InputsApplied{..} = fromCardanoTxId $ getTxId txBody

contractNoToStandardContract :: MarloweQueryTestData -> RefSym GetHeaders -> StandardContractInit 'V1
contractNoToStandardContract MarloweQueryTestData{..} = \case
  Contract1 -> contract1
  Contract2 -> contract2
  Contract3 -> contract3
  Contract4 -> contract4

contractNoToContractCreated :: MarloweQueryTestData -> RefSym GetHeaders -> ContractCreated BabbageEra 'V1
contractNoToContractCreated testData = contractCreated . contractNoToStandardContract testData

contractNoToInitialBlock :: MarloweQueryTestData -> RefSym GetHeaders -> BlockHeader
contractNoToInitialBlock testData = createdBlock . contractNoToStandardContract testData

contractNoToLatestBlock :: MarloweQueryTestData -> RefSym GetHeaders -> BlockHeader
contractNoToLatestBlock MarloweQueryTestData{..} = \case
  Contract1 -> returnDepositBlock contract1Step4 -- note: _not_ `snd contract1Step5` (which is a withdrawal)
  Contract2 -> returnDepositBlock contract2Step4
  Contract3 -> returnDepositBlock contract3Step4
  Contract4 -> createdBlock contract4

contractNoToLatestOutput :: MarloweQueryTestData -> RefSym GetHeaders -> Maybe (TransactionScriptOutput 'V1)
contractNoToLatestOutput MarloweQueryTestData{..} = \case
  Contract1 -> Nothing
  Contract2 -> case returnDeposited contract2Step4 of
    InputsApplied{..} -> output
  Contract3 -> case returnDeposited contract3Step4 of
    InputsApplied{..} -> output
  Contract4 -> case contractCreated contract4 of
    ContractCreated{..} -> Just TransactionScriptOutput
      { address = marloweScriptAddress
      , assets
      , utxo = unContractId contractId
      , datum
      }

contractNoToUnclaimedPayouts :: MarloweQueryTestData -> RefSym GetHeaders -> Map TxOutRef (Payout 'V1)
contractNoToUnclaimedPayouts MarloweQueryTestData{..} =  \case
  Contract1 -> mempty
  Contract2 -> mempty
  Contract3 -> case inputsAppliedToTransaction (returnDepositBlock contract3Step4) (returnDeposited contract3Step4) of
    Core.Transaction{output=Core.TransactionOutput{payouts}} -> payouts
  Contract4 -> mempty

contractNoToTransactions :: MarloweQueryTestData -> RefSym GetHeaders -> [Core.Transaction 'V1]
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
    , inputsAppliedToTransaction (returnDepositBlock contract3Step4) (returnDeposited contract3Step4)
    ]
  Contract4 -> mempty

txNoToSomeTransaction :: MarloweQueryTestData -> TxNo -> SomeTransaction
txNoToSomeTransaction testData txNo = SomeTransaction
  MarloweV1
  (txNoToInput testData txNo)
  (txNoToConsumer testData txNo)
  (inputsAppliedToTransaction (txNoToBlockHeader testData txNo) (txNoToInputsApplied testData txNo))

contract1Step5Withdrawal :: MarloweQueryTestData -> Withdrawal
contract1Step5Withdrawal MarloweQueryTestData{..} = Withdrawal
  { block = snd contract1Step5
  , withdrawnPayouts =
    case inputsAppliedToTransaction (returnDepositBlock contract1Step4) (returnDeposited contract1Step4) of
      Core.Transaction{output=Core.TransactionOutput{payouts}} -> flip Map.mapWithKey payouts \payout Payout{..} -> case datum of
        AssetId{..} -> PayoutRef
          { contractId = standardContractId contract1
          , payout
          , rolesCurrency = policyId
          , role = tokenName
          }
  , withdrawalTx = fromCardanoTxId $ getTxId $ fst contract1Step5
  }

runMarloweQueryIntegrationTest :: (MarloweQueryTestData -> MarloweQueryClient Integration a) -> ActionWith MarloweQueryTestData
runMarloweQueryIntegrationTest test testData@MarloweQueryTestData{..} =
  void $ runIntegrationTest (runMarloweQueryClient $ test testData) runtime
