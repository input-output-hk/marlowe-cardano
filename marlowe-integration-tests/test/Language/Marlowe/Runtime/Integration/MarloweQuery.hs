{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Integration.MarloweQuery where

import Cardano.Api (getTxId)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Foldable (Foldable (fold), foldl', for_)
import Data.Functor (void)
import Data.Kind (Type)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word8)
import Language.Marlowe.Protocol.BulkSync.Client (
  ClientStIdle (..),
  ClientStNext (..),
  ClientStPoll (..),
  MarloweBulkSyncClient (MarloweBulkSyncClient),
 )
import Language.Marlowe.Protocol.Query.Client (
  MarloweQueryClient,
  getContractHeaders,
  getContractState,
  getPayouts,
  getRoleCurrencies,
  getTransaction,
  getTransactions,
  getWithdrawal,
  getWithdrawals,
 )
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (Address, AssetId (..), BlockHeader, PolicyId, TxId, TxOutRef (..))
import Language.Marlowe.Runtime.Client (runMarloweBulkSyncClient, runMarloweQueryClient)
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweMetadataTag (..),
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.History.Api (MarloweBlock (..), MarloweWithdrawTransaction (..))
import Language.Marlowe.Runtime.Integration.Basic (
  contractCreatedToMarloweCreateTransaction,
  contractCreatedToUnspentContractOutput,
  inputsAppliedToMarloweApplyInputsTransaction,
  inputsAppliedToUnspentContractOutput,
 )
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract
import Language.Marlowe.Runtime.Transaction.Api (
  ContractCreated (..),
  ContractCreatedInEra (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  WalletAddresses (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
 )
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
  describe "GetPayouts" $ paginatedQuerySpec GetPayouts runMarloweQueryIntegrationTest
  bulkSyncTest
  getRoleCurrenciesSpec

data GetHeaders = GetHeaders
data GetWithdrawals = GetWithdrawals
data GetPayouts = GetPayouts

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
    , partyAddressesSym :: Set PartyAddress
    , partyRolesSym :: Set (RefSym GetHeaders)
    }
    deriving (Eq, Ord, Show)
  applyFilter _ ContractFilterSym{..} ref =
    (Set.null tagsSym || not (Set.null $ Set.intersection tagsSym $ tagsForContract ref))
      && (Set.null roleCurrenciesSym || Set.member ref roleCurrenciesSym)
      && case (Set.null partyAddressesSym, Set.null partyRolesSym) of
        (True, True) -> True
        (False, True) -> not (Set.null $ Set.intersection (partyAddressesForContract ref) partyAddressesSym)
        (True, False) -> Set.member ref partyRolesSym
        (False, False) ->
          not (Set.null $ Set.intersection (partyAddressesForContract ref) partyAddressesSym)
            || Set.member ref partyRolesSym
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
  toFilter _ testData@MarloweQueryTestData{..} ContractFilterSym{..} =
    ContractFilter
      { tags = testTagsToTags tagsSym
      , roleCurrencies = flip Set.map roleCurrenciesSym \case
          Contract1 -> standardContractRoleCurrency contract1
          Contract2 -> standardContractRoleCurrency contract2
          Contract3 -> standardContractRoleCurrency contract3
          Contract4 -> standardContractRoleCurrency contract4
      , partyAddresses = partyAddressesToAddresses testData partyAddressesSym
      , partyRoles =
          flip Set.map partyRolesSym $
            flip AssetId "Party A" . \case
              Contract1 -> standardContractRoleCurrency contract1
              Contract2 -> standardContractRoleCurrency contract2
              Contract3 -> standardContractRoleCurrency contract3
              Contract4 -> standardContractRoleCurrency contract4
      }
  enumerateFilters q =
    nub $
      defaultContractFilter
        : fold
          [ do
              tagsSym <- Set.toList $ Set.powerSet $ Set.fromList [minBound .. maxBound]
              filterVariations defaultContractFilter{tagsSym}
          , do
              roleCurrenciesSym <- Set.toList $ Set.powerSet $ Set.fromList $ allRefs q
              filterVariations defaultContractFilter{roleCurrenciesSym}
          , do
              partyAddressesSym <- Set.toList $ Set.powerSet $ Set.fromList [minBound .. maxBound]
              filterVariations defaultContractFilter{partyAddressesSym}
          , do
              partyRolesSym <- Set.toList $ Set.powerSet $ Set.fromList $ allRefs q
              filterVariations defaultContractFilter{partyRolesSym}
          ]
  runQuery _ = getContractHeaders

defaultContractFilter :: FilterSym GetHeaders
defaultContractFilter =
  ContractFilterSym
    { tagsSym = mempty
    , roleCurrenciesSym = mempty
    , partyAddressesSym = mempty
    , partyRolesSym = mempty
    }

data ContractFilterVariation
  = SpecifyTag
  | SpecifyRoleCurrency
  | SpecifyPartyAddress
  | SpecifyPartyRole
  deriving (Eq, Ord, Show, Enum, Bounded)

filterVariations :: FilterSym GetHeaders -> [FilterSym GetHeaders]
filterVariations filterSym = do
  variations <- Set.toList $ Set.powerSet $ Set.fromList $ filter (variationApplies filterSym) [minBound .. maxBound]
  pure $ applyVariations variations filterSym

variationApplies :: FilterSym GetHeaders -> ContractFilterVariation -> Bool
variationApplies ContractFilterSym{..} = \case
  SpecifyTag -> Set.null tagsSym
  SpecifyRoleCurrency -> Set.null roleCurrenciesSym
  SpecifyPartyAddress -> Set.null partyAddressesSym
  SpecifyPartyRole -> Set.null partyRolesSym

applyVariations :: Set ContractFilterVariation -> FilterSym GetHeaders -> FilterSym GetHeaders
applyVariations = flip $ foldl' applyVariation

applyVariation :: FilterSym GetHeaders -> ContractFilterVariation -> FilterSym GetHeaders
applyVariation filterSym = \case
  SpecifyTag -> filterSym{tagsSym = Set.singleton Tag1}
  SpecifyRoleCurrency -> filterSym{roleCurrenciesSym = Set.singleton Contract1}
  SpecifyPartyAddress -> filterSym{partyAddressesSym = Set.singleton Wallet1}
  SpecifyPartyRole -> filterSym{partyRolesSym = Set.singleton Contract1}

partyAddressesForContract :: RefSym GetHeaders -> Set PartyAddress
partyAddressesForContract = \case
  Contract1 -> Set.fromList [Wallet1, Wallet2]
  Contract2 -> Set.fromList [Wallet1, Wallet2]
  Contract3 -> Set.fromList [Wallet1, Wallet2]
  Contract4 -> Set.singleton Wallet2 -- wallet 1 is not a *visible* party address of contract 4 due to merkleization.

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
  applyFilter _ WithdrawalFilterSym{..} =
    (Set.null withdrawalRoleCurrenciesSym ||) . \case
      Withdrawal1 -> Set.member Contract1 withdrawalRoleCurrenciesSym
      Withdrawal2 -> Set.member Contract2 withdrawalRoleCurrenciesSym
  toRef _ MarloweQueryTestData{..} = \case
    Unknown -> "0000000000000000000000000000000000000000000000000000000000000000"
    Known Withdrawal1 -> case fst contract1Step5 of WithdrawTx _ WithdrawTxInEra{..} -> fromCardanoTxId $ getTxId txBody
    Known Withdrawal2 -> case fst contract2Step5 of WithdrawTx _ WithdrawTxInEra{..} -> fromCardanoTxId $ getTxId txBody
  toItem _ MarloweQueryTestData{..} ref = Withdrawal{..}
    where
      (withdrawalTx, block, withdrawnPayouts) = case ref of
        Withdrawal1 -> case contract1Step5 of
          (WithdrawTx _ WithdrawTxInEra{txBody = txBody', ..}, block') ->
            ( fromCardanoTxId $ getTxId txBody'
            , block'
            , flip Map.mapWithKey inputs \payoutId Payout{..} ->
                PayoutHeader
                  { contractId = standardContractId contract1
                  , withdrawalId = Just $ fromCardanoTxId $ getTxId txBody'
                  , payoutId
                  , role = datum
                  }
            )
        Withdrawal2 -> case contract2Step5 of
          (WithdrawTx _ WithdrawTxInEra{txBody = txBody', ..}, block') ->
            ( fromCardanoTxId $ getTxId txBody'
            , block'
            , flip Map.mapWithKey inputs \payoutId Payout{..} ->
                PayoutHeader
                  { contractId = standardContractId contract2
                  , withdrawalId = Just $ fromCardanoTxId $ getTxId txBody'
                  , payoutId
                  , role = datum
                  }
            )
  toFilter _ MarloweQueryTestData{..} WithdrawalFilterSym{..} =
    WithdrawalFilter
      { roleCurrencies = flip Set.map withdrawalRoleCurrenciesSym \case
          Contract1 -> standardContractRoleCurrency contract1
          Contract2 -> standardContractRoleCurrency contract2
          Contract3 -> standardContractRoleCurrency contract3
          Contract4 -> standardContractRoleCurrency contract4
      }
  enumerateFilters _ = WithdrawalFilterSym <$> Set.toList (Set.powerSet $ Set.fromList $ allRefs GetHeaders)
  runQuery _ = getWithdrawals

instance PaginatedQuery GetPayouts where
  type Filter GetPayouts = PayoutFilter
  type Ref GetPayouts = TxOutRef
  type Item GetPayouts = PayoutHeader
  data RefSym GetPayouts
    = Payout1
    | Payout2
    | Payout3
    deriving (Eq, Ord, Show, Enum, Bounded)
  data FilterSym GetPayouts = PayoutFilterSym
    { isWithdrawnSym :: Maybe Bool
    , roleTokensSym :: Set (RefSym GetHeaders)
    , contractIdsSym :: Set (RefSym GetHeaders)
    }
    deriving (Eq, Ord, Show)
  applyFilter _ PayoutFilterSym{..} ref =
    (Set.null roleTokensSym || Set.member (payoutContract ref) roleTokensSym)
      && (Set.null contractIdsSym || Set.member (payoutContract ref) contractIdsSym)
      && case isWithdrawnSym of
        Nothing -> True
        Just True -> ref == Payout1 || ref == Payout2
        Just False -> ref == Payout3
  toRef p testData = \case
    Unknown -> TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 1
    Known payout -> case toItem p testData payout of PayoutHeader{..} -> payoutId
  toItem _ MarloweQueryTestData{..} = \case
    Payout1 -> standardContractPayout contract1Step4 $ Just $ fst contract1Step5
    Payout2 -> standardContractPayout contract2Step4 $ Just $ fst contract2Step5
    Payout3 -> standardContractPayout contract3Step4 Nothing
  toFilter _ MarloweQueryTestData{..} PayoutFilterSym{..} =
    PayoutFilter
      { isWithdrawn = isWithdrawnSym
      , roleTokens = flip Set.map roleTokensSym \case
          Contract1 -> AssetId (standardContractRoleCurrency contract1) "Party A"
          Contract2 -> AssetId (standardContractRoleCurrency contract2) "Party A"
          Contract3 -> AssetId (standardContractRoleCurrency contract3) "Party A"
          Contract4 -> AssetId (standardContractRoleCurrency contract4) "Party A"
      , contractIds = flip Set.map contractIdsSym \case
          Contract1 -> standardContractId contract1
          Contract2 -> standardContractId contract2
          Contract3 -> standardContractId contract3
          Contract4 -> standardContractId contract4
      }
  enumerateFilters _ = do
    isWithdrawnSym <- [Nothing, Just True, Just False]
    roleTokensSym1 <- allRefs GetHeaders
    roleTokensSym2 <- allRefs GetHeaders
    guard $ roleTokensSym2 >= roleTokensSym1
    contractIdsSym1 <- allRefs GetHeaders
    contractIdsSym2 <- allRefs GetHeaders
    guard $ contractIdsSym2 >= contractIdsSym1
    let roleTokensSym = Set.fromList [roleTokensSym1, roleTokensSym2]
    let contractIdsSym = Set.fromList [contractIdsSym1, contractIdsSym2]
    pure PayoutFilterSym{..}
  runQuery _ = getPayouts

payoutContract :: RefSym GetPayouts -> RefSym GetHeaders
payoutContract = \case
  Payout1 -> Contract1
  Payout2 -> Contract2
  Payout3 -> Contract3

standardContractRoleCurrency :: StandardContractInit 'V1 -> PolicyId
standardContractRoleCurrency StandardContractInit{..} = case contractCreated of
  ContractCreated _ ContractCreatedInEra{..} -> rolesCurrency

data PartyAddress = Wallet1 | Wallet2
  deriving (Eq, Ord, Show, Enum, Bounded)

data TestTag = Tag1 | Tag2
  deriving (Eq, Ord, Show, Enum, Bounded)

partyAddressesToAddresses :: MarloweQueryTestData -> Set PartyAddress -> Set Address
partyAddressesToAddresses = Set.map . partyAddressToAddress

partyAddressToAddress :: MarloweQueryTestData -> PartyAddress -> Address
partyAddressToAddress MarloweQueryTestData{..} = \case
  Wallet1 -> changeAddress $ addresses wallet1
  Wallet2 -> changeAddress $ addresses wallet2

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
      let expected =
            SomeContractState MarloweV1 <$> case contractNo of
              Unknown -> Nothing
              Known contractNo' -> case contractNoToContractCreated testData contractNo' of
                ContractCreated _ ContractCreatedInEra{..} ->
                  Just
                    ContractState
                      { contractId
                      , roleTokenMintingPolicyId = rolesCurrency
                      , metadata
                      , initialBlock = contractNoToInitialBlock testData contractNo'
                      , initialOutput =
                          TransactionScriptOutput
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
      let expected =
            SomeTransactions MarloweV1 <$> case contractNo of
              Unknown -> Nothing
              Known contractNo' -> Just $ contractNoToTransactions testData contractNo'
      actual <- getTransactions $ toRef GetHeaders testData contractNo
      liftIO $ actual `shouldBe` expected

getTransactionSpec :: SpecWith MarloweQueryTestData
getTransactionSpec = describe "getTransaction" do
  for_ (Unknown : ((Known . Left <$> allRefs GetHeaders) <> (Known . Right <$> allTxNos))) \txNo -> do
    it (show txNo) $ runMarloweQueryIntegrationTest \testData -> do
      let expected = case txNo of
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
    actual <- getWithdrawal case fst contract1Step5 of WithdrawTx _ WithdrawTxInEra{..} -> fromCardanoTxId $ getTxId txBody
    liftIO $ actual `shouldBe` Just (contract1Step5Withdrawal testData)

getRoleCurrenciesSpec :: SpecWith MarloweQueryTestData
getRoleCurrenciesSpec = describe "getRoleCurrencies" do
  for_ allRoleCurrencyFilters \rcFilter ->
    it (show rcFilter) $ runMarloweQueryIntegrationTest \testData -> do
      let actualFilter = convertRoleCurrencyFilter testData rcFilter
      let expected = evalTestRoleCurrencyFilter testData rcFilter
      actual <- getRoleCurrencies actualFilter
      liftIO $ actual `shouldBe` expected

data TestRoleCurrencyFilter
  = TestAnd TestRoleCurrencyFilter TestRoleCurrencyFilter
  | TestOr TestRoleCurrencyFilter TestRoleCurrencyFilter
  | TestNot TestRoleCurrencyFilter
  | TestAny
  | TestNone
  | TestByContract (Set (WithUnknown (RefSym GetHeaders)))
  | TestByPolicy (Set (WithUnknown (RefSym GetHeaders)))
  deriving (Show)

allRoleCurrencyFilters :: [TestRoleCurrencyFilter]
allRoleCurrencyFilters = go (2 :: Int)
  where
    go depthBudget =
      TestAny
        : TestNone
        : [ TestByContract c
          | c <-
              (Set.singleton Unknown :) $
                Set.toList $
                  Set.powerSet $
                    Set.fromList $
                      Known <$> [minBound .. maxBound]
          ]
        ++ [ TestByPolicy c
           | c <-
              (Set.singleton Unknown :) $
                Set.toList $
                  Set.powerSet $
                    Set.fromList $
                      Known <$> [minBound .. maxBound]
           ]
        ++ if depthBudget > 0
          then do
            let nextLayer = go $ depthBudget - 1
            let firstInNextLayer = head nextLayer
            (TestAnd <$> nextLayer <*> pure firstInNextLayer)
              ++ (TestAnd firstInNextLayer <$> nextLayer)
              ++ (TestOr <$> nextLayer <*> pure firstInNextLayer)
              ++ (TestOr firstInNextLayer <$> nextLayer)
              ++ (TestNot <$> nextLayer)
          else []

evalTestRoleCurrencyFilter :: MarloweQueryTestData -> TestRoleCurrencyFilter -> Set RoleCurrency
evalTestRoleCurrencyFilter MarloweQueryTestData{..} = go
  where
    go = \case
      TestAnd f g -> go f `Set.intersection` go g
      TestOr f g -> go f <> go g
      TestNot f -> go TestAny `Set.difference` go f
      TestAny -> Set.fromList $ Map.elems roleCurrenciesByContract
      TestNone -> mempty
      TestByContract contracts -> Set.fromList $ Map.elems $ Map.restrictKeys roleCurrenciesByContract contracts
      TestByPolicy policies -> Set.fromList $ Map.elems $ Map.restrictKeys roleCurrenciesByContract policies
    roleCurrenciesByContract =
      Map.fromList
        [ (Known Contract1, standardContractRoleCurrency' contract1 False)
        , (Known Contract2, standardContractRoleCurrency' contract2 False)
        , (Known Contract3, standardContractRoleCurrency' contract3 False)
        , (Known Contract4, standardContractRoleCurrency' contract4 True)
        ]

standardContractRoleCurrency' :: StandardContractInit 'V1 -> Bool -> RoleCurrency
standardContractRoleCurrency' contract active =
  RoleCurrency
    { rolePolicyId = standardContractRoleCurrency contract
    , roleContract = standardContractId contract
    , ..
    }

convertRoleCurrencyFilter :: MarloweQueryTestData -> TestRoleCurrencyFilter -> RoleCurrencyFilter
convertRoleCurrencyFilter MarloweQueryTestData{..} = go
  where
    go = \case
      TestAnd f g -> RoleCurrencyAnd (go f) (go g)
      TestOr f g -> RoleCurrencyOr (go f) (go g)
      TestNot f -> RoleCurrencyNot $ go f
      TestAny -> RoleCurrencyFilterAny
      TestNone -> RoleCurrencyFilterNone
      TestByContract contracts -> RoleCurrencyFilterByContract $ Set.map convertContractId contracts
      TestByPolicy policies -> RoleCurrencyFilterByPolicy $ Set.map convertPolicyId policies

    convertContractId Unknown =
      ContractId $ TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" 1
    convertContractId (Known Contract1) = standardContractId contract1
    convertContractId (Known Contract2) = standardContractId contract2
    convertContractId (Known Contract3) = standardContractId contract3
    convertContractId (Known Contract4) = standardContractId contract4

    convertPolicyId Unknown = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    convertPolicyId (Known Contract1) = standardContractRoleCurrency contract1
    convertPolicyId (Known Contract2) = standardContractRoleCurrency contract2
    convertPolicyId (Known Contract3) = standardContractRoleCurrency contract3
    convertPolicyId (Known Contract4) = standardContractRoleCurrency contract4

setup :: ActionWith MarloweQueryTestData -> IO ()
setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- ask
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  contract1 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract1) wallet1 wallet2
  contract1Step1 <- makeInitialDeposit contract1
  contract1Step2 <- chooseGimmeTheMoney contract1Step1
  contract2 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract2) wallet2 wallet1
  contract1Step3 <- sendNotify contract1Step2
  contract1Step4 <- makeReturnDeposit contract1Step3
  contract1Step5 <- withdrawPartyAFunds contract1Step4
  contract2Step1 <- makeInitialDeposit contract2
  contract3 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract3) wallet1 wallet2
  contract4 <- createStandardContractWithTags (testTagsToTags $ tagsForContract Contract4) wallet2 wallet1
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
  , contract1Step5 :: (WithdrawTx 'V1, BlockHeader)
  , contract2 :: StandardContractInit 'V1
  , contract2Step1 :: StandardContractFundsDeposited 'V1
  , contract2Step2 :: StandardContractChoiceMade 'V1
  , contract2Step3 :: StandardContractNotified 'V1
  , contract2Step4 :: StandardContractClosed 'V1
  , contract2Step5 :: (WithdrawTx 'V1, BlockHeader)
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
  type Filter q :: Type
  type Ref q :: Type
  type Item q :: Type
  data FilterSym q :: Type
  data RefSym q :: Type
  applyFilter :: q -> FilterSym q -> RefSym q -> Bool
  toRef :: q -> MarloweQueryTestData -> WithUnknown (RefSym q) -> Ref q
  toItem :: q -> MarloweQueryTestData -> RefSym q -> Item q
  toFilter :: q -> MarloweQueryTestData -> FilterSym q -> Filter q
  enumerateFilters :: q -> [FilterSym q]
  runQuery :: (Applicative m) => q -> Filter q -> Range (Ref q) -> MarloweQueryClient m (Maybe (Page (Ref q) (Item q)))

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
        it
          ( show filter_
              <> "; pageSize = "
              <> show (rangeLimit range)
              <> "; start = "
              <> show (rangeStart range)
              <> "; order = "
              <> show (rangeDirection range)
              <> " => "
              <> show expectedSym
          )
          $ runner \testData -> do
            actual <- appendRange q testData filter_ [] $ toRef q testData <$> range
            let expected = toItem q testData <$> expectedSym
            liftIO $ actual `shouldBe` Just (length expected, expected)

isValidStartingRange :: (PaginatedQuery q) => q -> FilterSym q -> Range (WithUnknown (RefSym q)) -> Bool
isValidStartingRange q filter_ Range{..} = case getFilteredSortedRefs q filter_ Range{..} of
  [] -> False
  a : _ ->
    (isNothing rangeStart || rangeStart == Just (Known a))
      && rangeOffset == 0
      && rangeLimit > 0

appendRange
  :: (Monad m)
  => (PaginatedQuery q)
  => q
  -> MarloweQueryTestData
  -> FilterSym q
  -> [Item q]
  -> Range (Ref q)
  -> MarloweQueryClient m (Maybe (Int, [Item q]))
appendRange q testData filter_ prev range =
  runQuery q (toFilter q testData filter_) range >>= \case
    Nothing -> pure Nothing
    Just Page{..} -> case nextRange of
      Nothing -> pure $ Just (totalCount, prev <> items)
      Just range' -> appendRange q testData filter_ (prev <> items) range'

allRefs :: (PaginatedQuery q) => q -> [RefSym q]
allRefs _ = [minBound .. maxBound]

getFilteredSortedRefs :: (PaginatedQuery q) => q -> FilterSym q -> Range a -> [RefSym q]
getFilteredSortedRefs q filter_ Range{..} = case rangeDirection of
  Ascending -> filter (applyFilter q filter_) $ allRefs q
  Descending -> reverse $ filter (applyFilter q filter_) $ allRefs q

getFilteredRefs :: (PaginatedQuery q) => q -> FilterSym q -> [RefSym q]
getFilteredRefs q filter_ = filter (applyFilter q filter_) $ allRefs q

allRanges :: (PaginatedQuery q) => q -> FilterSym q -> [Range (WithUnknown (RefSym q))]
allRanges q filter_ =
  fold
    [ allValidRanges
    , badBoundsRanges
    , notFoundRanges
    ]
  where
    allValidRanges = do
      let filteredRefs = getFilteredRefs q filter_
      rangeStart <- Nothing : (Just . Known <$> allRefs q)
      rangeOffset <- [0 .. (length filteredRefs)]
      rangeLimit <- [1 .. (length filteredRefs)]
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
  :: (PaginatedQuery q)
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
  pure
    Page
      { items = take rangeLimit itemsWithNext
      , nextRange = case drop rangeLimit itemsWithNext of
          [] -> Nothing
          a : _ ->
            Just $
              Range
                { rangeStart = Just a
                , rangeOffset = 0
                , ..
                }
      , totalCount = length sortedRefs
      }

from :: (Eq a) => a -> [a] -> Maybe [a]
from _ [] = Nothing
from start (a : as)
  | a == start = Just $ a : as
  | otherwise = from start as

allTxNos :: [TxNo]
allTxNos = [minBound .. maxBound]

contractOrTxNoToTxId :: MarloweQueryTestData -> WithUnknown (Either (RefSym GetHeaders) TxNo) -> TxId
contractOrTxNoToTxId testData = \case
  Unknown -> "0000000000000000000000000000000000000000000000000000000000000000"
  Known (Left contractNo) -> case toRef GetHeaders testData $ Known contractNo of
    ContractId TxOutRef{..} -> txId
  Known (Right txNo) -> txNoToTxId testData txNo

txNoToInputsApplied :: MarloweQueryTestData -> TxNo -> InputsApplied 'V1
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
  Contract1Step2 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ initialFundsDeposited contract1Step1
  Contract1Step3 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ gimmeTheMoneyChosen contract1Step2
  Contract1Step4 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ notified contract1Step3
  Contract2Step1 -> unContractId $ standardContractId contract2
  Contract2Step2 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ initialFundsDeposited contract2Step1
  Contract2Step3 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ gimmeTheMoneyChosen contract2Step2
  Contract2Step4 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ notified contract2Step3
  Contract3Step1 -> unContractId $ standardContractId contract3
  Contract3Step2 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ initialFundsDeposited contract3Step1
  Contract3Step3 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ gimmeTheMoneyChosen contract3Step2
  Contract3Step4 -> utxo $ fromJust $ scriptOutput $ inputsAppliedOutput $ notified contract3Step3

txNoToInputDatum :: MarloweQueryTestData -> TxNo -> Core.Datum 'V1
txNoToInputDatum MarloweQueryTestData{..} = \case
  Contract1Step1 -> standardContractDatum contract1
  Contract1Step2 -> inputsAppliedInputDatum $ gimmeTheMoneyChosen contract1Step2
  Contract1Step3 -> inputsAppliedInputDatum $ notified contract1Step3
  Contract1Step4 -> inputsAppliedInputDatum $ returnDeposited contract1Step4
  Contract2Step1 -> standardContractDatum contract2
  Contract2Step2 -> inputsAppliedInputDatum $ gimmeTheMoneyChosen contract2Step2
  Contract2Step3 -> inputsAppliedInputDatum $ notified contract2Step3
  Contract2Step4 -> inputsAppliedInputDatum $ returnDeposited contract2Step4
  Contract3Step1 -> standardContractDatum contract3
  Contract3Step2 -> inputsAppliedInputDatum $ gimmeTheMoneyChosen contract3Step2
  Contract3Step3 -> inputsAppliedInputDatum $ notified contract3Step3
  Contract3Step4 -> inputsAppliedInputDatum $ returnDeposited contract3Step4

inputsAppliedOutput :: InputsApplied v -> TransactionOutput v
inputsAppliedOutput (InputsApplied _ InputsAppliedInEra{..}) = output

inputsAppliedInputDatum :: InputsApplied v -> Core.Datum v
inputsAppliedInputDatum (InputsApplied _ InputsAppliedInEra{..}) = case input of
  Core.TransactionScriptOutput{..} -> datum

txNoToConsumer :: MarloweQueryTestData -> TxNo -> Maybe TxId
txNoToConsumer MarloweQueryTestData{..} =
  fmap inputsAppliedTxId . \case
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

inputsAppliedTxId :: InputsApplied v -> TxId
inputsAppliedTxId (InputsApplied _ InputsAppliedInEra{..}) = fromCardanoTxId $ getTxId txBody

contractNoToStandardContract :: MarloweQueryTestData -> RefSym GetHeaders -> StandardContractInit 'V1
contractNoToStandardContract MarloweQueryTestData{..} = \case
  Contract1 -> contract1
  Contract2 -> contract2
  Contract3 -> contract3
  Contract4 -> contract4

contractNoToContractCreated :: MarloweQueryTestData -> RefSym GetHeaders -> ContractCreated 'V1
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
  Contract2 -> scriptOutput $ inputsAppliedOutput $ returnDeposited contract2Step4
  Contract3 -> scriptOutput $ inputsAppliedOutput $ returnDeposited contract3Step4
  Contract4 -> case contractCreated contract4 of
    ContractCreated _ ContractCreatedInEra{..} ->
      Just
        TransactionScriptOutput
          { address = marloweScriptAddress
          , assets
          , utxo = unContractId contractId
          , datum
          }

contractNoToUnclaimedPayouts :: MarloweQueryTestData -> RefSym GetHeaders -> Map TxOutRef (Payout 'V1)
contractNoToUnclaimedPayouts MarloweQueryTestData{..} = \case
  Contract1 -> mempty
  Contract2 -> mempty
  Contract3 -> case inputsAppliedToTransaction (returnDepositBlock contract3Step4) (returnDeposited contract3Step4) of
    Core.Transaction{output = Core.TransactionOutput{payouts}} -> payouts
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
txNoToSomeTransaction testData txNo =
  SomeTransaction
    MarloweV1
    (txNoToInput testData txNo)
    (txNoToInputDatum testData txNo)
    (txNoToConsumer testData txNo)
    (inputsAppliedToTransaction (txNoToBlockHeader testData txNo) (txNoToInputsApplied testData txNo))

contract1Step5Withdrawal :: MarloweQueryTestData -> Withdrawal
contract1Step5Withdrawal MarloweQueryTestData{..} =
  Withdrawal
    { block = snd contract1Step5
    , withdrawnPayouts = case fst contract1Step5 of
        WithdrawTx _ WithdrawTxInEra{..} -> flip Map.mapWithKey inputs \payoutId Payout{..} ->
          PayoutHeader
            { contractId = standardContractId contract1
            , withdrawalId = Just $ fromCardanoTxId $ getTxId txBody
            , payoutId
            , role = datum
            }
    , withdrawalTx = case fst contract1Step5 of WithdrawTx _ WithdrawTxInEra{txBody} -> fromCardanoTxId $ getTxId txBody
    }

runMarloweQueryIntegrationTest
  :: (MarloweQueryTestData -> MarloweQueryClient Integration a) -> ActionWith MarloweQueryTestData
runMarloweQueryIntegrationTest test testData@MarloweQueryTestData{..} =
  void $ runIntegrationTest (runMarloweQueryClient $ test testData) runtime

bulkSyncTest :: SpecWith MarloweQueryTestData
bulkSyncTest = describe "MarloweBulkSync" do
  for_ [0 .. 4] \batchSize ->
    it ("Returns all marlowe blocks when fetching " <> show (batchSize + 1) <> " blocks at a time") do
      runMarloweQueryIntegrationTest \MarloweQueryTestData{..} -> do
        actual <- lift $ getMarloweBlocksFromBulkSync batchSize
        let expected =
              [ let blockHeader = createdBlock contract1
                 in MarloweBlock
                      { blockHeader
                      , createTransactions =
                          [ contractCreatedToMarloweCreateTransaction $ contractCreated contract1
                          ]
                      , applyInputsTransactions = []
                      , withdrawTransactions = []
                      }
              , let blockHeader = initialDepositBlock contract1Step1
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              (contractCreatedToUnspentContractOutput $ contractCreated contract1)
                              (initialFundsDeposited contract1Step1)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = choiceBlock contract1Step2
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract1)
                                  (initialFundsDeposited contract1Step1)
                              )
                              (gimmeTheMoneyChosen contract1Step2)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = createdBlock contract2
                 in MarloweBlock
                      { blockHeader
                      , createTransactions =
                          [ contractCreatedToMarloweCreateTransaction $ contractCreated contract2
                          ]
                      , applyInputsTransactions = []
                      , withdrawTransactions = []
                      }
              , let blockHeader = notifiedBlock contract1Step3
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract1)
                                  (gimmeTheMoneyChosen contract1Step2)
                              )
                              (notified contract1Step3)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = returnDepositBlock contract1Step4
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract1)
                                  (notified contract1Step3)
                              )
                              (returnDeposited contract1Step4)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = snd contract1Step5
                 in case fst contract1Step5 of
                      WithdrawTx _ WithdrawTxInEra{txBody} ->
                        MarloweBlock
                          { blockHeader
                          , createTransactions = []
                          , applyInputsTransactions = []
                          , withdrawTransactions =
                              [ MarloweWithdrawTransaction
                                  { consumingTx = fromCardanoTxId $ getTxId txBody
                                  , consumedPayouts = Map.singleton (standardContractId contract1) case returnDeposited contract1Step4 of
                                      InputsApplied _ InputsAppliedInEra{output = TransactionOutput{payouts}} ->
                                        Map.keysSet payouts
                                  }
                              ]
                          }
              , let blockHeader = initialDepositBlock contract2Step1
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              (contractCreatedToUnspentContractOutput $ contractCreated contract2)
                              (initialFundsDeposited contract2Step1)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = createdBlock contract3
                 in MarloweBlock
                      { blockHeader
                      , createTransactions =
                          [ contractCreatedToMarloweCreateTransaction $ contractCreated contract3
                          ]
                      , applyInputsTransactions = []
                      , withdrawTransactions = []
                      }
              , let blockHeader = createdBlock contract4
                 in MarloweBlock
                      { blockHeader
                      , createTransactions =
                          [ contractCreatedToMarloweCreateTransaction $ contractCreated contract4
                          ]
                      , applyInputsTransactions = []
                      , withdrawTransactions = []
                      }
              , let blockHeader = choiceBlock contract2Step2
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract2)
                                  (initialFundsDeposited contract2Step1)
                              )
                              (gimmeTheMoneyChosen contract2Step2)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = notifiedBlock contract2Step3
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract2)
                                  (gimmeTheMoneyChosen contract2Step2)
                              )
                              (notified contract2Step3)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = returnDepositBlock contract2Step4
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract2)
                                  (notified contract2Step3)
                              )
                              (returnDeposited contract2Step4)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = snd contract2Step5
                 in case fst contract2Step5 of
                      WithdrawTx _ WithdrawTxInEra{txBody} ->
                        MarloweBlock
                          { blockHeader
                          , createTransactions = []
                          , applyInputsTransactions = []
                          , withdrawTransactions =
                              [ MarloweWithdrawTransaction
                                  { consumingTx = fromCardanoTxId $ getTxId txBody
                                  , consumedPayouts = Map.singleton (standardContractId contract2) case returnDeposited contract2Step4 of
                                      InputsApplied _ InputsAppliedInEra{output = TransactionOutput{payouts}} ->
                                        Map.keysSet payouts
                                  }
                              ]
                          }
              , let blockHeader = initialDepositBlock contract3Step1
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              (contractCreatedToUnspentContractOutput $ contractCreated contract3)
                              (initialFundsDeposited contract3Step1)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = choiceBlock contract3Step2
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract3)
                                  (initialFundsDeposited contract3Step1)
                              )
                              (gimmeTheMoneyChosen contract3Step2)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = notifiedBlock contract3Step3
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract3)
                                  (gimmeTheMoneyChosen contract3Step2)
                              )
                              (notified contract3Step3)
                          ]
                      , withdrawTransactions = []
                      }
              , let blockHeader = returnDepositBlock contract3Step4
                 in MarloweBlock
                      { blockHeader
                      , createTransactions = []
                      , applyInputsTransactions =
                          [ inputsAppliedToMarloweApplyInputsTransaction
                              blockHeader
                              ( inputsAppliedToUnspentContractOutput
                                  (contractCreated contract3)
                                  (notified contract3Step3)
                              )
                              (returnDeposited contract3Step4)
                          ]
                      , withdrawTransactions = []
                      }
              ]
        liftIO $ actual `shouldBe` expected

getMarloweBlocksFromBulkSync :: Word8 -> Integration [MarloweBlock]
getMarloweBlocksFromBulkSync batchSize = runMarloweBulkSyncClient $ MarloweBulkSyncClient $ pure $ idle []
  where
    idle acc =
      SendMsgRequestNext
        batchSize
        ClientStNext
          { recvMsgRollForward = \blocks _ -> pure $ idle $ acc <> blocks
          , recvMsgRollBackward = \_ _ -> fail "Unexpected rollback"
          , recvMsgWait = pure $ SendMsgCancel $ SendMsgDone acc
          }
