{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Marlowe.Runtime.Integration.Create
  where

import Cardano.Api
  ( BabbageEra
  , CardanoEra(..)
  , Script(..)
  , SimpleScript(..)
  , SimpleScriptVersion(..)
  , SlotNo(..)
  , TimeLocksSupported(..)
  , TxBody(..)
  , TxBodyContent(..)
  , TxInsCollateral(..)
  , TxMintValue(..)
  , TxOut(..)
  , TxOutValue(..)
  , scriptPolicyId
  , valueToList
  )
import qualified Cardano.Api as C
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api
  (assetsFromCardanoValue, fromCardanoAddressInEra, fromCardanoPolicyId, fromCardanoTxIn, fromCardanoTxOutValue)
import Language.Marlowe.Runtime.ChainSync.Api
  ( AssetId(..)
  , Assets(..)
  , Lovelace
  , Metadata(..)
  , PolicyId(..)
  , StakeCredential
  , StakeReference(..)
  , Tokens(..)
  , TransactionMetadata(..)
  , fromCardanoStakeCredential
  , stakeReference
  )
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Integration.Common
  (Wallet(..), allocateWallet, getGenesisWallet, getStakeCredential, runIntegrationTest, submitBuilder)
import Language.Marlowe.Runtime.Transaction.Api
import Network.Protocol.Codec.Spec (varyAp)
import Network.Protocol.Job.Client (liftCommand)
import Network.URI (parseURI)
import Test.Hspec
import Test.Integration.Marlowe (MarloweRuntime(..), withLocalMarloweRuntime)

spec :: Spec
spec = describe "Create" $ aroundAll setup do
  parallel $ for_ allCreateCases \createCase -> aroundAllWith (runCreateCase createCase) do
    describe (show createCase) do
      case mkSpec createCase of
        Nothing -> it "Should fail" \case
          (_, Left _) -> pure ()
          (_, Right contractCreated) -> expectationFailure $ "Expected a failure, but it succeeded: " <> show contractCreated
        Just s -> aroundAllWith expectSuccess s

expectSuccess :: Show e => ActionWith (a, b) -> ActionWith (a, Either e b)
expectSuccess action = \case
  (_, Left err) -> expectationFailure $ "Expected success, but it failed: " <> show err
  (a, Right b) -> action (a, b)

runCreateCase :: CreateCase -> ActionWith (TestData, Either (CreateError 'V1) (ContractCreated BabbageEra 'V1)) -> ActionWith TestData
runCreateCase createCase action testData = flip runIntegrationTest (runtime testData) do
  result <- runMarloweTxClient $ liftCommand $ mkCreateCommand testData createCase
  liftIO $ action (testData, result)

data TestData = TestData
  { stakeCredential :: StakeCredential
  , singleAddressInsufficientBalanceWallet :: WalletAddresses
  , singleAddressSufficientBalanceOneInsufficientCollateralWallet :: WalletAddresses
  , singleAddressSufficientBalanceMultiInsufficientCollateralWallet :: WalletAddresses
  , singleAddressSufficientBalanceOneSufficientCollateralWallet :: WalletAddresses
  , singleAddressSufficientBalanceMultiSufficientCollateralWallet :: WalletAddresses
  , multiAddressInsufficientBalanceWallet :: WalletAddresses
  , multiAddressSufficientBalanceOneInsufficientCollateralWallet :: WalletAddresses
  , multiAddressSufficientBalanceMultiInsufficientCollateralWallet :: WalletAddresses
  , multiAddressSufficientBalanceOneSufficientCollateralWallet :: WalletAddresses
  , multiAddressSufficientBalanceMultiSufficientCollateralWallet :: WalletAddresses
  , existingRoleTokenPolicy :: PolicyId
  , runtime :: MarloweRuntime
  }

setup :: ActionWith TestData -> IO ()
setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- ask
  genesisWallet0 <- getGenesisWallet 0
  genesisWallet1 <- getGenesisWallet 1
  stakeCredential <- getStakeCredential 0
  liftIO . runSpec . snd =<< submitBuilder (genesisWallet0 <> genesisWallet1) do
    singleAddressInsufficientBalanceWallet <- fmap addresses $ allocateWallet [[(False, 857690)]]
    singleAddressSufficientBalanceOneInsufficientCollateralWallet <- fmap addresses $ allocateWallet [[(True, 857690), (False, 12_000_000)]]
    singleAddressSufficientBalanceMultiInsufficientCollateralWallet <- fmap addresses $ allocateWallet [[(True, 857690), (True, 857690), (False, 12_000_000)]]
    singleAddressSufficientBalanceOneSufficientCollateralWallet <- fmap addresses $ allocateWallet [[(True, 12_000_000)]]
    singleAddressSufficientBalanceMultiSufficientCollateralWallet <- fmap addresses $ allocateWallet [[(True, 1_500_000), (True, 2_000_000), (False, 10_000_000)]]
    multiAddressInsufficientBalanceWallet <- fmap addresses $ allocateWallet [[(False, 857690)], [], [(False, 1_000_000)]]
    multiAddressSufficientBalanceOneInsufficientCollateralWallet <- fmap addresses $ allocateWallet [[(True, 857690), (False, 1_000_000)], [(False, 1_500_000), (False, 10_000_000)]]
    multiAddressSufficientBalanceMultiInsufficientCollateralWallet <- fmap addresses $ allocateWallet [[(True, 857690), (True, 857690)], [(False, 11_000_000)]]
    multiAddressSufficientBalanceOneSufficientCollateralWallet <- fmap addresses $ allocateWallet [[(False, 5_000_000)], [(True, 2_000_000), (False, 10_000_000)]]
    multiAddressSufficientBalanceMultiSufficientCollateralWallet <- fmap addresses $ allocateWallet [[(True, 1_000_000), (True, 2_000_000)], [], [(False, 10_000_000)]]
    pure TestData
      { stakeCredential = fromCardanoStakeCredential stakeCredential
      , singleAddressInsufficientBalanceWallet
      , singleAddressSufficientBalanceOneInsufficientCollateralWallet
      , singleAddressSufficientBalanceMultiInsufficientCollateralWallet
      , singleAddressSufficientBalanceOneSufficientCollateralWallet
      , singleAddressSufficientBalanceMultiSufficientCollateralWallet
      , multiAddressInsufficientBalanceWallet
      , multiAddressSufficientBalanceOneInsufficientCollateralWallet
      , multiAddressSufficientBalanceMultiInsufficientCollateralWallet
      , multiAddressSufficientBalanceOneSufficientCollateralWallet
      , multiAddressSufficientBalanceMultiSufficientCollateralWallet
      , existingRoleTokenPolicy = fromCardanoPolicyId $ scriptPolicyId $ SimpleScript SimpleScriptV2 $ RequireTimeAfter TimeLocksInSimpleScriptV2 $ SlotNo 0
      , runtime
      }

mkSpec :: CreateCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra 'V1))
mkSpec (CreateCase stakeCredential wallet (roleTokens, metadata) minLovelace) = mergeSpecs
  [ stakeCredentialsSpec stakeCredential
  , walletSpec roleTokens wallet
  , roleTokenSpec roleTokens
  , metadataSpec roleTokens metadata
  , minLovelaceSpec minLovelace
  ]

mergeSpecs
  :: [Maybe (SpecWith (TestData, ContractCreated BabbageEra v))]
  -> Maybe (SpecWith (TestData, ContractCreated BabbageEra v))
mergeSpecs = foldr (liftA2 (*>)) $ Just $ pure ()

stakeCredentialsSpec :: StakeCredentialCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra v))
stakeCredentialsSpec = Just . \case
  UseStakeCredential -> do
    it "Attaches a stake credential to the Marlowe script address" \(TestData{..}, ContractCreated{..}) -> do
      stakeReference marloweScriptAddress `shouldBe` Just (StakeCredential stakeCredential)
  NoStakeCredential -> do
    it "Does not attach a stake credential to the Marlowe script address" \(_, ContractCreated{..}) -> do
      stakeReference marloweScriptAddress `shouldBe` Nothing

walletSpec :: RoleTokenCase -> WalletCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra v))
walletSpec roleTokens (WalletCase balance addresses collateral) = mergeSpecs
  [ balanceSpec balance
  , collateralSpec roleTokens addresses collateral
  ]

balanceSpec :: WalletBalanceCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra v))
balanceSpec = \case
  BalanceSufficient -> Just $ pure ()
  BalanceInsufficient -> Nothing

collateralSpec :: RoleTokenCase -> WalletAddressesCase -> WalletCollateralUTxOCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra v))
collateralSpec NoRoleTokens _ = const $ Just $ pure ()
collateralSpec ExistingPolicyRoleTokens _ = const $ Just $ pure ()
collateralSpec _ addresses = \case
  NoCollateralUTxOs -> Just $ pure ()
  OneCollateralUTxOInsufficient -> Just do
    it "Should fail if there is insufficient collateral" $ const pending
  OneCollateralUTxOSufficient -> Just do
    it "Should only spend the one collateral UTxO" \(TestData{..}, ContractCreated{..}) -> do
      let
        collateral = case txBody of
          TxBody TxBodyContent{..} -> case txInsCollateral of
            TxInsCollateralNone -> mempty
            TxInsCollateral _ collateralTxIns -> Set.fromList $ fromCardanoTxIn <$> collateralTxIns
      collateral `shouldBe` collateralUtxos case addresses of
        SingleAddress -> singleAddressSufficientBalanceOneSufficientCollateralWallet
        MultiAddress -> multiAddressSufficientBalanceOneSufficientCollateralWallet
  MultipleCollateralUTxOsInsufficient -> Just do
    it "Should fail if there is insufficient collateral" $ const pending
  MultipleCollateralUTxOsSufficient -> Just do
    it "Should only spend the one collateral UTxO" \(TestData{..}, ContractCreated{..}) -> do
      let
        collateral = case txBody of
          TxBody TxBodyContent{..} -> case txInsCollateral of
            TxInsCollateralNone -> mempty
            TxInsCollateral _ collateralTxIns -> Set.fromList $ fromCardanoTxIn <$> collateralTxIns
        availableCollateral = collateralUtxos case addresses of
          SingleAddress -> singleAddressSufficientBalanceMultiSufficientCollateralWallet
          MultiAddress -> multiAddressSufficientBalanceMultiSufficientCollateralWallet
      Set.toList availableCollateral `shouldContain` Set.toList collateral

roleTokenSpec :: RoleTokenCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra v))
roleTokenSpec = \case
  NoRoleTokens -> Just do
    it "Should use ADA as the role token currency" \(_, ContractCreated{..}) -> do
      rolesCurrency `shouldBe` ""
    it "Should not mint any tokens" \(_, ContractCreated{..}) -> do
      let
        mintValue = case txBody of
          TxBody TxBodyContent{..} -> txMintValue
      mintValue `shouldBe` TxMintNone
    it "Should not output any non-ada tokens" \(_, ContractCreated{..}) -> do
      let
        tokensOutput = case txBody of
          TxBody TxBodyContent{..} -> txOuts & foldMap \(TxOut _ value _ _) -> case value of
            TxOutAdaOnly era _ -> case era of
            TxOutValue _ value' -> Set.fromList $ fst <$> valueToList value'
      tokensOutput `shouldBe` Set.singleton C.AdaAssetId
  ExistingPolicyRoleTokens -> Just do
    it "Should use the given policyId as the role token currency" \(TestData{..}, ContractCreated{..}) -> do
      rolesCurrency `shouldBe` existingRoleTokenPolicy
    it "Should not mint any tokens" \(_, ContractCreated{..}) -> do
      let
        mintValue = case txBody of
          TxBody TxBodyContent{..} -> txMintValue
      mintValue `shouldBe` TxMintNone
    it "Should not output any non-ada tokens" \(_, ContractCreated{..}) -> do
      let
        tokensOutput = case txBody of
          TxBody TxBodyContent{..} -> txOuts & foldMap \(TxOut _ value _ _) -> case value of
            TxOutAdaOnly era _ -> case era of
            TxOutValue _ value' -> Set.fromList $ fst <$> valueToList value'
      tokensOutput `shouldBe` Set.singleton C.AdaAssetId
  -- Metadata checks done with other metadata checks.
  _ -> Just do
    it "Should mint the required tokens" \(_, ContractCreated{..}) -> do
      let
        mintedTokenNames = case txBody of
          TxBody TxBodyContent{..} -> case txMintValue of
            TxMintNone -> mempty
            TxMintValue _ value _  -> Map.keysSet $ unTokens $ tokens $ assetsFromCardanoValue value
      mintedTokenNames `shouldBe` Set.singleton (AssetId rolesCurrency "Role")
    it "Should distribute the role tokens" \(TestData{..}, ContractCreated{..}) -> do
      let
        tokenDistribution = case txBody of
          TxBody TxBodyContent{..} -> Map.unionsWith (+) $ txOuts <&> \(TxOut address value _ _) -> Map.fromList
            $ fmap (\(assetId, quantity) -> ((fromCardanoAddressInEra BabbageEra address, assetId), quantity))
            $ Map.toList
            $ unTokens
            $ tokens
            $ fromCardanoTxOutValue value
      tokenDistribution `shouldBe` Map.singleton (changeAddress singleAddressInsufficientBalanceWallet, AssetId rolesCurrency "Role") 1

metadataSpec :: RoleTokenCase -> MetadataCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra 'V1))
metadataSpec roleTokens metadataCase = Just do
  it "Should write the expected metadata" \(_, contract@ContractCreated{..}) -> do
    metadata `shouldBe` addNFTMetadata contract roleTokens (expectedMetadata metadataCase)

expectedMetadata :: MetadataCase -> MarloweTransactionMetadata
expectedMetadata (MetadataCase marloweMetadata extraMetadata) = MarloweTransactionMetadata
  (mkMarloweMetadata marloweMetadata)
  case marloweMetadata of
    NoMarloweMetadata -> mkExtraMetadata extraMetadata
    EmptyTags -> TransactionMetadata $ Map.insert 1564 (encodeMarloweMetadata $ MarloweMetadata mempty Nothing) $ unTransactionMetadata $ mkExtraMetadata extraMetadata
    NonEmptyTags -> TransactionMetadata $ Map.insert 1564 (encodeMarloweMetadata testMarloweMetadata) $ unTransactionMetadata $ mkExtraMetadata extraMetadata

addNFTMetadata :: ContractCreated BabbageEra 'V1 -> RoleTokenCase -> MarloweTransactionMetadata -> MarloweTransactionMetadata
addNFTMetadata ContractCreated{..} = \case
  MintRoleTokensMetadata -> \MarloweTransactionMetadata{..} ->
    MarloweTransactionMetadata
      { transactionMetadata = TransactionMetadata
          $ Map.insert 721
            ( MetadataMap
              [ ( MetadataBytes $ unPolicyId rolesCurrency
                , MetadataMap [(MetadataBytes "Role", encodeRoleTokenMetadata testNftMetadata)]
                )
              ]
            )
          $ unTransactionMetadata transactionMetadata
      , ..
      }
  _ -> id

minLovelaceSpec :: MinLovelaceCase -> Maybe (SpecWith (TestData, ContractCreated BabbageEra v))
minLovelaceSpec = \case
  MinLovelaceSufficient -> Just $ pure ()
  MinLovelaceInsufficient -> Nothing

mkCreateCommand :: TestData -> CreateCase -> MarloweTxCommand Void (CreateError 'V1) (ContractCreated BabbageEra 'V1)
mkCreateCommand testData (CreateCase stakeCredential wallet (roleTokens, metadata) minLovelace) = Create
  (mkStakeCredential testData stakeCredential)
  MarloweV1
  (mkWalletAddresses testData wallet)
  (mkRoleTokensConfig testData roleTokens)
  (mkMarloweTxMetadata metadata)
  (mkMinLovelace minLovelace)
  (mkContract roleTokens)

mkStakeCredential :: TestData -> StakeCredentialCase -> Maybe StakeCredential
mkStakeCredential TestData{..} = \case
  UseStakeCredential -> Just stakeCredential
  NoStakeCredential -> Nothing

mkWalletAddresses :: TestData -> WalletCase -> WalletAddresses
mkWalletAddresses TestData{..} = \case
  WalletCase BalanceInsufficient SingleAddress _ -> singleAddressInsufficientBalanceWallet
  WalletCase BalanceInsufficient MultiAddress _ -> multiAddressInsufficientBalanceWallet
  WalletCase BalanceSufficient SingleAddress NoCollateralUTxOs -> singleAddressSufficientBalanceOneSufficientCollateralWallet { collateralUtxos = mempty }
  WalletCase BalanceSufficient SingleAddress OneCollateralUTxOInsufficient -> singleAddressSufficientBalanceOneInsufficientCollateralWallet
  WalletCase BalanceSufficient SingleAddress MultipleCollateralUTxOsInsufficient -> singleAddressSufficientBalanceMultiInsufficientCollateralWallet
  WalletCase BalanceSufficient SingleAddress OneCollateralUTxOSufficient -> singleAddressSufficientBalanceOneSufficientCollateralWallet
  WalletCase BalanceSufficient SingleAddress MultipleCollateralUTxOsSufficient -> singleAddressSufficientBalanceMultiSufficientCollateralWallet
  WalletCase BalanceSufficient MultiAddress NoCollateralUTxOs -> multiAddressSufficientBalanceOneSufficientCollateralWallet { collateralUtxos = mempty }
  WalletCase BalanceSufficient MultiAddress OneCollateralUTxOInsufficient -> multiAddressSufficientBalanceOneInsufficientCollateralWallet
  WalletCase BalanceSufficient MultiAddress MultipleCollateralUTxOsInsufficient -> multiAddressSufficientBalanceMultiInsufficientCollateralWallet
  WalletCase BalanceSufficient MultiAddress OneCollateralUTxOSufficient -> multiAddressSufficientBalanceOneSufficientCollateralWallet
  WalletCase BalanceSufficient MultiAddress MultipleCollateralUTxOsSufficient -> multiAddressSufficientBalanceMultiSufficientCollateralWallet

mkRoleTokensConfig :: TestData -> RoleTokenCase -> RoleTokensConfig
mkRoleTokensConfig TestData{..} = \case
  NoRoleTokens -> RoleTokensNone
  ExistingPolicyRoleTokens -> RoleTokensUsePolicy existingRoleTokenPolicy
  MintRoleTokensSimple -> RoleTokensMint $ mkMint $ NE.fromList
    [ ("Role", (changeAddress singleAddressInsufficientBalanceWallet, Nothing))
    ]
  MintRoleTokensMetadata -> RoleTokensMint $ mkMint $ NE.fromList
    [ ("Role", (changeAddress singleAddressInsufficientBalanceWallet, Just testNftMetadata))
    ]

testNftMetadata :: RoleTokenMetadata
testNftMetadata = RoleTokenMetadata
  { name = "Test token"
  , image = fromJust $ parseURI "https://example.com/img.png"
  , mediaType = Just "image/png"
  , files = []
  , description = Nothing
  }

mkMarloweTxMetadata :: MetadataCase -> MarloweTransactionMetadata
mkMarloweTxMetadata (MetadataCase marloweMetadata extraMetadata) = MarloweTransactionMetadata
  { marloweMetadata = mkMarloweMetadata marloweMetadata
  , transactionMetadata = mkExtraMetadata extraMetadata
  }

mkMarloweMetadata :: MarloweMetadataCase -> Maybe MarloweMetadata
mkMarloweMetadata = \case
  NoMarloweMetadata -> Nothing
  EmptyTags -> Just MarloweMetadata
    { tags = mempty
    , continuations = Nothing
    }
  NonEmptyTags -> Just testMarloweMetadata

testMarloweMetadata :: MarloweMetadata
testMarloweMetadata = MarloweMetadata
  { tags = Map.fromList
      [ ("tag1", Nothing)
      , ("tag2", Just $ MetadataNumber 0)
      ]
  , continuations = Nothing
  }

mkExtraMetadata :: ExtraMetadataCase -> TransactionMetadata
mkExtraMetadata (ExtraMetadataCase extraRandomMetadata extraMarloweMetadata extraNFTMetadata) =
  TransactionMetadata $ Map.fromList $ catMaybes
    [ guard extraRandomMetadata $> (1000, MetadataNumber 1000)
    , guard extraMarloweMetadata $> (1564, MetadataNumber 1564)
    , guard extraNFTMetadata $> (721, MetadataNumber 721)
    ]


mkMinLovelace :: MinLovelaceCase -> Lovelace
mkMinLovelace = \case
  MinLovelaceSufficient -> 5_000_000
  MinLovelaceInsufficient -> 500_000

mkContract :: RoleTokenCase -> V1.Contract
mkContract = \case
  NoRoleTokens -> V1.Close
  _ -> V1.Pay (V1.Role "Role") (V1.Party $ V1.Role "Role") (V1.Token "" "") (V1.Constant 10) V1.Close

allCreateCases :: NonEmpty CreateCase
allCreateCases = CreateCase
  <$> allEnumCases
  `varyAp` allWalletCases
  `varyAp` ((,) <$> allEnumCases <*> allMetadataCases)
  `varyAp` allEnumCases

allWalletCases :: NonEmpty WalletCase
allWalletCases = WalletCase <$> allEnumCases <*> allEnumCases <*> allEnumCases

allMetadataCases :: NonEmpty MetadataCase
allMetadataCases = MetadataCase <$> allEnumCases <*> allExtraMetadataCases

allExtraMetadataCases :: NonEmpty ExtraMetadataCase
allExtraMetadataCases = ExtraMetadataCase <$> allEnumCases <*> allEnumCases <*> allEnumCases

allEnumCases :: (Enum a, Bounded a) => NonEmpty a
allEnumCases = NE.fromList [minBound..maxBound]

data CreateCase = CreateCase StakeCredentialCase WalletCase (RoleTokenCase, MetadataCase) MinLovelaceCase
  deriving (Show, Eq)

data StakeCredentialCase
  = UseStakeCredential
  | NoStakeCredential
  deriving (Show, Eq, Enum, Bounded)

data WalletCase = WalletCase WalletBalanceCase WalletAddressesCase WalletCollateralUTxOCase
  deriving (Show, Eq)

data WalletBalanceCase
  = BalanceSufficient
  | BalanceInsufficient
  deriving (Show, Eq, Enum, Bounded)

data WalletAddressesCase
  = SingleAddress
  | MultiAddress
  deriving (Show, Eq, Enum, Bounded)

data WalletCollateralUTxOCase
  = NoCollateralUTxOs
  | OneCollateralUTxOInsufficient
  | OneCollateralUTxOSufficient
  | MultipleCollateralUTxOsInsufficient
  | MultipleCollateralUTxOsSufficient
  deriving (Show, Eq, Enum, Bounded)

data RoleTokenCase
  = NoRoleTokens
  | ExistingPolicyRoleTokens
  | MintRoleTokensSimple
  | MintRoleTokensMetadata
  deriving (Show, Eq, Enum, Bounded)

data MetadataCase = MetadataCase MarloweMetadataCase ExtraMetadataCase
  deriving (Show, Eq)

data MarloweMetadataCase
  = NoMarloweMetadata
  | EmptyTags
  | NonEmptyTags
  deriving (Show, Eq, Enum, Bounded)

data ExtraMetadataCase = ExtraMetadataCase Bool Bool Bool
  deriving (Show, Eq)

data MinLovelaceCase
  = MinLovelaceSufficient
  | MinLovelaceInsufficient
  deriving (Show, Eq, Enum, Bounded)
