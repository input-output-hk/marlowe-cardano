{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Integration.Create
  where

import Cardano.Api
  ( BabbageEra
  , CardanoEra(..)
  , TxBody(..)
  , TxBodyContent(..)
  , TxInsCollateral(..)
  , TxMintValue(..)
  , TxOut(..)
  , TxOutValue(..)
  , valueToList
  )
import qualified Cardano.Api as C
import Cardano.Api.ChainSync.ClientPipelined (Nat(..))
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api
  (assetsFromCardanoValue, fromCardanoAddressInEra, fromCardanoTxIn, fromCardanoTxOutValue)
import Language.Marlowe.Runtime.ChainSync.Api
  ( AssetId(..)
  , Assets(..)
  , Lovelace
  , Metadata(..)
  , PolicyId(..)
  , ScriptHash
  , StakeCredential
  , StakeReference(..)
  , Tokens(..)
  , TransactionMetadata(..)
  , stakeReference
  )
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Integration.Common (runIntegrationTest)
import Language.Marlowe.Runtime.Transaction.Api
import Network.Protocol.Codec.Spec (varyAp)
import Network.Protocol.Job.Client (liftCommand)
import Test.Hspec
import Test.Integration.Marlowe (MarloweRuntime(..), withLocalMarloweRuntime)
import Test.Matcher
  (CustomMatcher(..), EqMatcher(..), Matcher(..), TrivialMatcher(..), VariantMatcher(..), injectMatcher)

spec :: Spec
spec = describe "Create" $ aroundAll setup do
  for_ allCreateCases \createCase -> aroundAllWith (runCreateCase createCase) do
    describe (show createCase) do
      case mkSpec createCase of
        Left errorMatcher -> it "Should fail" \case
          (_, Left err) -> match errorMatcher err
          (_, Right contractCreated) -> expectationFailure $ "Expected a failure, but it succeeded: " <> show contractCreated
        Right s -> aroundAllWith expectSuccess s

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
  pure ()

mkSpec :: CreateCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
mkSpec (CreateCase stakeCredential wallet (roleTokens, metadata) minLovelace) = mergeSpecs
  [ stakeCredentialsSpec stakeCredential
  , walletSpec wallet
  , roleTokenSpec roleTokens
  , metadataSpec roleTokens metadata
  , minLovelaceSpec minLovelace
  ]

mergeSpecs
  :: [Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))]
  -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
mergeSpecs = go $ Right $ pure ()
  where
    go acc [] = acc
    go acc (spec' : specs) = case (acc, spec') of
      (Left acc', Left errs) -> go (Left $ acc' <> errs) specs
      (Left _, _) -> go acc specs
      (Right _, Left errs) -> go (Left errs) specs
      (Right tests1, Right tests2) -> go (Right $ tests1 *> tests2) specs

stakeCredentialsSpec :: StakeCredentialCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
stakeCredentialsSpec = Right . \case
  UseStakeCredential -> do
    it "Attaches a stake credential to the Marlowe script address" \(TestData{..}, ContractCreated{..}) -> do
      stakeReference marloweScriptAddress `shouldBe` Just (StakeCredential stakeCredential)
  NoStakeCredential -> do
    it "Does not attach a stake credential to the Marlowe script address" \(_, ContractCreated{..}) -> do
      stakeReference marloweScriptAddress `shouldBe` Nothing

walletSpec :: WalletCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
walletSpec (WalletCase balance addresses collateral) = mergeSpecs
  [ balanceSpec balance
  , collateralSpec addresses collateral
  ]

balanceSpec :: WalletBalanceCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
balanceSpec = \case
  BalanceSufficient -> Right $ pure ()
  BalanceInsufficient -> Left $ matchCreateConstraintError $ matchCoinSelectionFailed $ CustomMatcher \msg ->
    msg `shouldStartWith` "Insufficient lovelace available for coin selection:"

collateralSpec :: WalletAddressesCase -> WalletCollateralUTxOCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
collateralSpec addresses = \case
  NoCollateralUTxOs -> Right $ pure ()
  OneCollateralUTxOInsufficient -> Right do
    it "Should fail if there is insufficient collateral" $ const pending
  OneCollateralUTxOSufficient -> Right do
    it "Should only spend the one collateral UTxO" \(TestData{..}, ContractCreated{..}) -> do
      let
        collateral = case txBody of
          TxBody TxBodyContent{..} -> case txInsCollateral of
            TxInsCollateralNone -> mempty
            TxInsCollateral _ collateralTxIns -> Set.fromList $ fromCardanoTxIn <$> collateralTxIns
      collateral `shouldBe` collateralUtxos case addresses of
        SingleAddress -> singleAddressSufficientBalanceOneSufficientCollateralWallet
        MultiAddress -> multiAddressSufficientBalanceOneSufficientCollateralWallet
  MultipleCollateralUTxOsInsufficient -> Right do
    it "Should fail if there is insufficient collateral" $ const pending
  MultipleCollateralUTxOsSufficient -> Right do
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

roleTokenSpec :: RoleTokenCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
roleTokenSpec = \case
  NoRoleTokens -> Right do
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
  ExistingPolicyRoleTokens -> Right do
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
  _ -> Right do
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

metadataSpec :: RoleTokenCase -> MetadataCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
metadataSpec _ _ = Right $ pure ()

minLovelaceSpec :: MinLovelaceCase -> Either CreateErrorMatcher (SpecWith (TestData, ContractCreated BabbageEra v))
minLovelaceSpec _ = Right $ pure ()

type CreateErrorMatcher = VariantMatcher (CreateError 'V1)
  '[ ConstraintErrorMatcher
   , LoadMarloweContextErrorMatcher
   ]

matchCreateConstraintError :: ConstraintErrorMatcher -> CreateErrorMatcher
matchCreateConstraintError matcher = injectMatcher Zero matcher \case
  CreateConstraintError e -> Just e
  _ -> Nothing

matchCreateLoadMarloweContextFailed :: LoadMarloweContextErrorMatcher -> CreateErrorMatcher
matchCreateLoadMarloweContextFailed matcher = injectMatcher (Succ Zero) matcher \case
  CreateLoadMarloweContextFailed e -> Just e
  _ -> Nothing

type ConstraintErrorMatcher = VariantMatcher (ConstraintError 'V1)
  '[ TrivialMatcher
   , EqMatcher AssetId
   , TrivialMatcher
   , TrivialMatcher
   , EqMatcher AssetId
   , CustomMatcher String
   , CustomMatcher String
   , CustomMatcher String
   ]

matchMintingUtxoNotFound :: ConstraintErrorMatcher
matchMintingUtxoNotFound = injectMatcher Zero TrivialMatcher \case
  MintingUtxoNotFound _ -> Just ()
  _ -> Nothing

matchRoleTokenNotFound :: AssetId -> ConstraintErrorMatcher
matchRoleTokenNotFound assetId = injectMatcher (Succ Zero) (EqMatcher assetId) \case
  RoleTokenNotFound e -> Just e
  _ -> Nothing

matchToCardanoError :: ConstraintErrorMatcher
matchToCardanoError = injectMatcher (Succ $ Succ Zero) TrivialMatcher \case
  ToCardanoError -> Just ()
  _ -> Nothing

matchMissingMarloweInput :: ConstraintErrorMatcher
matchMissingMarloweInput = injectMatcher (Succ $ Succ $ Succ Zero) TrivialMatcher \case
  MissingMarloweInput -> Just ()
  _ -> Nothing

matchPayoutInputNotFound :: AssetId -> ConstraintErrorMatcher
matchPayoutInputNotFound payoutDatum = injectMatcher (Succ $ Succ $ Succ $ Succ Zero) (EqMatcher payoutDatum) \case
  PayoutInputNotFound e -> Just e
  _ -> Nothing

matchCalculateMinUtxoFailed :: CustomMatcher String -> ConstraintErrorMatcher
matchCalculateMinUtxoFailed matcher = injectMatcher (Succ $ Succ $ Succ $ Succ $ Succ Zero) matcher \case
  CalculateMinUtxoFailed e -> Just e
  _ -> Nothing

matchCoinSelectionFailed :: CustomMatcher String -> ConstraintErrorMatcher
matchCoinSelectionFailed matcher = injectMatcher (Succ $ Succ $ Succ $ Succ $ Succ $ Succ Zero) matcher \case
  CoinSelectionFailed e -> Just e
  _ -> Nothing

matchBalancingError :: CustomMatcher String -> ConstraintErrorMatcher
matchBalancingError matcher = injectMatcher (Succ $ Succ $ Succ $ Succ $ Succ $ Succ $ Succ Zero) matcher \case
  BalancingError e -> Just e
  _ -> Nothing

type LoadMarloweContextErrorMatcher = VariantMatcher LoadMarloweContextError
  '[ TrivialMatcher
   , EqMatcher SomeMarloweVersion
   , TrivialMatcher
   , EqMatcher ScriptHash
   , EqMatcher ScriptHash
   ]

matchLoadMarloweContextErrorNotFound :: LoadMarloweContextErrorMatcher
matchLoadMarloweContextErrorNotFound = injectMatcher Zero TrivialMatcher \case
  LoadMarloweContextErrorNotFound -> Just ()
  _ -> Nothing

matchLoadMarloweContextErrorVersionMismatch :: SomeMarloweVersion -> LoadMarloweContextErrorMatcher
matchLoadMarloweContextErrorVersionMismatch version = injectMatcher (Succ Zero) (EqMatcher version) \case
  LoadMarloweContextErrorVersionMismatch e -> Just e
  _ -> Nothing

matchLoadMarloweContextToCardanoError :: LoadMarloweContextErrorMatcher
matchLoadMarloweContextToCardanoError = injectMatcher (Succ $ Succ Zero) TrivialMatcher \case
  LoadMarloweContextToCardanoError -> Just ()
  _ -> Nothing

matchMarloweScriptNotPublished :: ScriptHash -> LoadMarloweContextErrorMatcher
matchMarloweScriptNotPublished scriptHash = injectMatcher (Succ $ Succ $ Succ Zero) (EqMatcher scriptHash) \case
  MarloweScriptNotPublished e -> Just e
  _ -> Nothing

matchPayoutScriptNotPublished :: ScriptHash -> LoadMarloweContextErrorMatcher
matchPayoutScriptNotPublished scriptHash = injectMatcher (Succ $ Succ $ Succ $ Succ Zero) (EqMatcher scriptHash) \case
  PayoutScriptNotPublished e -> Just e
  _ -> Nothing

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
    [ ("Role", (changeAddress singleAddressInsufficientBalanceWallet, Left 1))
    ]
  MintRoleTokensMetadata -> RoleTokensMint $ mkMint $ NE.fromList
    [ ("Role", (changeAddress singleAddressInsufficientBalanceWallet, Right $ mkNFTMetadata $ MetadataNumber 42))
    ]

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
  NonEmptyTags -> Just MarloweMetadata
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
  MinLovelaceSufficient -> 2_000_000
  MinLovelaceInsufficient -> 1_000_000

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
