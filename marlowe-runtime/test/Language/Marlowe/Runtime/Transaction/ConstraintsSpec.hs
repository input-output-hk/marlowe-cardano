{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
module Language.Marlowe.Runtime.Transaction.ConstraintsSpec
  where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScriptOrReferenceInput(PScript), ProtocolParameters)
import Control.Applicative (Alternative)
import Control.Monad (guard)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Word (Word32)
import GHC.Word (Word64)
import Gen.Cardano.Api.Metadata (genTxMetadataValue)
import Gen.Cardano.Api.Typed
  ( genAddressByron
  , genAddressShelley
  , genAssetName
  , genPlutusScript
  , genProtocolParameters
  , genScriptData
  , genScriptHash
  , genTxId
  , genValueForTxOut
  , genVerificationKey
  )
import Language.Marlowe (MarloweData(..), MarloweParams(..), txInputs)
import Language.Marlowe.Runtime.Cardano.Api
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  ( Contract
  , Datum
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , PayoutDatum
  , Redeemer
  , TransactionScriptOutput(..)
  )
import Language.Marlowe.Runtime.Core.ScriptRegistry (ReferenceScriptUtxo(..))
import Language.Marlowe.Runtime.Transaction.Constraints
import Spec.Marlowe.Common (shrinkContract)
import Spec.Marlowe.Semantics.Arbitrary (SemiArbitrary(semiArbitrary), arbitraryValidInput)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (shrinkMap)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Property (failed)

spec :: Spec
spec = do
  focus $ describe "solveInitialTxBodyContent" do
    prop "satisfies the constraints" \(SolveInitialTxBodyContentArgs SolveInitialTxBodyContentArgs'{..}) ->
      case solveInitialTxBodyContent protocol marloweVersion marloweContext walletContext constraints of
        Left err -> case marloweVersion of
          MarloweV1 -> counterexample (show err) failed
        Right txBodyContent -> satisfiesConstraints marloweVersion constraints txBodyContent

satisfiesConstraints :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> Property
satisfiesConstraints marloweVersion constraints txBodyContent =
  violations marloweVersion constraints txBodyContent === []

violations :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
violations marloweVersion constraints txBodyContent = fold
  [ ("mustMintRoleToken: " <>) <$> mustMintRoleTokenViolations marloweVersion constraints txBodyContent
  , ("mustSpendRoleToken: " <>) <$> mustSpendRoleTokenViolations marloweVersion constraints txBodyContent
  , ("mustPayToAddress: " <>) <$> mustPayToAddressViolations marloweVersion constraints txBodyContent
  , ("mustSendMarloweOutput: " <>) <$> mustSendMarloweOutputViolations marloweVersion constraints txBodyContent
  , ("mustSendMerkleizedContinuationOutput: " <>) <$> mustSendMerkleizedContinuationOutputViolations marloweVersion constraints txBodyContent
  , ("mustPayToRole: " <>) <$> mustPayToRoleViolations marloweVersion constraints txBodyContent
  , ("mustConsumeMarloweOutput: " <>) <$> mustConsumeMarloweOutputViolations marloweVersion constraints txBodyContent
  , ("mustConsumePayouts: " <>) <$> mustConsumePayoutsViolations marloweVersion constraints txBodyContent
  , ("requiresSignature: " <>) <$> requiresSignatureViolations marloweVersion constraints txBodyContent
  , ("requiresMetadata: " <>) <$> requiresMetadataViolations marloweVersion constraints txBodyContent
  ]

check :: Alternative m => Bool -> a -> m a
check condition msg = msg <$ guard (not condition)

mustMintRoleTokenViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustMintRoleTokenViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = fold
  [ mintsOneToken
  , sendsOneTokenAndOnlyToken
  , consumesUtxo
  ]
  where
    consumesUtxo = case roleTokenConstraints of
      MintRoleTokens txOutRef _ _ -> do
        check
          (any ((== txOutRef) . fromCardanoTxIn . fst) txIns)
          ("UTxO not consumed: " <> show (Chain.renderTxOutRef txOutRef))
      _ -> []

    sendsOneTokenAndOnlyToken = case roleTokenConstraints of
      MintRoleTokens _ _ distribution -> do
        (assetId, address) <- Map.toList distribution
        (("roleToken: " <> show assetId) <>) <$> do
          let
            cardanoAssetId = toCardanoAssetId assetId
            matches (TxOut outAddress (TxOutValue MultiAssetInBabbageEra value) _ _)
              | selectAsset value cardanoAssetId > 0 = Just (outAddress, value)
              | otherwise = Nothing
            matches (TxOut _ (TxOutAdaOnly era _) _ _) = case era of
          let matchingOuts = mapMaybe matches txOuts
          case matchingOuts of
            [(outAddress, value)] -> do
              fold
                [ check
                    (fmap fst (valueToList value) == [cardanoAssetId])
                    ("Output contains extra tokens: " <> show (fmap fst (valueToList value)))
                , check
                    (selectAsset value cardanoAssetId == 1)
                    ("Output quantity for token expected to equal 1, was: " <> show (selectAsset value cardanoAssetId))
                , check
                    (fromCardanoAddressInEra BabbageEra outAddress == address)
                    ("Output sent to wrong address: " <> show outAddress)
                ]
            [] -> pure "No outputs contain role token"
            _ -> pure "Multiple outputs contain role token"
      _ -> []

    mintsOneToken = case roleTokenConstraints of
      MintRoleTokens _ _ distribution -> case txMintValue of
        TxMintNone
          | Map.null distribution -> []
          | otherwise -> ["No tokens minted"]
        TxMintValue MultiAssetInBabbageEra value _ -> do
          assetId <- Map.keys distribution
          (("roleToken: " <> show assetId) <>) <$> do
            let cardanoAssetId = toCardanoAssetId assetId
            let quantityMinted = selectAsset value cardanoAssetId
            check (quantityMinted == 1) ("Expected to mint 1 token, found " <> show quantityMinted)
      _ -> []

toCardanoAssetId :: Chain.AssetId -> AssetId
toCardanoAssetId (Chain.AssetId policy name) = AssetId
  (fromJust $ toCardanoPolicyId policy)
  (toCardanoAssetName name)


mustSpendRoleTokenViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustSpendRoleTokenViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

mustPayToAddressViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustPayToAddressViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

mustSendMarloweOutputViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustSendMarloweOutputViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

mustSendMerkleizedContinuationOutputViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustSendMerkleizedContinuationOutputViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

mustPayToRoleViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustPayToRoleViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

mustConsumeMarloweOutputViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustConsumeMarloweOutputViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

mustConsumePayoutsViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustConsumePayoutsViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

requiresSignatureViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
requiresSignatureViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

requiresMetadataViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
requiresMetadataViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = []

data SolveInitialTxBodyContentArgs =
  forall v. SolveInitialTxBodyContentArgs (SolveInitialTxBodyContentArgs' v)

instance Show SolveInitialTxBodyContentArgs where
  show (SolveInitialTxBodyContentArgs args@SolveInitialTxBodyContentArgs'{..}) = case marloweVersion of
    MarloweV1 -> show args

data SolveInitialTxBodyContentArgs' v = SolveInitialTxBodyContentArgs'
  { protocol :: ProtocolParameters
  , marloweVersion :: MarloweVersion v
  , marloweContext :: MarloweContext v
  , walletContext :: WalletContext
  , constraints :: TxConstraints v
  }

deriving instance Show (SolveInitialTxBodyContentArgs' 'V1)

instance Arbitrary SolveInitialTxBodyContentArgs where
  arbitrary = oneof
    [ SolveInitialTxBodyContentArgs <$> genV1SolveInitialTxBodyContentArgs
    ]
  shrink (SolveInitialTxBodyContentArgs args@SolveInitialTxBodyContentArgs'{..}) =
    case marloweVersion of
      MarloweV1 -> SolveInitialTxBodyContentArgs <$> shrinkV1SolveInitialTxBodyContentArgs args

shrinkV1SolveInitialTxBodyContentArgs
  :: SolveInitialTxBodyContentArgs' 'V1
  -> [SolveInitialTxBodyContentArgs' 'V1]
shrinkV1SolveInitialTxBodyContentArgs SolveInitialTxBodyContentArgs'{..} = do
  constraints' <- shrinkConstraints constraints
  SolveInitialTxBodyContentArgs' protocol MarloweV1
    <$> shrinkMarloweContext constraints' marloweContext
    <*> shrinkWalletContext constraints' walletContext
    <*> pure constraints'

shrinkConstraints :: TxConstraints 'V1 -> [TxConstraints 'V1]
shrinkConstraints TxConstraints{..} = TxConstraints
  <$> shrinkMarloweInputConstraints marloweInputConstraints
  <*> shrinkPayoutInputConstraints payoutInputConstraints
  <*> shrinkRoleTokenConstraints roleTokenConstraints
  <*> shrinkPayToAddresses payToAddresses
  <*> shrinkPayToRoles payToRoles
  <*> shrinkMarloweOutputConstraints marloweOutputConstraints
  <*> shrinkMerkleizedContinuationsConstraints merkleizedContinuationsConstraints
  <*> shrinkSignatureConstraints signatureConstraints
  <*> shrinkMetadataConstraints metadataConstraints

shrinkMarloweInputConstraints :: MarloweInputConstraints 'V1 -> [MarloweInputConstraints 'V1]
shrinkMarloweInputConstraints = \case
  MarloweInputConstraintsNone -> []
  MarloweInput s1 s2 redeemer -> MarloweInputConstraintsNone : (MarloweInput s1 s2 <$> shrinkList shrinkNothing redeemer)

shrinkSet :: (a -> [a]) -> Set.Set a -> [Set.Set a]
shrinkSet shrinkItem = fmap Set.fromDistinctAscList . shrinkList shrinkItem . Set.toAscList

shrinkMap :: (v -> [v]) -> Map k v -> [Map k v]
shrinkMap shrinkItem = fmap Map.fromDistinctAscList . shrinkList (traverse shrinkItem) . Map.toAscList

shrinkPayoutInputConstraints :: Set.Set Chain.AssetId -> [Set.Set Chain.AssetId]
shrinkPayoutInputConstraints = shrinkSet shrinkNothing

shrinkRoleTokenConstraints :: RoleTokenConstraints -> [RoleTokenConstraints]
shrinkRoleTokenConstraints = \case
  RoleTokenConstraintsNone -> []
  MintRoleTokens ref witness distribution -> RoleTokenConstraintsNone :
    (MintRoleTokens ref witness <$> shrinkMap shrinkNothing distribution)
  SpendRoleTokens roleTokens -> RoleTokenConstraintsNone :
    (SpendRoleTokens <$> shrinkSet shrinkNothing roleTokens)

shrinkPayToAddresses :: Map Chain.Address Chain.Assets -> [Map Chain.Address Chain.Assets]
shrinkPayToAddresses = shrinkMap shrinkAssets

shrinkPayToRoles :: Map Chain.AssetId Chain.Assets -> [Map Chain.AssetId Chain.Assets]
shrinkPayToRoles = shrinkMap shrinkAssets

shrinkAssets :: Chain.Assets -> [Chain.Assets]
shrinkAssets Chain.Assets{..} = Chain.Assets ada <$> shrinkTokens tokens

shrinkTokens :: Chain.Tokens -> [Chain.Tokens]
shrinkTokens = fmap Chain.Tokens . shrinkMap shrinkNothing . Chain.unTokens

shrinkMarloweOutputConstraints :: MarloweOutputConstraints 'V1 -> [MarloweOutputConstraints 'V1]
shrinkMarloweOutputConstraints = \case
  MarloweOutputConstraintsNone -> []
  MarloweOutput assets datum -> MarloweOutput <$> shrinkAssets assets <*> shrinkDatum datum

shrinkDatum :: MarloweData -> [MarloweData]
shrinkDatum MarloweData{..} = MarloweData marloweParams marloweState <$> shrinkContract marloweContract

shrinkMerkleizedContinuationsConstraints :: Set.Set (Contract 'V1) -> [Set.Set (Contract 'V1)]
shrinkMerkleizedContinuationsConstraints = shrinkSet shrinkContract

shrinkSignatureConstraints :: Set.Set Chain.PaymentKeyHash -> [Set.Set Chain.PaymentKeyHash]
shrinkSignatureConstraints = shrinkSet shrinkNothing

shrinkMetadataConstraints :: Map Word64 Chain.Metadata -> [Map Word64 Chain.Metadata]
shrinkMetadataConstraints metadata
  | Map.null metadata = []
  | otherwise = [mempty]

shrinkWalletContext :: TxConstraints 'V1 -> WalletContext -> [WalletContext]
shrinkWalletContext _ _ = []

genV1SolveInitialTxBodyContentArgs :: Gen (SolveInitialTxBodyContentArgs' 'V1)
genV1SolveInitialTxBodyContentArgs = do
  constraints <- fixConstraints <$> genConstraints
  SolveInitialTxBodyContentArgs'
    <$> hedgehog genProtocolParameters
    <*> pure MarloweV1
    <*> genMarloweContext constraints
    <*> genWalletContext constraints
    <*> pure constraints

fixConstraints :: TxConstraints 'V1 -> TxConstraints 'V1
fixConstraints constraints@TxConstraints{..} = constraints
  { payToAddresses = case roleTokenConstraints of
      MintRoleTokens _ _ distribution -> removeTokens distribution <$> payToAddresses
      _ -> payToAddresses
  , payToRoles = case roleTokenConstraints of
      MintRoleTokens _ _ distribution -> removeTokens distribution <$> payToRoles
      _ -> payToRoles
  , marloweOutputConstraints = case (roleTokenConstraints, marloweOutputConstraints) of
      (MintRoleTokens _ _ distribution, MarloweOutput assets datum) ->
        MarloweOutput (removeTokens distribution assets) datum
      _ -> marloweOutputConstraints
  }

removeTokens :: Map Chain.AssetId Chain.Address -> Chain.Assets -> Chain.Assets
removeTokens distribution (Chain.Assets lovelace (Chain.Tokens tokens)) =
  Chain.Assets lovelace $ Chain.Tokens $ Map.difference tokens distribution

genConstraints :: Gen (TxConstraints 'V1)
genConstraints = sized \n -> frequency
    [ (n, resize (n `div` 2) $ (<>) <$> genConstraints <*> genConstraints)
    , (1, pure mempty)
    , (1, mustMintRoleToken <$> genTxOutRef <*> genMintScriptWitness <*> genAssetId <*> genAddress)
    , (1, mustSpendRoleToken <$> genAssetId)
    , (1, mustPayToAddress <$> genOutAssets <*> genAddress)
    , (1, mustSendMarloweOutput <$> genOutAssets <*> genDatum)
    , (1, mustSendMerkleizedContinuationOutput <$> genContract)
    , (1, mustPayToRole <$> genOutAssets <*> genAssetId)
    , (1, uncurry mustConsumeMarloweOutput <$> genValidityInterval <*> genRedeemer)
    , (1, mustConsumePayouts <$> genAssetId)
    , (1, requiresSignature <$> genPaymentKeyHash)
    , (1, requiresMetadata <$> arbitrary <*> genMetadata)
    ]

genValidityInterval :: Gen (SlotNo, SlotNo)
genValidityInterval = do
  (s1, s2) <- arbitrary
  pure (SlotNo $ min s1 s2, SlotNo $ max s1 s2)

genRedeemer :: Gen (Redeemer 'V1)
genRedeemer = do
  ctx <- arbitrary
  state <- semiArbitrary ctx
  contract <- semiArbitrary ctx
  txInputs <$> arbitraryValidInput state contract

genMetadata :: Gen Chain.Metadata
genMetadata = hedgehog $ fromCardanoMetadata <$> genTxMetadataValue

genPaymentKeyHash :: Gen Chain.PaymentKeyHash
genPaymentKeyHash = hedgehog $ fromCardanoPaymentKeyHash . verificationKeyHash <$> genVerificationKey AsPaymentKey

genPayoutDatum :: Gen (PayoutDatum 'V1)
genPayoutDatum = genAssetId

genContract :: Gen (Contract 'V1)
genContract = semiArbitrary =<< arbitrary

genDatum :: Gen (Datum 'V1)
genDatum = do
  ctx <- arbitrary
  MarloweData <$> (MarloweParams <$> arbitrary) <*> semiArbitrary ctx <*> semiArbitrary ctx

genOutAssets :: Gen Chain.Assets
genOutAssets = hedgehog $ assetsFromCardanoValue <$> genValueForTxOut

genTxOutRef :: Gen Chain.TxOutRef
genTxOutRef = Chain.TxOutRef <$> hedgehog (fromCardanoTxId <$> genTxId) <*> (Chain.TxIx <$> arbitrary)

genMintScriptWitness :: Gen (ScriptWitness WitCtxMint BabbageEra)
genMintScriptWitness = oneof
  [ PlutusScriptWitness
      PlutusScriptV1InBabbage
      PlutusScriptV1
      <$> (hedgehog $ PScript <$> genPlutusScript PlutusScriptV1)
      <*> pure NoScriptDatumForMint
      <*> hedgehog genScriptData
      <*> (ExecutionUnits <$> (fromIntegral @Word32 <$> arbitrary) <*> (fromIntegral @Word32 <$> arbitrary))
  ]

genAssetId :: Gen Chain.AssetId
genAssetId = hedgehog $ Chain.AssetId
  <$> (fromCardanoPolicyId . PolicyId <$> genScriptHash)
  <*> (fromCardanoAssetName <$> genAssetName)

genAddress :: Gen Chain.Address
genAddress = fromCardanoAddressAny <$> oneof
  [ hedgehog $ AddressByron <$> genAddressByron
  , hedgehog $ AddressShelley <$> genAddressShelley
  ]

shrinkMarloweContext :: TxConstraints 'V1 -> MarloweContext 'V1 -> [MarloweContext 'V1]
shrinkMarloweContext constraints MarloweContext{..} = MarloweContext
  <$> shrinkScriptOutput constraints scriptOutput
  <*> shrinkPayoutOutputs constraints payoutOutputs
  <*> pure marloweAddress
  <*> pure payoutAddress
  <*> shrinkReferenceScriptUtxo marloweScriptUTxO
  <*> shrinkReferenceScriptUtxo payoutScriptUTxO
  <*> pure marloweScriptHash
  <*> pure payoutScriptHash

genMarloweContext :: TxConstraints 'V1 -> Gen (MarloweContext 'V1)
genMarloweContext constraints = do
  marloweScriptHash <- hedgehog genScriptHash
  payoutScriptHash <- hedgehog genScriptHash
  let
    scriptAddress hash = fromCardanoAddressAny
      $ AddressShelley
      $ makeShelleyAddress Mainnet (PaymentCredentialByScript hash) NoStakeAddress
    marloweAddress = scriptAddress marloweScriptHash
    payoutAddress = scriptAddress payoutScriptHash
  MarloweContext
    <$> genScriptOutput marloweAddress constraints
    <*> genPayoutOutputs payoutAddress constraints
    <*> pure marloweAddress
    <*> pure payoutAddress
    <*> genReferenceScriptUtxo marloweAddress
    <*> genReferenceScriptUtxo payoutAddress
    <*> pure (fromCardanoScriptHash marloweScriptHash)
    <*> pure (fromCardanoScriptHash payoutScriptHash)

shrinkScriptOutput :: TxConstraints 'V1 -> Maybe (TransactionScriptOutput 'V1) -> [Maybe (TransactionScriptOutput 'V1)]
shrinkScriptOutput TxConstraints{..} = \case
  Nothing -> []
  Just TransactionScriptOutput{..} -> case marloweInputConstraints of
    MarloweInputConstraintsNone -> [Nothing]
    MarloweInput {} -> Just <$>
      (TransactionScriptOutput address
        <$> shrinkAssets assets
        <*> pure utxo
        <*> shrinkDatum datum
      )


genScriptOutput :: Chain.Address -> TxConstraints 'V1 -> Gen (Maybe (TransactionScriptOutput 'V1))
genScriptOutput address TxConstraints{..} = case marloweInputConstraints of
  MarloweInputConstraintsNone -> oneof
    [ pure Nothing
    , Just <$> (TransactionScriptOutput address <$> genOutAssets <*> genTxOutRef <*> genDatum)
    ]
  MarloweInput {} -> Just <$> (TransactionScriptOutput address <$> genOutAssets <*> genTxOutRef <*> genDatum)

shrinkPayoutOutputs :: TxConstraints 'V1 -> Map Chain.TxOutRef (Payout 'V1) -> [Map Chain.TxOutRef (Payout 'V1)]
shrinkPayoutOutputs TxConstraints{..} = filter containsAllRequired . shrinkMap shrinkPayout
  where
    containsAllRequired payouts = all (flip Set.member $ Set.fromList $ fmap getDatum $ snd <$> Map.toList payouts) payoutInputConstraints
    getDatum Payout{..} = datum

shrinkPayout :: Payout 'V1 -> [Payout 'V1]
shrinkPayout Payout{..} = Payout address <$> shrinkAssets assets <*> pure datum

genPayoutOutputs :: Chain.Address -> TxConstraints 'V1 -> Gen (Map Chain.TxOutRef (Payout 'V1))
genPayoutOutputs address TxConstraints{..} = (<>) <$> required <*> extra
  where
    required = Map.fromList <$> traverse (genPayout address) (Set.toList payoutInputConstraints)
    extra = Map.fromList <$> listOf (genPayout address =<< genAssetId)

genPayout :: Chain.Address -> Chain.AssetId -> Gen (Chain.TxOutRef, Payout 'V1)
genPayout address datum = do
  assets <- genOutAssets
  (,Payout{..}) <$> genTxOutRef

shrinkReferenceScriptUtxo :: ReferenceScriptUtxo -> [ReferenceScriptUtxo]
shrinkReferenceScriptUtxo ReferenceScriptUtxo{..} = ReferenceScriptUtxo txOutRef
  <$> shrinkTransactionOutput txOut
  <*> pure script

shrinkTransactionOutput :: Chain.TransactionOutput -> [Chain.TransactionOutput]
shrinkTransactionOutput Chain.TransactionOutput{..} = Chain.TransactionOutput address
  <$> shrinkAssets assets
  <*> pure datumHash
  <*> pure datum

genReferenceScriptUtxo :: Chain.Address -> Gen ReferenceScriptUtxo
genReferenceScriptUtxo address = ReferenceScriptUtxo
  <$> genTxOutRef
  <*> genTransactionOutput (pure address)
  <*> hedgehog (genPlutusScript PlutusScriptV2)

genTransactionOutput :: Gen Chain.Address -> Gen Chain.TransactionOutput
genTransactionOutput address = Chain.TransactionOutput
  <$> address
  <*> genOutAssets
  <*> pure Nothing
  <*> pure Nothing

genWalletContext :: TxConstraints 'V1 -> Gen WalletContext
genWalletContext constraints = WalletContext
  <$> genWalletUtxos constraints
  <*> pure mempty
  <*> genAddress

genWalletUtxos :: TxConstraints 'V1 -> Gen Chain.UTxOs
genWalletUtxos TxConstraints{..} = (<>) <$> required <*> extra
  where
    required = case roleTokenConstraints of
      RoleTokenConstraintsNone -> pure mempty
      MintRoleTokens txOutRef _ _ -> Chain.UTxOs . Map.singleton txOutRef <$> genTransactionOutput genAddress
      SpendRoleTokens roleTokens -> fold <$> for (Set.toList roleTokens) \roleToken -> do
        txOutRef <- genTxOutRef
        txOut <- genTransactionOutput genAddress
        let roleTokenAssets = Chain.Assets 0 $ Chain.Tokens $ Map.singleton roleToken 1
        pure $ Chain.UTxOs $ Map.singleton txOutRef $ txOut { Chain.assets = Chain.assets txOut <> roleTokenAssets }
    extra = fold <$> listOf do
      txOutRef <- genTxOutRef
      txOut <- genTransactionOutput genAddress
      pure $ Chain.UTxOs $ Map.singleton txOutRef txOut
