{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.Transaction.ConstraintsSpec
  where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScriptOrReferenceInput(PScript), ProtocolParameters)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Word (Word32)
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
import Language.Marlowe (MarloweData(MarloweData), MarloweParams(..), txInputs)
import Language.Marlowe.Runtime.Cardano.Api
  ( assetsFromCardanoValue
  , fromCardanoAddressAny
  , fromCardanoAssetName
  , fromCardanoPaymentKeyHash
  , fromCardanoPolicyId
  , fromCardanoScriptHash
  , fromCardanoTxId
  )
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
import Spec.Marlowe.Semantics.Arbitrary (SemiArbitrary(semiArbitrary), arbitraryValidInput)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Property (failed)

spec :: Spec
spec = do
  describe "solveInitialTxBodyContent" do
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
  [ mustMintRoleTokenViolations marloweVersion constraints txBodyContent
  , mustSpendRoleTokenViolations marloweVersion constraints txBodyContent
  , mustPayToAddressViolations marloweVersion constraints txBodyContent
  , mustSendMarloweOutputViolations marloweVersion constraints txBodyContent
  , mustSendMerkleizedContinuationOutputViolations marloweVersion constraints txBodyContent
  , mustPayToRoleViolations marloweVersion constraints txBodyContent
  , mustConsumeMarloweOutputViolations marloweVersion constraints txBodyContent
  , mustConsumePayoutsViolations marloweVersion constraints txBodyContent
  , requiresSignatureViolations marloweVersion constraints txBodyContent
  , requiresMetadataViolations marloweVersion constraints txBodyContent
  ]

mustMintRoleTokenViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustMintRoleTokenViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

mustSpendRoleTokenViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustSpendRoleTokenViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

mustPayToAddressViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustPayToAddressViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

mustSendMarloweOutputViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustSendMarloweOutputViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

mustSendMerkleizedContinuationOutputViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustSendMerkleizedContinuationOutputViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

mustPayToRoleViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustPayToRoleViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

mustConsumeMarloweOutputViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustConsumeMarloweOutputViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

mustConsumePayoutsViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
mustConsumePayoutsViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

requiresSignatureViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
requiresSignatureViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

requiresMetadataViolations
  :: MarloweVersion v -> TxConstraints v -> TxBodyContent BuildTx BabbageEra -> [String]
requiresMetadataViolations MarloweV1 TxConstraints{..} TxBodyContent{..} = error "not implemented"

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

genV1SolveInitialTxBodyContentArgs :: Gen (SolveInitialTxBodyContentArgs' 'V1)
genV1SolveInitialTxBodyContentArgs = do
  constraints <- genConstraints
  SolveInitialTxBodyContentArgs'
    <$> hedgehog genProtocolParameters
    <*> pure MarloweV1
    <*> genMarloweContext constraints
    <*> genWalletContext constraints
    <*> pure constraints

genConstraints :: Gen (TxConstraints 'V1)
genConstraints = sized \n -> frequency
    [ (n, resize (n - 1) $ (<>) <$> genConstraints <*> genConstraints)
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

fromCardanoMetadata :: TxMetadataValue -> Chain.Metadata
fromCardanoMetadata = \case
  TxMetaMap ms -> Chain.MetadataMap $ bimap fromCardanoMetadata fromCardanoMetadata <$> ms
  TxMetaList ms -> Chain.MetadataList $ fromCardanoMetadata <$> ms
  TxMetaNumber i -> Chain.MetadataNumber i
  TxMetaBytes bs -> Chain.MetadataBytes bs
  TxMetaText t -> Chain.MetadataText t

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

genScriptOutput :: Chain.Address -> TxConstraints 'V1 -> Gen (Maybe (TransactionScriptOutput 'V1))
genScriptOutput address TxConstraints{..} = case marloweInputConstraints of
  MarloweInputConstraintsNone -> oneof
    [ pure Nothing
    , Just <$> (TransactionScriptOutput address <$> genOutAssets <*> genTxOutRef <*> genDatum)
    ]
  MarloweInput {} -> Just <$> (TransactionScriptOutput address <$> genOutAssets <*> genTxOutRef <*> genDatum)

genPayoutOutputs :: Chain.Address -> TxConstraints 'V1 -> Gen (Map Chain.TxOutRef (Payout 'V1))
genPayoutOutputs address TxConstraints{..} = (<>) <$> required <*> extra
  where
    required = Map.fromList . concat <$> traverse (genPayouts address) (Set.toList payoutInputConstraints)
    extra = Map.fromList . concat <$> listOf (genPayouts address =<< genAssetId)

genPayouts :: Chain.Address -> Chain.AssetId -> Gen [(Chain.TxOutRef, Payout 'V1)]
genPayouts address datum = listOf1 $ genPayout address datum

genPayout :: Chain.Address -> Chain.AssetId -> Gen (Chain.TxOutRef, Payout 'V1)
genPayout address datum = do
  assets <- genOutAssets
  (,Payout{..}) <$> genTxOutRef

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
