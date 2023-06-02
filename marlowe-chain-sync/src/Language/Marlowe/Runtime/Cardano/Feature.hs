{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Conversion and casting utilities for Cardano feature objects that use
-- CardanoEra and ShelleyBasedEra tags.

module Language.Marlowe.Runtime.Cardano.Feature
  where

import Cardano.Api
import Cardano.Api.Shelley
import Data.Functor.Product (Product(..))
import Data.Functor.These (These1(..))

withCardanoEra :: forall era f r. CardanoFeature f => f era -> (IsCardanoEra era => r) -> r
withCardanoEra f = case cardanoEraOfFeature f of
  ByronEra -> id
  ShelleyEra -> id
  AllegraEra -> id
  MaryEra -> id
  AlonzoEra -> id
  BabbageEra -> id

withShelleyBasedEra :: forall era f r. ShelleyFeature f => f era -> (IsShelleyBasedEra era => r) -> r
withShelleyBasedEra f = case shelleyBasedEraOfFeature f of
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary -> id
  ShelleyBasedEraAlonzo -> id
  ShelleyBasedEraBabbage -> id

class CardanoFeature f where
  featureInCardanoEra :: CardanoEra era -> Maybe (f era)
  cardanoEraOfFeature :: f era -> CardanoEra era

class CardanoFeature f => ShelleyFeature f where
  featureInShelleyBasedEra :: ShelleyBasedEra era -> Maybe (f era)
  featureInShelleyBasedEra = featureInCardanoEra . shelleyBasedToCardanoEra
  shelleyBasedEraOfFeature :: f era -> ShelleyBasedEra era

instance CardanoFeature CardanoEra where
  featureInCardanoEra = Just
  cardanoEraOfFeature = id

instance CardanoFeature ShelleyBasedEra where
  featureInCardanoEra = \case
    ByronEra -> Nothing
    ShelleyEra -> Just ShelleyBasedEraShelley
    AllegraEra -> Just ShelleyBasedEraAllegra
    MaryEra -> Just ShelleyBasedEraMary
    AlonzoEra -> Just ShelleyBasedEraAlonzo
    BabbageEra -> Just ShelleyBasedEraBabbage
  cardanoEraOfFeature = shelleyBasedToCardanoEra

instance ShelleyFeature ShelleyBasedEra where
  featureInShelleyBasedEra = Just
  shelleyBasedEraOfFeature = id

-- The product of two Cardano features is a Cardano feature that is available
-- in eras where both features are available.
instance (CardanoFeature f, CardanoFeature g) => CardanoFeature (Product f g) where
  featureInCardanoEra era = Pair <$> featureInCardanoEra era <*> featureInCardanoEra era
  cardanoEraOfFeature (Pair f _) = cardanoEraOfFeature f

-- The non-exclusive sum of two Cardano features is a Cardano feature that is
-- available in eras when either feature is available.
instance (CardanoFeature f, CardanoFeature g) => CardanoFeature (These1 f g) where
  featureInCardanoEra era = case (featureInCardanoEra era, featureInCardanoEra era) of
    (Nothing, Nothing) -> Nothing
    (Just f, Nothing) -> Just $ This1 f
    (Nothing, Just g) -> Just $ That1 g
    (Just f, Just g) -> Just $ These1 f g
  cardanoEraOfFeature (This1 f) = cardanoEraOfFeature f
  cardanoEraOfFeature (That1 g) = cardanoEraOfFeature g
  cardanoEraOfFeature (These1 f _) = cardanoEraOfFeature f

instance (ShelleyFeature f, ShelleyFeature g) => ShelleyFeature (Product f g) where
  featureInShelleyBasedEra era = Pair <$> featureInShelleyBasedEra era <*> featureInShelleyBasedEra era
  shelleyBasedEraOfFeature (Pair f _) = shelleyBasedEraOfFeature f

instance (ShelleyFeature f, ShelleyFeature g) => ShelleyFeature (These1 f g) where
  featureInShelleyBasedEra era = case (featureInShelleyBasedEra era, featureInShelleyBasedEra era) of
    (Nothing, Nothing) -> Nothing
    (Just f, Nothing) -> Just $ This1 f
    (Nothing, Just g) -> Just $ That1 g
    (Just f, Just g) -> Just $ These1 f g
  shelleyBasedEraOfFeature (This1 f) = shelleyBasedEraOfFeature f
  shelleyBasedEraOfFeature (That1 g) = shelleyBasedEraOfFeature g
  shelleyBasedEraOfFeature (These1 f _) = shelleyBasedEraOfFeature f

instance CardanoFeature CollateralSupportedInEra where
  featureInCardanoEra = collateralSupportedInEra
  cardanoEraOfFeature = \case
    CollateralInAlonzoEra -> AlonzoEra
    CollateralInBabbageEra -> BabbageEra

instance ShelleyFeature CollateralSupportedInEra where
  shelleyBasedEraOfFeature = \case
    CollateralInAlonzoEra -> ShelleyBasedEraAlonzo
    CollateralInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature MultiAssetSupportedInEra where
  featureInCardanoEra = hush . multiAssetSupportedInEra
  cardanoEraOfFeature = \case
    MultiAssetInMaryEra -> MaryEra
    MultiAssetInAlonzoEra -> AlonzoEra
    MultiAssetInBabbageEra -> BabbageEra

instance ShelleyFeature MultiAssetSupportedInEra where
  shelleyBasedEraOfFeature = \case
    MultiAssetInMaryEra -> ShelleyBasedEraMary
    MultiAssetInAlonzoEra -> ShelleyBasedEraAlonzo
    MultiAssetInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature OnlyAdaSupportedInEra where
  featureInCardanoEra = whine . multiAssetSupportedInEra
  cardanoEraOfFeature = \case
    AdaOnlyInByronEra -> ByronEra
    AdaOnlyInShelleyEra -> ShelleyEra
    AdaOnlyInAllegraEra -> AllegraEra

instance CardanoFeature TxFeesExplicitInEra where
  featureInCardanoEra = hush . txFeesExplicitInEra
  cardanoEraOfFeature = \case
    TxFeesExplicitInShelleyEra -> ShelleyEra
    TxFeesExplicitInAllegraEra -> AllegraEra
    TxFeesExplicitInMaryEra -> MaryEra
    TxFeesExplicitInAlonzoEra -> AlonzoEra
    TxFeesExplicitInBabbageEra -> BabbageEra

instance ShelleyFeature TxFeesExplicitInEra where
  shelleyBasedEraOfFeature = \case
    TxFeesExplicitInShelleyEra -> ShelleyBasedEraShelley
    TxFeesExplicitInAllegraEra -> ShelleyBasedEraAllegra
    TxFeesExplicitInMaryEra -> ShelleyBasedEraMary
    TxFeesExplicitInAlonzoEra -> ShelleyBasedEraAlonzo
    TxFeesExplicitInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature TxFeesImplicitInEra where
  featureInCardanoEra = whine . txFeesExplicitInEra
  cardanoEraOfFeature = \case
    TxFeesImplicitInByronEra -> ByronEra

instance CardanoFeature ValidityUpperBoundSupportedInEra where
  featureInCardanoEra = validityUpperBoundSupportedInEra
  cardanoEraOfFeature = \case
    ValidityUpperBoundInShelleyEra -> ShelleyEra
    ValidityUpperBoundInAllegraEra -> AllegraEra
    ValidityUpperBoundInMaryEra -> MaryEra
    ValidityUpperBoundInAlonzoEra -> AlonzoEra
    ValidityUpperBoundInBabbageEra -> BabbageEra

instance ShelleyFeature ValidityUpperBoundSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ValidityUpperBoundInShelleyEra -> ShelleyBasedEraShelley
    ValidityUpperBoundInAllegraEra -> ShelleyBasedEraAllegra
    ValidityUpperBoundInMaryEra -> ShelleyBasedEraMary
    ValidityUpperBoundInAlonzoEra -> ShelleyBasedEraAlonzo
    ValidityUpperBoundInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature ValidityNoUpperBoundSupportedInEra where
  featureInCardanoEra = validityNoUpperBoundSupportedInEra
  cardanoEraOfFeature = \case
    ValidityNoUpperBoundInByronEra -> ByronEra
    ValidityNoUpperBoundInAllegraEra -> AllegraEra
    ValidityNoUpperBoundInMaryEra -> MaryEra
    ValidityNoUpperBoundInAlonzoEra -> AlonzoEra
    ValidityNoUpperBoundInBabbageEra -> BabbageEra

instance CardanoFeature ValidityLowerBoundSupportedInEra where
  featureInCardanoEra = validityLowerBoundSupportedInEra
  cardanoEraOfFeature = \case
    ValidityLowerBoundInAllegraEra -> AllegraEra
    ValidityLowerBoundInMaryEra -> MaryEra
    ValidityLowerBoundInAlonzoEra -> AlonzoEra
    ValidityLowerBoundInBabbageEra -> BabbageEra

instance ShelleyFeature ValidityLowerBoundSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ValidityLowerBoundInAllegraEra -> ShelleyBasedEraAllegra
    ValidityLowerBoundInMaryEra -> ShelleyBasedEraMary
    ValidityLowerBoundInAlonzoEra -> ShelleyBasedEraAlonzo
    ValidityLowerBoundInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature TxMetadataSupportedInEra where
  featureInCardanoEra = txMetadataSupportedInEra
  cardanoEraOfFeature = \case
    TxMetadataInShelleyEra -> ShelleyEra
    TxMetadataInAllegraEra -> AllegraEra
    TxMetadataInMaryEra -> MaryEra
    TxMetadataInAlonzoEra -> AlonzoEra
    TxMetadataInBabbageEra -> BabbageEra

instance ShelleyFeature TxMetadataSupportedInEra where
  shelleyBasedEraOfFeature = \case
    TxMetadataInShelleyEra -> ShelleyBasedEraShelley
    TxMetadataInAllegraEra -> ShelleyBasedEraAllegra
    TxMetadataInMaryEra -> ShelleyBasedEraMary
    TxMetadataInAlonzoEra -> ShelleyBasedEraAlonzo
    TxMetadataInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature AuxScriptsSupportedInEra where
  featureInCardanoEra = auxScriptsSupportedInEra
  cardanoEraOfFeature = \case
    AuxScriptsInAllegraEra -> AllegraEra
    AuxScriptsInMaryEra -> MaryEra
    AuxScriptsInAlonzoEra -> AlonzoEra
    AuxScriptsInBabbageEra -> BabbageEra

instance ShelleyFeature AuxScriptsSupportedInEra where
  shelleyBasedEraOfFeature = \case
    AuxScriptsInAllegraEra -> ShelleyBasedEraAllegra
    AuxScriptsInMaryEra -> ShelleyBasedEraMary
    AuxScriptsInAlonzoEra -> ShelleyBasedEraAlonzo
    AuxScriptsInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature TxExtraKeyWitnessesSupportedInEra where
  featureInCardanoEra = \case
    AlonzoEra -> Just ExtraKeyWitnessesInAlonzoEra
    BabbageEra -> Just ExtraKeyWitnessesInBabbageEra
    _ -> Nothing
  cardanoEraOfFeature = \case
    ExtraKeyWitnessesInAlonzoEra -> AlonzoEra
    ExtraKeyWitnessesInBabbageEra -> BabbageEra

instance ShelleyFeature TxExtraKeyWitnessesSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ExtraKeyWitnessesInAlonzoEra -> ShelleyBasedEraAlonzo
    ExtraKeyWitnessesInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature ScriptDataSupportedInEra where
  featureInCardanoEra = scriptDataSupportedInEra
  cardanoEraOfFeature = \case
    ScriptDataInAlonzoEra -> AlonzoEra
    ScriptDataInBabbageEra -> BabbageEra

instance ShelleyFeature ScriptDataSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ScriptDataInAlonzoEra -> ShelleyBasedEraAlonzo
    ScriptDataInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature WithdrawalsSupportedInEra where
  featureInCardanoEra = withdrawalsSupportedInEra
  cardanoEraOfFeature = \case
    WithdrawalsInShelleyEra -> ShelleyEra
    WithdrawalsInAllegraEra -> AllegraEra
    WithdrawalsInMaryEra -> MaryEra
    WithdrawalsInAlonzoEra -> AlonzoEra
    WithdrawalsInBabbageEra -> BabbageEra

instance ShelleyFeature WithdrawalsSupportedInEra where
  shelleyBasedEraOfFeature = \case
    WithdrawalsInShelleyEra -> ShelleyBasedEraShelley
    WithdrawalsInAllegraEra -> ShelleyBasedEraAllegra
    WithdrawalsInMaryEra -> ShelleyBasedEraMary
    WithdrawalsInAlonzoEra -> ShelleyBasedEraAlonzo
    WithdrawalsInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature CertificatesSupportedInEra where
  featureInCardanoEra = certificatesSupportedInEra
  cardanoEraOfFeature = \case
    CertificatesInShelleyEra -> ShelleyEra
    CertificatesInAllegraEra -> AllegraEra
    CertificatesInMaryEra -> MaryEra
    CertificatesInAlonzoEra -> AlonzoEra
    CertificatesInBabbageEra -> BabbageEra

instance ShelleyFeature CertificatesSupportedInEra where
  shelleyBasedEraOfFeature = \case
    CertificatesInShelleyEra -> ShelleyBasedEraShelley
    CertificatesInAllegraEra -> ShelleyBasedEraAllegra
    CertificatesInMaryEra -> ShelleyBasedEraMary
    CertificatesInAlonzoEra -> ShelleyBasedEraAlonzo
    CertificatesInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature UpdateProposalSupportedInEra where
  featureInCardanoEra = updateProposalSupportedInEra
  cardanoEraOfFeature = \case
    UpdateProposalInShelleyEra -> ShelleyEra
    UpdateProposalInAllegraEra -> AllegraEra
    UpdateProposalInMaryEra -> MaryEra
    UpdateProposalInAlonzoEra -> AlonzoEra
    UpdateProposalInBabbageEra -> BabbageEra

instance ShelleyFeature UpdateProposalSupportedInEra where
  shelleyBasedEraOfFeature = \case
    UpdateProposalInShelleyEra -> ShelleyBasedEraShelley
    UpdateProposalInAllegraEra -> ShelleyBasedEraAllegra
    UpdateProposalInMaryEra -> ShelleyBasedEraMary
    UpdateProposalInAlonzoEra -> ShelleyBasedEraAlonzo
    UpdateProposalInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature ReferenceTxInsScriptsInlineDatumsSupportedInEra where
  featureInCardanoEra = \case
    BabbageEra -> Just ReferenceTxInsScriptsInlineDatumsInBabbageEra
    _ -> Nothing
  cardanoEraOfFeature = \case
    ReferenceTxInsScriptsInlineDatumsInBabbageEra -> BabbageEra

instance ShelleyFeature ReferenceTxInsScriptsInlineDatumsSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ReferenceTxInsScriptsInlineDatumsInBabbageEra -> ShelleyBasedEraBabbage

instance CardanoFeature (ScriptLanguageInEra SimpleScriptV1) where
  featureInCardanoEra = \case
    ShelleyEra -> Just SimpleScriptV1InShelley
    AllegraEra -> Just SimpleScriptV1InAllegra
    MaryEra -> Just SimpleScriptV1InMary
    AlonzoEra -> Just SimpleScriptV1InAlonzo
    BabbageEra -> Just SimpleScriptV1InBabbage
    _ -> Nothing
  cardanoEraOfFeature = \case
    SimpleScriptV1InShelley -> ShelleyEra
    SimpleScriptV1InAllegra -> AllegraEra
    SimpleScriptV1InMary -> MaryEra
    SimpleScriptV1InAlonzo -> AlonzoEra
    SimpleScriptV1InBabbage -> BabbageEra

instance ShelleyFeature (ScriptLanguageInEra SimpleScriptV1) where
  shelleyBasedEraOfFeature = \case
    SimpleScriptV1InShelley -> ShelleyBasedEraShelley
    SimpleScriptV1InAllegra -> ShelleyBasedEraAllegra
    SimpleScriptV1InMary -> ShelleyBasedEraMary
    SimpleScriptV1InAlonzo -> ShelleyBasedEraAlonzo
    SimpleScriptV1InBabbage -> ShelleyBasedEraBabbage

instance CardanoFeature (ScriptLanguageInEra SimpleScriptV2) where
  featureInCardanoEra = \case
    AllegraEra -> Just SimpleScriptV2InAllegra
    MaryEra -> Just SimpleScriptV2InMary
    AlonzoEra -> Just SimpleScriptV2InAlonzo
    BabbageEra -> Just SimpleScriptV2InBabbage
    _ -> Nothing
  cardanoEraOfFeature = \case
    SimpleScriptV2InAllegra -> AllegraEra
    SimpleScriptV2InMary -> MaryEra
    SimpleScriptV2InAlonzo -> AlonzoEra
    SimpleScriptV2InBabbage -> BabbageEra

instance ShelleyFeature (ScriptLanguageInEra SimpleScriptV2) where
  shelleyBasedEraOfFeature = \case
    SimpleScriptV2InAllegra -> ShelleyBasedEraAllegra
    SimpleScriptV2InMary -> ShelleyBasedEraMary
    SimpleScriptV2InAlonzo -> ShelleyBasedEraAlonzo
    SimpleScriptV2InBabbage -> ShelleyBasedEraBabbage

instance CardanoFeature (ScriptLanguageInEra PlutusScriptV1) where
  featureInCardanoEra = \case
    AlonzoEra -> Just PlutusScriptV1InAlonzo
    BabbageEra -> Just PlutusScriptV1InBabbage
    _ -> Nothing
  cardanoEraOfFeature = \case
    PlutusScriptV1InAlonzo -> AlonzoEra
    PlutusScriptV1InBabbage -> BabbageEra

instance ShelleyFeature (ScriptLanguageInEra PlutusScriptV1) where
  shelleyBasedEraOfFeature = \case
    PlutusScriptV1InAlonzo -> ShelleyBasedEraAlonzo
    PlutusScriptV1InBabbage -> ShelleyBasedEraBabbage

instance CardanoFeature (ScriptLanguageInEra PlutusScriptV2) where
  featureInCardanoEra = \case
    BabbageEra -> Just PlutusScriptV2InBabbage
    _ -> Nothing
  cardanoEraOfFeature = \case
    PlutusScriptV2InBabbage -> BabbageEra

instance ShelleyFeature (ScriptLanguageInEra PlutusScriptV2) where
  shelleyBasedEraOfFeature = \case
    PlutusScriptV2InBabbage -> ShelleyBasedEraBabbage

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

whine :: Either a b -> Maybe a
whine = either Just (const Nothing)

castInShelleyBasedEra :: (ShelleyFeature f, ShelleyFeature g) => f era -> Maybe (g era)
castInShelleyBasedEra = featureInShelleyBasedEra . shelleyBasedEraOfFeature

castInCardanoEra :: (CardanoFeature f, CardanoFeature g) => f era -> Maybe (g era)
castInCardanoEra = featureInCardanoEra . cardanoEraOfFeature
