{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Conversion and casting utilities for Cardano feature objects that use
-- CardanoEra and ShelleyBasedEra tags.
module Language.Marlowe.Runtime.Cardano.Feature where

import Cardano.Api
import Cardano.Api.Shelley
import Data.Functor.Product (Product (..))
import Data.Functor.These (These1 (..))

withCardanoEra :: forall era f r. (CardanoFeature f) => f era -> ((IsCardanoEra era) => r) -> r
withCardanoEra f = case cardanoEraOfFeature f of
  ByronEra -> id
  ShelleyEra -> id
  AllegraEra -> id
  MaryEra -> id
  AlonzoEra -> id
  BabbageEra -> id
  ConwayEra -> id

withShelleyBasedEra :: forall era f r. (ShelleyFeature f) => f era -> ((IsShelleyBasedEra era) => r) -> r
withShelleyBasedEra f = case shelleyBasedEraOfFeature f of
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary -> id
  ShelleyBasedEraAlonzo -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway -> id

class CardanoFeature f where
  featureInCardanoEra :: CardanoEra era -> Maybe (f era)
  cardanoEraOfFeature :: f era -> CardanoEra era

class (CardanoFeature f) => ShelleyFeature f where
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
    ConwayEra -> Just ShelleyBasedEraConway
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
    CollateralInConwayEra -> ConwayEra

instance ShelleyFeature CollateralSupportedInEra where
  shelleyBasedEraOfFeature = \case
    CollateralInAlonzoEra -> ShelleyBasedEraAlonzo
    CollateralInBabbageEra -> ShelleyBasedEraBabbage
    CollateralInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature MultiAssetSupportedInEra where
  featureInCardanoEra = hush . multiAssetSupportedInEra
  cardanoEraOfFeature = \case
    MultiAssetInMaryEra -> MaryEra
    MultiAssetInAlonzoEra -> AlonzoEra
    MultiAssetInBabbageEra -> BabbageEra
    MultiAssetInConwayEra -> ConwayEra

instance ShelleyFeature MultiAssetSupportedInEra where
  shelleyBasedEraOfFeature = \case
    MultiAssetInMaryEra -> ShelleyBasedEraMary
    MultiAssetInAlonzoEra -> ShelleyBasedEraAlonzo
    MultiAssetInBabbageEra -> ShelleyBasedEraBabbage
    MultiAssetInConwayEra -> ShelleyBasedEraConway

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
    TxFeesExplicitInConwayEra -> ConwayEra

instance ShelleyFeature TxFeesExplicitInEra where
  shelleyBasedEraOfFeature = \case
    TxFeesExplicitInShelleyEra -> ShelleyBasedEraShelley
    TxFeesExplicitInAllegraEra -> ShelleyBasedEraAllegra
    TxFeesExplicitInMaryEra -> ShelleyBasedEraMary
    TxFeesExplicitInAlonzoEra -> ShelleyBasedEraAlonzo
    TxFeesExplicitInBabbageEra -> ShelleyBasedEraBabbage
    TxFeesExplicitInConwayEra -> ShelleyBasedEraConway

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
    ValidityUpperBoundInConwayEra -> ConwayEra

instance ShelleyFeature ValidityUpperBoundSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ValidityUpperBoundInShelleyEra -> ShelleyBasedEraShelley
    ValidityUpperBoundInAllegraEra -> ShelleyBasedEraAllegra
    ValidityUpperBoundInMaryEra -> ShelleyBasedEraMary
    ValidityUpperBoundInAlonzoEra -> ShelleyBasedEraAlonzo
    ValidityUpperBoundInBabbageEra -> ShelleyBasedEraBabbage
    ValidityUpperBoundInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature ValidityNoUpperBoundSupportedInEra where
  featureInCardanoEra = validityNoUpperBoundSupportedInEra
  cardanoEraOfFeature = \case
    ValidityNoUpperBoundInByronEra -> ByronEra
    ValidityNoUpperBoundInAllegraEra -> AllegraEra
    ValidityNoUpperBoundInMaryEra -> MaryEra
    ValidityNoUpperBoundInAlonzoEra -> AlonzoEra
    ValidityNoUpperBoundInBabbageEra -> BabbageEra
    ValidityNoUpperBoundInConwayEra -> ConwayEra

instance CardanoFeature ValidityLowerBoundSupportedInEra where
  featureInCardanoEra = validityLowerBoundSupportedInEra
  cardanoEraOfFeature = \case
    ValidityLowerBoundInAllegraEra -> AllegraEra
    ValidityLowerBoundInMaryEra -> MaryEra
    ValidityLowerBoundInAlonzoEra -> AlonzoEra
    ValidityLowerBoundInBabbageEra -> BabbageEra
    ValidityLowerBoundInConwayEra -> ConwayEra

instance ShelleyFeature ValidityLowerBoundSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ValidityLowerBoundInAllegraEra -> ShelleyBasedEraAllegra
    ValidityLowerBoundInMaryEra -> ShelleyBasedEraMary
    ValidityLowerBoundInAlonzoEra -> ShelleyBasedEraAlonzo
    ValidityLowerBoundInBabbageEra -> ShelleyBasedEraBabbage
    ValidityLowerBoundInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature TxMetadataSupportedInEra where
  featureInCardanoEra = txMetadataSupportedInEra
  cardanoEraOfFeature = \case
    TxMetadataInShelleyEra -> ShelleyEra
    TxMetadataInAllegraEra -> AllegraEra
    TxMetadataInMaryEra -> MaryEra
    TxMetadataInAlonzoEra -> AlonzoEra
    TxMetadataInBabbageEra -> BabbageEra
    TxMetadataInConwayEra -> ConwayEra

instance ShelleyFeature TxMetadataSupportedInEra where
  shelleyBasedEraOfFeature = \case
    TxMetadataInShelleyEra -> ShelleyBasedEraShelley
    TxMetadataInAllegraEra -> ShelleyBasedEraAllegra
    TxMetadataInMaryEra -> ShelleyBasedEraMary
    TxMetadataInAlonzoEra -> ShelleyBasedEraAlonzo
    TxMetadataInBabbageEra -> ShelleyBasedEraBabbage
    TxMetadataInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature AuxScriptsSupportedInEra where
  featureInCardanoEra = auxScriptsSupportedInEra
  cardanoEraOfFeature = \case
    AuxScriptsInAllegraEra -> AllegraEra
    AuxScriptsInMaryEra -> MaryEra
    AuxScriptsInAlonzoEra -> AlonzoEra
    AuxScriptsInBabbageEra -> BabbageEra
    AuxScriptsInConwayEra -> ConwayEra

instance ShelleyFeature AuxScriptsSupportedInEra where
  shelleyBasedEraOfFeature = \case
    AuxScriptsInAllegraEra -> ShelleyBasedEraAllegra
    AuxScriptsInMaryEra -> ShelleyBasedEraMary
    AuxScriptsInAlonzoEra -> ShelleyBasedEraAlonzo
    AuxScriptsInBabbageEra -> ShelleyBasedEraBabbage
    AuxScriptsInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature TxExtraKeyWitnessesSupportedInEra where
  featureInCardanoEra = \case
    ByronEra -> Nothing
    ShelleyEra -> Nothing
    AllegraEra -> Nothing
    MaryEra -> Nothing
    AlonzoEra -> Just ExtraKeyWitnessesInAlonzoEra
    BabbageEra -> Just ExtraKeyWitnessesInBabbageEra
    ConwayEra -> Just ExtraKeyWitnessesInConwayEra
  cardanoEraOfFeature = \case
    ExtraKeyWitnessesInAlonzoEra -> AlonzoEra
    ExtraKeyWitnessesInBabbageEra -> BabbageEra
    ExtraKeyWitnessesInConwayEra -> ConwayEra

instance ShelleyFeature TxExtraKeyWitnessesSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ExtraKeyWitnessesInAlonzoEra -> ShelleyBasedEraAlonzo
    ExtraKeyWitnessesInBabbageEra -> ShelleyBasedEraBabbage
    ExtraKeyWitnessesInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature ScriptDataSupportedInEra where
  featureInCardanoEra = scriptDataSupportedInEra
  cardanoEraOfFeature = \case
    ScriptDataInAlonzoEra -> AlonzoEra
    ScriptDataInBabbageEra -> BabbageEra
    ScriptDataInConwayEra -> ConwayEra

instance ShelleyFeature ScriptDataSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ScriptDataInAlonzoEra -> ShelleyBasedEraAlonzo
    ScriptDataInBabbageEra -> ShelleyBasedEraBabbage
    ScriptDataInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature WithdrawalsSupportedInEra where
  featureInCardanoEra = withdrawalsSupportedInEra
  cardanoEraOfFeature = \case
    WithdrawalsInShelleyEra -> ShelleyEra
    WithdrawalsInAllegraEra -> AllegraEra
    WithdrawalsInMaryEra -> MaryEra
    WithdrawalsInAlonzoEra -> AlonzoEra
    WithdrawalsInBabbageEra -> BabbageEra
    WithdrawalsInConwayEra -> ConwayEra

instance ShelleyFeature WithdrawalsSupportedInEra where
  shelleyBasedEraOfFeature = \case
    WithdrawalsInShelleyEra -> ShelleyBasedEraShelley
    WithdrawalsInAllegraEra -> ShelleyBasedEraAllegra
    WithdrawalsInMaryEra -> ShelleyBasedEraMary
    WithdrawalsInAlonzoEra -> ShelleyBasedEraAlonzo
    WithdrawalsInBabbageEra -> ShelleyBasedEraBabbage
    WithdrawalsInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature CertificatesSupportedInEra where
  featureInCardanoEra = certificatesSupportedInEra
  cardanoEraOfFeature = \case
    CertificatesInShelleyEra -> ShelleyEra
    CertificatesInAllegraEra -> AllegraEra
    CertificatesInMaryEra -> MaryEra
    CertificatesInAlonzoEra -> AlonzoEra
    CertificatesInBabbageEra -> BabbageEra
    CertificatesInConwayEra -> ConwayEra

instance ShelleyFeature CertificatesSupportedInEra where
  shelleyBasedEraOfFeature = \case
    CertificatesInShelleyEra -> ShelleyBasedEraShelley
    CertificatesInAllegraEra -> ShelleyBasedEraAllegra
    CertificatesInMaryEra -> ShelleyBasedEraMary
    CertificatesInAlonzoEra -> ShelleyBasedEraAlonzo
    CertificatesInBabbageEra -> ShelleyBasedEraBabbage
    CertificatesInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature UpdateProposalSupportedInEra where
  featureInCardanoEra = updateProposalSupportedInEra
  cardanoEraOfFeature = \case
    UpdateProposalInShelleyEra -> ShelleyEra
    UpdateProposalInAllegraEra -> AllegraEra
    UpdateProposalInMaryEra -> MaryEra
    UpdateProposalInAlonzoEra -> AlonzoEra
    UpdateProposalInBabbageEra -> BabbageEra
    UpdateProposalInConwayEra -> ConwayEra

instance ShelleyFeature UpdateProposalSupportedInEra where
  shelleyBasedEraOfFeature = \case
    UpdateProposalInShelleyEra -> ShelleyBasedEraShelley
    UpdateProposalInAllegraEra -> ShelleyBasedEraAllegra
    UpdateProposalInMaryEra -> ShelleyBasedEraMary
    UpdateProposalInAlonzoEra -> ShelleyBasedEraAlonzo
    UpdateProposalInBabbageEra -> ShelleyBasedEraBabbage
    UpdateProposalInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature ReferenceTxInsScriptsInlineDatumsSupportedInEra where
  featureInCardanoEra = \case
    ByronEra -> Nothing
    ShelleyEra -> Nothing
    AllegraEra -> Nothing
    MaryEra -> Nothing
    AlonzoEra -> Nothing
    BabbageEra -> Just ReferenceTxInsScriptsInlineDatumsInBabbageEra
    ConwayEra -> Just ReferenceTxInsScriptsInlineDatumsInConwayEra
  cardanoEraOfFeature = \case
    ReferenceTxInsScriptsInlineDatumsInBabbageEra -> BabbageEra
    ReferenceTxInsScriptsInlineDatumsInConwayEra -> ConwayEra

instance ShelleyFeature ReferenceTxInsScriptsInlineDatumsSupportedInEra where
  shelleyBasedEraOfFeature = \case
    ReferenceTxInsScriptsInlineDatumsInBabbageEra -> ShelleyBasedEraBabbage
    ReferenceTxInsScriptsInlineDatumsInConwayEra -> ShelleyBasedEraConway

instance CardanoFeature (ScriptLanguageInEra SimpleScript') where
  featureInCardanoEra = \case
    ByronEra -> Nothing
    ShelleyEra -> Just SimpleScriptInShelley
    AllegraEra -> Just SimpleScriptInAllegra
    MaryEra -> Just SimpleScriptInMary
    AlonzoEra -> Just SimpleScriptInAlonzo
    BabbageEra -> Just SimpleScriptInBabbage
    ConwayEra -> Just SimpleScriptInConway
  cardanoEraOfFeature = \case
    SimpleScriptInShelley -> ShelleyEra
    SimpleScriptInAllegra -> AllegraEra
    SimpleScriptInMary -> MaryEra
    SimpleScriptInAlonzo -> AlonzoEra
    SimpleScriptInBabbage -> BabbageEra
    SimpleScriptInConway -> ConwayEra

instance ShelleyFeature (ScriptLanguageInEra SimpleScript') where
  shelleyBasedEraOfFeature = \case
    SimpleScriptInShelley -> ShelleyBasedEraShelley
    SimpleScriptInAllegra -> ShelleyBasedEraAllegra
    SimpleScriptInMary -> ShelleyBasedEraMary
    SimpleScriptInAlonzo -> ShelleyBasedEraAlonzo
    SimpleScriptInBabbage -> ShelleyBasedEraBabbage
    SimpleScriptInConway -> ShelleyBasedEraConway

instance CardanoFeature (ScriptLanguageInEra PlutusScriptV1) where
  featureInCardanoEra = \case
    ByronEra -> Nothing
    ShelleyEra -> Nothing
    AllegraEra -> Nothing
    MaryEra -> Nothing
    AlonzoEra -> Just PlutusScriptV1InAlonzo
    BabbageEra -> Just PlutusScriptV1InBabbage
    ConwayEra -> Just PlutusScriptV1InConway
  cardanoEraOfFeature = \case
    PlutusScriptV1InAlonzo -> AlonzoEra
    PlutusScriptV1InBabbage -> BabbageEra
    PlutusScriptV1InConway -> ConwayEra

instance ShelleyFeature (ScriptLanguageInEra PlutusScriptV1) where
  shelleyBasedEraOfFeature = \case
    PlutusScriptV1InAlonzo -> ShelleyBasedEraAlonzo
    PlutusScriptV1InBabbage -> ShelleyBasedEraBabbage
    PlutusScriptV1InConway -> ShelleyBasedEraConway

instance CardanoFeature (ScriptLanguageInEra PlutusScriptV2) where
  featureInCardanoEra = \case
    ByronEra -> Nothing
    ShelleyEra -> Nothing
    AllegraEra -> Nothing
    MaryEra -> Nothing
    AlonzoEra -> Nothing
    BabbageEra -> Just PlutusScriptV2InBabbage
    ConwayEra -> Just PlutusScriptV2InConway
  cardanoEraOfFeature = \case
    PlutusScriptV2InBabbage -> BabbageEra
    PlutusScriptV2InConway -> ConwayEra

instance ShelleyFeature (ScriptLanguageInEra PlutusScriptV2) where
  shelleyBasedEraOfFeature = \case
    PlutusScriptV2InBabbage -> ShelleyBasedEraBabbage
    PlutusScriptV2InConway -> ShelleyBasedEraConway

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

whine :: Either a b -> Maybe a
whine = either Just (const Nothing)

castInShelleyBasedEra :: (ShelleyFeature f, ShelleyFeature g) => f era -> Maybe (g era)
castInShelleyBasedEra = featureInShelleyBasedEra . shelleyBasedEraOfFeature

castInCardanoEra :: (CardanoFeature f, CardanoFeature g) => f era -> Maybe (g era)
castInCardanoEra = featureInCardanoEra . cardanoEraOfFeature
