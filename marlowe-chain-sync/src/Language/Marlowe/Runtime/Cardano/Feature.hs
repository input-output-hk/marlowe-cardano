{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Conversion and casting utilities for Cardano feature objects that use
-- CardanoEra and ShelleyBasedEra tags.
module Language.Marlowe.Runtime.Cardano.Feature where

import Cardano.Api
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
