-- | Additional conersion functions for `PlutusScript` plus a copy of not exposed `IsPlutusScriptLanguage` class.
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}

module Language.Marlowe.CLI.Cardano.Api.PlutusScript (
  fromV1TypedValidator
, fromV2TypedValidator
, fromTypedValidator
, IsPlutusScriptLanguage(..)
, toScript
, withPlutusScriptVersion
) where

import Cardano.Api (IsScriptLanguage, PlutusScriptV1, PlutusScriptV2,
                    PlutusScriptVersion (PlutusScriptV1, PlutusScriptV2), Script (PlutusScript))
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.CLI.Plutus.Script.Utils (TypedValidator' (TypedValidatorV1, TypedValidatorV2))
import qualified Plutus.Script.Utils.V2.Typed.Scripts as V1.Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as V2.Scripts
import qualified Plutus.V2.Ledger.Api as Plutus

withPlutusScriptVersion :: PlutusScriptVersion lang -> (IsPlutusScriptLanguage lang => a) -> a
withPlutusScriptVersion PlutusScriptV1 = id
withPlutusScriptVersion PlutusScriptV2 = id


class IsScriptLanguage lang => IsPlutusScriptLanguage lang where
    plutusScriptVersion :: PlutusScriptVersion lang

instance IsPlutusScriptLanguage PlutusScriptV1 where
    plutusScriptVersion = PlutusScriptV1

instance IsPlutusScriptLanguage PlutusScriptV2 where
    plutusScriptVersion = PlutusScriptV2


fromTypedValidator :: TypedValidator' lang t -> PlutusScript lang
fromTypedValidator (TypedValidatorV1 v) = fromV1TypedValidator v
fromTypedValidator (TypedValidatorV2 v) = fromV2TypedValidator v


fromV1TypedValidator :: V1.Scripts.TypedValidator t -> PlutusScript PlutusScriptV1
fromV1TypedValidator =
  PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . Plutus.getValidator
  . V1.Scripts.validatorScript


fromV2TypedValidator :: V2.Scripts.TypedValidator t -> PlutusScript PlutusScriptV2
fromV2TypedValidator =
  PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise
  . Plutus.getValidator
  . V2.Scripts.validatorScript


toScript :: forall lang. IsPlutusScriptLanguage lang => PlutusScript lang -> Script lang
toScript = PlutusScript (plutusScriptVersion :: PlutusScriptVersion lang)

