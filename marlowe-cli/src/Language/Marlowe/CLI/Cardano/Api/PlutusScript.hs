{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Additional conversion functions for `PlutusScript` plus a copy of not exposed `IsPlutusScriptLanguage` class.
module Language.Marlowe.CLI.Cardano.Api.PlutusScript (
  IsPlutusScriptLanguage (..),
  toScript,
  toScriptLanguageInEra,
  withPlutusScriptVersion,
) where

import Cardano.Api (
  IsScriptLanguage,
  PlutusScriptV1,
  PlutusScriptV2,
  PlutusScriptVersion (..),
  Script (..),
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (PlutusScript)
import Language.Marlowe.CLI.Orphans ()

withPlutusScriptVersion :: PlutusScriptVersion lang -> ((IsPlutusScriptLanguage lang) => a) -> a
withPlutusScriptVersion PlutusScriptV1 = id
withPlutusScriptVersion PlutusScriptV2 = id
-- FIXME update with next cardano-api
withPlutusScriptVersion PlutusScriptV3 = const $ error "unsupported until cardano-api exposes PlutusScriptV3"

class (IsScriptLanguage lang) => IsPlutusScriptLanguage lang where
  plutusScriptVersion :: PlutusScriptVersion lang

instance IsPlutusScriptLanguage PlutusScriptV1 where
  plutusScriptVersion = PlutusScriptV1

instance IsPlutusScriptLanguage PlutusScriptV2 where
  plutusScriptVersion = PlutusScriptV2

toScript :: forall lang. (IsPlutusScriptLanguage lang) => PlutusScript lang -> Script lang
toScript = PlutusScript (plutusScriptVersion :: PlutusScriptVersion lang)

toScriptLanguageInEra
  :: forall era lang
   . (IsPlutusScriptLanguage lang)
  => C.ScriptDataSupportedInEra era
  -> Maybe (C.ScriptLanguageInEra lang era)
toScriptLanguageInEra = case plutusScriptVersion @lang of
  PlutusScriptV1 -> Just . toPlutusScriptV1LanguageInEra
  PlutusScriptV2 -> toPlutusScriptV2LanguageInEra
  PlutusScriptV3 -> const Nothing
  where
    toPlutusScriptV1LanguageInEra :: C.ScriptDataSupportedInEra era -> C.ScriptLanguageInEra PlutusScriptV1 era
    toPlutusScriptV1LanguageInEra = \case
      C.ScriptDataInAlonzoEra -> C.PlutusScriptV1InAlonzo
      C.ScriptDataInBabbageEra -> C.PlutusScriptV1InBabbage
      C.ScriptDataInConwayEra -> C.PlutusScriptV1InConway

    toPlutusScriptV2LanguageInEra :: C.ScriptDataSupportedInEra era -> Maybe (C.ScriptLanguageInEra PlutusScriptV2 era)
    toPlutusScriptV2LanguageInEra = \case
      C.ScriptDataInAlonzoEra -> Nothing
      C.ScriptDataInBabbageEra -> Just C.PlutusScriptV2InBabbage
      C.ScriptDataInConwayEra -> Just C.PlutusScriptV2InConway
