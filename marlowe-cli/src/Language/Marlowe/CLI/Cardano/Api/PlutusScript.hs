{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Additional conversion functions for `PlutusScript`
module Language.Marlowe.CLI.Cardano.Api.PlutusScript (
  toScript,
  toScriptLanguageInEra,
  withPlutusScriptVersion,
) where

import Cardano.Api (
  PlutusScriptV1,
  PlutusScriptV2,
  PlutusScriptV3,
  PlutusScriptVersion (..),
  Script (..),
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (PlutusScript)
import Language.Marlowe.CLI.Orphans ()

withPlutusScriptVersion :: PlutusScriptVersion lang -> ((C.IsPlutusScriptLanguage lang) => a) -> a
withPlutusScriptVersion PlutusScriptV1 = id
withPlutusScriptVersion PlutusScriptV2 = id
withPlutusScriptVersion PlutusScriptV3 = id

toScript :: forall lang. (C.IsPlutusScriptLanguage lang) => PlutusScript lang -> Script lang
toScript = PlutusScript (C.plutusScriptVersion @lang)

toScriptLanguageInEra
  :: forall era lang
   . (C.IsPlutusScriptLanguage lang)
  => C.BabbageEraOnwards era
  -> Maybe (C.ScriptLanguageInEra lang era)
toScriptLanguageInEra = case C.plutusScriptVersion @lang of
  PlutusScriptV1 -> Just . toPlutusScriptV1LanguageInEra
  PlutusScriptV2 -> Just . toPlutusScriptV2LanguageInEra
  PlutusScriptV3 -> toPlutusScriptV3LanguageInEra
  where
    toPlutusScriptV1LanguageInEra :: C.BabbageEraOnwards era -> C.ScriptLanguageInEra PlutusScriptV1 era
    toPlutusScriptV1LanguageInEra = \case
      C.BabbageEraOnwardsBabbage -> C.PlutusScriptV1InBabbage
      C.BabbageEraOnwardsConway -> C.PlutusScriptV1InConway

    toPlutusScriptV2LanguageInEra :: C.BabbageEraOnwards era -> C.ScriptLanguageInEra PlutusScriptV2 era
    toPlutusScriptV2LanguageInEra = \case
      C.BabbageEraOnwardsBabbage -> C.PlutusScriptV2InBabbage
      C.BabbageEraOnwardsConway -> C.PlutusScriptV2InConway

    toPlutusScriptV3LanguageInEra :: C.BabbageEraOnwards era -> Maybe (C.ScriptLanguageInEra PlutusScriptV3 era)
    toPlutusScriptV3LanguageInEra = \case
      C.BabbageEraOnwardsBabbage -> Nothing
      C.BabbageEraOnwardsConway -> Just C.PlutusScriptV3InConway
