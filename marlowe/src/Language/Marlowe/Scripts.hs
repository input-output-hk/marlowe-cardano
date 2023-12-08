{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Marlowe.Scripts where

import Cardano.Api (PlutusScript, PlutusScriptV2)
import Language.Marlowe.Scripts.Types (readPlutusScript)

marloweValidator :: PlutusScript PlutusScriptV2
marloweValidator = $(readPlutusScript "marlowe/scripts/marlowe-semantics.plutus")

payoutValidator :: PlutusScript PlutusScriptV2
payoutValidator = $(readPlutusScript "marlowe/scripts/marlowe-rolepayout.plutus")

openRolesValidator :: PlutusScript PlutusScriptV2
openRolesValidator = $(readPlutusScript "marlowe/scripts/open-role.plutus")
