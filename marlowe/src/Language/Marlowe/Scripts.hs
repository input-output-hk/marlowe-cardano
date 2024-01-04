{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Marlowe.Scripts where

import Cardano.Api (PlutusScript, PlutusScriptV2)
import Language.Marlowe.Scripts.Types (readPlutusScript)

marloweValidator :: PlutusScript PlutusScriptV2
marloweValidator = $(readPlutusScript "scripts/marlowe-semantics.plutus")

payoutValidator :: PlutusScript PlutusScriptV2
payoutValidator = $(readPlutusScript "scripts/marlowe-rolepayout.plutus")

openRolesValidator :: PlutusScript PlutusScriptV2
openRolesValidator = $(readPlutusScript "scripts/open-role.plutus")
