{-# LANGUAGE GADTs #-}
module Language.Marlowe.CLI.Plutus.Script.Utils
  where

import qualified Plutus.Script.Utils.V1.Typed.Scripts as V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as V2
-- These types are common to Plutus V1 and V2
import Cardano.Api (PlutusScriptV1)
import Cardano.Api.Byron (PlutusScriptV2)
import Plutus.V2.Ledger.Api (Validator, ValidatorHash)

data TypedValidator' lang t where
  TypedValidatorV1 :: V1.TypedValidator t -> TypedValidator' PlutusScriptV1 t
  TypedValidatorV2 :: V2.TypedValidator t -> TypedValidator' PlutusScriptV2 t


validatorHash :: TypedValidator' lang t -> ValidatorHash
validatorHash (TypedValidatorV1 v) = V1.validatorHash v
validatorHash (TypedValidatorV2 v) = V2.validatorHash v

validatorScript :: TypedValidator' lang t -> Validator
validatorScript (TypedValidatorV1 v) = V1.validatorScript v
validatorScript (TypedValidatorV2 v) = V2.validatorScript v

