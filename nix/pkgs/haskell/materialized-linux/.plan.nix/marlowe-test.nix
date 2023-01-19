{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { defer-plugin-errors = false; limit-static-analysis-time = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "marlowe-test"; version = "0.1.1.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "B W Bush <brian.bush@iokk.io>";
      author = "B W Bush <brian.bush@iokk.io>";
      homepage = "";
      url = "";
      synopsis = "Tests for Marlowe semantics and validators on Cardano";
      description = "Unit and integration tests for Marlowe semantics, serialization, and Plutus\nvalidators on the Cardano blockchain.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [
        "test/contract.json"
        "test/input.json"
        "test/state.json"
        "test/Spec/Marlowe/Serialization/golden/swap-contract.json"
        "test/Spec/Marlowe/Serialization/golden/swap-module.json"
        ];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hint" or (errorHandler.buildDepError "hint"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."marlowe-spec-test" or (errorHandler.buildDepError "marlowe-spec-test"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-script-utils" or (errorHandler.buildDepError "plutus-script-utils"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Data/Jsonable"
          "Spec/Marlowe/Common"
          "Spec/Marlowe/Marlowe"
          "Spec/Marlowe/Semantics"
          "Spec/Marlowe/Plutus"
          "Spec/Marlowe/Plutus/Arbitrary"
          "Spec/Marlowe/Plutus/AssocMap"
          "Spec/Marlowe/Plutus/Lens"
          "Spec/Marlowe/Plutus/Prelude"
          "Spec/Marlowe/Plutus/Script"
          "Spec/Marlowe/Plutus/ScriptContext"
          "Spec/Marlowe/Plutus/Specification"
          "Spec/Marlowe/Plutus/Transaction"
          "Spec/Marlowe/Plutus/Types"
          "Spec/Marlowe/Plutus/Value"
          "Spec/Marlowe/Serialization"
          "Spec/Marlowe/Serialization/CoreJson"
          "Spec/Marlowe/Serialization/ExtendedJson"
          "Spec/Marlowe/Service"
          "Spec/Marlowe/Service/Isabelle"
          "Spec/Marlowe/Service/Random"
          "Spec/Marlowe/Service/Serialization"
          "Spec/Marlowe/Service/Types"
          "Spec/Marlowe/Semantics/Arbitrary"
          "Spec/Marlowe/Semantics/AssocMap"
          "Spec/Marlowe/Semantics/Compute"
          "Spec/Marlowe/Semantics/Entropy"
          "Spec/Marlowe/Semantics/Functions"
          "Spec/Marlowe/Semantics/Golden"
          "Spec/Marlowe/Semantics/Golden/Escrow"
          "Spec/Marlowe/Semantics/Golden/Pangram"
          "Spec/Marlowe/Semantics/Golden/Swap"
          "Spec/Marlowe/Semantics/Golden/Trivial"
          "Spec/Marlowe/Semantics/Golden/ZeroCouponBond"
          "Spec/Marlowe/Semantics/Merkle"
          "Spec/Marlowe/Semantics/Orphans"
          "Spec/Marlowe/Semantics/Util"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "marlowe-spec-client" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."marlowe-test" or (errorHandler.buildDepError "marlowe-test"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            ];
          buildable = true;
          hsSourceDirs = [ "spec-client" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "marlowe-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."marlowe-test" or (errorHandler.buildDepError "marlowe-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-test; }