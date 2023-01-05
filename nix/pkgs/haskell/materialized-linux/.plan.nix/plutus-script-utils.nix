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
    flags = { defer-plugin-errors = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-script-utils"; version = "1.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "konstantinos.lambrou@iohk.io";
      author = "Konstantinos Lambrou-Latreille";
      homepage = "https://github.com/input-output-hk/plutus-apps#readme";
      url = "";
      synopsis = "Helper/utility functions for writing Plutus scripts.";
      description = "Helper/utility functions for writing Plutus scripts.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.adoc" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Plutus/Script/Utils/Scripts"
          "Plutus/Script/Utils/Typed"
          "Plutus/Script/Utils/V1/Address"
          "Plutus/Script/Utils/V1/Generators"
          "Plutus/Script/Utils/V1/Scripts"
          "Plutus/Script/Utils/V1/Tx"
          "Plutus/Script/Utils/V1/Typed/Scripts"
          "Plutus/Script/Utils/V1/Typed/Scripts/MonetaryPolicies"
          "Plutus/Script/Utils/V1/Typed/Scripts/StakeValidators"
          "Plutus/Script/Utils/V1/Typed/Scripts/Validators"
          "Plutus/Script/Utils/V2/Address"
          "Plutus/Script/Utils/V2/Contexts"
          "Plutus/Script/Utils/V2/Generators"
          "Plutus/Script/Utils/V2/Scripts"
          "Plutus/Script/Utils/V2/Tx"
          "Plutus/Script/Utils/V2/Typed/Scripts"
          "Plutus/Script/Utils/V2/Typed/Scripts/MonetaryPolicies"
          "Plutus/Script/Utils/V2/Typed/Scripts/StakeValidators"
          "Plutus/Script/Utils/V2/Typed/Scripts/Validators"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "plutus-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "3";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "3";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/plutus-script-utils; echo source root reset to $sourceRoot";
    }