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
      identifier = { name = "marlowe"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "alexander.nemish@iohk.io";
      author = "Alexander Nemish";
      homepage = "";
      url = "";
      synopsis = "Marlowe: financial contracts on Cardano Computation Layer";
      description = "A reference implementation of Marlowe, domain-specific language targeted at\nthe execution of financial contracts in the style of Peyton Jones et al\non Cardano Computation Layer.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-aeson" or (errorHandler.buildDepError "base16-aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."plutus-ledger-ada" or (errorHandler.buildDepError "plutus-ledger-ada"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-ledger-slot" or (errorHandler.buildDepError "plutus-ledger-slot"))
          (hsPkgs."plutus-script-utils" or (errorHandler.buildDepError "plutus-script-utils"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."wl-pprint" or (errorHandler.buildDepError "wl-pprint"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"));
        buildable = true;
        modules = [
          "Language/Marlowe"
          "Language/Marlowe/Extended/V1"
          "Language/Marlowe/Core/V1/Semantics"
          "Language/Marlowe/Core/V1/Semantics/Types"
          "Language/Marlowe/FindInputs"
          "Language/Marlowe/Client"
          "Language/Marlowe/Client/History"
          "Language/Marlowe/Util"
          "Language/Marlowe/ParserUtil"
          "Language/Marlowe/Scripts"
          "Language/Marlowe/Pretty"
          "Language/Marlowe/Analysis/FSSemantics"
          "Plutus/Debug"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "marlowe-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hint" or (errorHandler.buildDepError "hint"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-slot" or (errorHandler.buildDepError "plutus-ledger-slot"))
            (hsPkgs."plutus-script-utils" or (errorHandler.buildDepError "plutus-script-utils"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Spec/Marlowe/Common"
            "Spec/Marlowe/Marlowe"
            "Spec/Marlowe/Semantics"
            "Spec/Marlowe/Plutus"
            "Spec/Marlowe/Plutus/Arbitrary"
            "Spec/Marlowe/Plutus/Specification"
            "Spec/Marlowe/Plutus/Types"
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
            "Spec/Marlowe/Semantics/Orphans"
            "Spec/Marlowe/Semantics/Util"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe; }