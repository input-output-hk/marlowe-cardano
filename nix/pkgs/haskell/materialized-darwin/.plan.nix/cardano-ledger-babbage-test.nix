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
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = {
        name = "cardano-ledger-babbage-test";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Tests for Cardano ledger babbage era";
      description = "This package builds upon the Alonzo ledger";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cddl-files/babbage.cddl"
        "cddl-files/real/crypto.cddl"
        "cddl-files/mock/extras.cddl"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Test/Cardano/Ledger/Babbage/Examples/Consensus"
          "Test/Cardano/Ledger/Babbage/Serialisation/Generators"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cardano-ledger-babbage-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
            (hsPkgs."cardano-ledger-babbage-test" or (errorHandler.buildDepError "cardano-ledger-babbage-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          modules = [
            "Test/Cardano/Ledger/Babbage/Serialisation/Tripping"
            "Test/Cardano/Ledger/Babbage/Serialisation/CDDL"
            "Test/Cardano/Ledger/Babbage/TxInfo"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "7";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "7";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/eras/babbage/test-suite; echo source root reset to $sourceRoot";
    }