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
      identifier = { name = "cardano-ledger-test"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 Input Output (Hong Kong) Ltd.";
      maintainer = "nicholas.clarke@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "Testing harness, tests and benchmarks for Shelley style cardano ledgers";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
          (hsPkgs."cardano-ledger-babbage" or (errorHandler.buildDepError "cardano-ledger-babbage"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-pretty" or (errorHandler.buildDepError "cardano-ledger-pretty"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."genvalidity" or (errorHandler.buildDepError "genvalidity"))
          (hsPkgs."genvalidity-scientific" or (errorHandler.buildDepError "genvalidity-scientific"))
          (hsPkgs."groups" or (errorHandler.buildDepError "groups"))
          (hsPkgs."hkd" or (errorHandler.buildDepError "hkd"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."monad-supply" or (errorHandler.buildDepError "monad-supply"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."QuickCheck-GenT" or (errorHandler.buildDepError "QuickCheck-GenT"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-ledger-babbage-test" or (errorHandler.buildDepError "cardano-ledger-babbage-test"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          (hsPkgs."set-algebra" or (errorHandler.buildDepError "set-algebra"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."writer-cps-mtl" or (errorHandler.buildDepError "writer-cps-mtl"))
          ];
        buildable = true;
        modules = [
          "Data/Group/GrpMap"
          "Data/Functor/PiecewiseConstant"
          "Test/Cardano/Ledger/Orphans"
          "Test/Cardano/Ledger/Alonzo/Tools"
          "Test/Cardano/Ledger/BaseTypes"
          "Test/Cardano/Ledger/Examples/BabbageFeatures"
          "Test/Cardano/Ledger/Examples/TwoPhaseValidation"
          "Test/Cardano/Ledger/Generic/AggPropTests"
          "Test/Cardano/Ledger/Generic/ApplyTx"
          "Test/Cardano/Ledger/Generic/Indexed"
          "Test/Cardano/Ledger/Generic/Fields"
          "Test/Cardano/Ledger/Generic/Functions"
          "Test/Cardano/Ledger/Generic/GenState"
          "Test/Cardano/Ledger/Generic/TxGen"
          "Test/Cardano/Ledger/Generic/Types"
          "Test/Cardano/Ledger/Generic/Proof"
          "Test/Cardano/Ledger/Generic/MockChain"
          "Test/Cardano/Ledger/Generic/ModelState"
          "Test/Cardano/Ledger/Generic/PrettyCore"
          "Test/Cardano/Ledger/Generic/Properties"
          "Test/Cardano/Ledger/Generic/Scriptic"
          "Test/Cardano/Ledger/Generic/Trace"
          "Test/Cardano/Ledger/Generic/Updaters"
          "Test/Cardano/Ledger/Model/API"
          "Test/Cardano/Ledger/Model/Acnt"
          "Test/Cardano/Ledger/Model/BaseTypes"
          "Test/Cardano/Ledger/Model/Elaborators"
          "Test/Cardano/Ledger/Model/Elaborators/Alonzo"
          "Test/Cardano/Ledger/Model/Elaborators/Shelley"
          "Test/Cardano/Ledger/Model/FeatureSet"
          "Test/Cardano/Ledger/Model/Fixup"
          "Test/Cardano/Ledger/Model/Generators"
          "Test/Cardano/Ledger/Model/Generators/Address"
          "Test/Cardano/Ledger/Model/Generators/Certificates"
          "Test/Cardano/Ledger/Model/Generators/Chain"
          "Test/Cardano/Ledger/Model/Generators/Script"
          "Test/Cardano/Ledger/Model/Generators/Shrinking"
          "Test/Cardano/Ledger/Model/Generators/Tx"
          "Test/Cardano/Ledger/Model/Generators/TxOut"
          "Test/Cardano/Ledger/Model/Generators/Value"
          "Test/Cardano/Ledger/Model/LedgerState"
          "Test/Cardano/Ledger/Model/PParams"
          "Test/Cardano/Ledger/Model/Properties"
          "Test/Cardano/Ledger/Model/Properties/Utils"
          "Test/Cardano/Ledger/Model/Prov"
          "Test/Cardano/Ledger/Model/Rewards"
          "Test/Cardano/Ledger/Model/Rules"
          "Test/Cardano/Ledger/Model/Script"
          "Test/Cardano/Ledger/Model/Snapshot"
          "Test/Cardano/Ledger/Model/Tx"
          "Test/Cardano/Ledger/Model/TxOut"
          "Test/Cardano/Ledger/Model/UTxO"
          "Test/Cardano/Ledger/Model/Value"
          "Test/Cardano/Ledger/Rational"
          "Test/Cardano/Ledger/TestableEra"
          "Test/Cardano/Ledger/ValueFromList"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "cardano-ledger-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-test" or (errorHandler.buildDepError "cardano-ledger-test"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Tests.hs" ];
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."vector-map" or (errorHandler.buildDepError "vector-map"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Bench/Cardano/Ledger/ApplyTx"
            "Bench/Cardano/Ledger/ApplyTx/Gen"
            "Bench/Cardano/Ledger/Balance"
            "Bench/Cardano/Ledger/EpochBoundary"
            "Bench/Cardano/Ledger/Serialisation/Generators"
            "Bench/Cardano/Ledger/SumStake"
            "Bench/Cardano/Ledger/TxOut"
            ];
          hsSourceDirs = [ "bench" ];
          };
        "benchProperty" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-alonzo-test" or (errorHandler.buildDepError "cardano-ledger-alonzo-test"))
            (hsPkgs."cardano-ledger-shelley-ma-test" or (errorHandler.buildDepError "cardano-ledger-shelley-ma-test"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "benchProperty" ];
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
    postUnpack = "sourceRoot+=/libs/cardano-ledger-test; echo source root reset to $sourceRoot";
    }