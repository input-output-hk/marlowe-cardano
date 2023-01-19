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
      specVersion = "2.2";
      identifier = { name = "marlowe-actus"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "Yves Hauser <yves.hauser@iohk.io>";
      author = "Dmytro Kondratiuk, Yves Hauser";
      homepage = "";
      url = "";
      synopsis = "Marlowe ACTUS: Standardised financial contracts with Marlowe";
      description = "/marlowe-actus/ is an implementation of the [ACTUS](https://www.actusfrf.org)\nspecification in Haskell together with generators for Marlowe contracts.\n\nACTUS is a [taxonomy](https://www.actusfrf.org/taxonomy) of financial contracts that\nuniformly specifies projected cash flows per contract type. A contract is evolved\nover time, i.e. state changes are triggered from event schedules and performed by\nstate transformation functions, projected cash flows are determined by payoff\nfunctions. Future contract state and payoff might depend on observable values, like\nuser provided input (a choice for example) or market values\nobserved by an Oracle.\n\nThe implementation is tested against the [reference test cases](https://github.com/actusfrf/actus-tests) provided by the\nACTUS foundation.";
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
          (hsPkgs."marlowe-actus".components.sublibs.actus-core or (errorHandler.buildDepError "marlowe-actus:actus-core"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          ];
        buildable = true;
        modules = [ "Actus/Marlowe/Instance" "Actus/Marlowe" ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "actus-core" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."sort" or (errorHandler.buildDepError "sort"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            ];
          buildable = true;
          modules = [
            "Actus/Domain/BusinessEvents"
            "Actus/Domain/ContractTerms"
            "Actus/Domain/ContractState"
            "Actus/Domain/Schedule"
            "Actus/Model/Applicability"
            "Actus/Model/ContractSchedule"
            "Actus/Model/Payoff"
            "Actus/Model/StateInitialization"
            "Actus/Model/StateTransition"
            "Actus/Utility/ANN/Annuity"
            "Actus/Utility/DateShift"
            "Actus/Utility/ScheduleGenerator"
            "Actus/Utility/YearFraction"
            "Actus/Core"
            "Actus/Domain"
            "Actus/Model"
            "Actus/Utility"
            ];
          hsSourceDirs = [ "actus-core/src" ];
          };
        };
      tests = {
        "actus-core-test" = {
          depends = [
            (hsPkgs."marlowe-actus".components.sublibs.actus-core or (errorHandler.buildDepError "marlowe-actus:actus-core"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."sort" or (errorHandler.buildDepError "sort"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            ];
          buildable = true;
          modules = [ "Spec/TestFramework" ];
          hsSourceDirs = [ "actus-core/test" ];
          mainPath = [ "Spec.hs" ];
          };
        "marlowe-actus-test" = {
          depends = [
            (hsPkgs."marlowe-actus".components.sublibs.actus-core or (errorHandler.buildDepError "marlowe-actus:actus-core"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-actus" or (errorHandler.buildDepError "marlowe-actus"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."sort" or (errorHandler.buildDepError "sort"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            ];
          buildable = true;
          modules = [ "Spec/Actus/Examples" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-actus; }