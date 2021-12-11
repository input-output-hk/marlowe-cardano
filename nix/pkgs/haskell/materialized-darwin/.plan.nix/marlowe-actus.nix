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
      maintainer = "dmytro.kondratiuk@iohk.io";
      author = "Dmytro Kondratiuk";
      homepage = "";
      url = "";
      synopsis = "Marlowe ACTUS: standardised financial contracts on Cardano Computation Layer";
      description = "implementation of ACTUS contracts on Marlowe";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."sort" or (errorHandler.buildDepError "sort"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          ];
        buildable = true;
        modules = [
          "Language/Marlowe/ACTUS/Domain/BusinessEvents"
          "Language/Marlowe/ACTUS/Domain/ContractTerms"
          "Language/Marlowe/ACTUS/Domain/ContractState"
          "Language/Marlowe/ACTUS/Domain/Ops"
          "Language/Marlowe/ACTUS/Domain/Schedule"
          "Language/Marlowe/ACTUS/Generator/Analysis"
          "Language/Marlowe/ACTUS/Generator/Generator"
          "Language/Marlowe/ACTUS/Generator/GeneratorFs"
          "Language/Marlowe/ACTUS/Generator/GeneratorStatic"
          "Language/Marlowe/ACTUS/Generator/MarloweCompat"
          "Language/Marlowe/ACTUS/Model/Applicability"
          "Language/Marlowe/ACTUS/Model/ContractSchedule"
          "Language/Marlowe/ACTUS/Model/StateInitialization"
          "Language/Marlowe/ACTUS/Model/StateTransition"
          "Language/Marlowe/ACTUS/Model/Payoff"
          "Language/Marlowe/ACTUS/Utility/ANN/Annuity"
          "Language/Marlowe/ACTUS/Utility/DateShift"
          "Language/Marlowe/ACTUS/Utility/ScheduleGenerator"
          "Language/Marlowe/ACTUS/Utility/YearFraction"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "marlowe-actus-test" = {
          depends = [
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-contract" or (errorHandler.buildDepError "plutus-contract"))
            (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
            (hsPkgs."marlowe-actus" or (errorHandler.buildDepError "marlowe-actus"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          modules = [
            "Spec/Marlowe/ACTUS/Examples"
            "Spec/Marlowe/ACTUS/TestFramework"
            "Spec/Marlowe/ACTUS/QCGenerator"
            "Spec/Marlowe/ACTUS/QCTests"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-actus; }