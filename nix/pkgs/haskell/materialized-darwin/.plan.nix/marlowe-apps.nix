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
      identifier = { name = "marlowe-apps"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "Brian W Bush <brian.bush@iohk.io>";
      author = "Brian W Bush <brian.bush@iohk.io>";
      homepage = "";
      url = "";
      synopsis = "Marlowe Runtimee applications";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "ReadMe.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
          (hsPkgs."eventuo11y-dsl" or (errorHandler.buildDepError "eventuo11y-dsl"))
          (hsPkgs."eventuo11y-json" or (errorHandler.buildDepError "eventuo11y-json"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
          (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
          (hsPkgs."marlowe-runtime".components.sublibs.config or (errorHandler.buildDepError "marlowe-runtime:config"))
          (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
          (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
          (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
          (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
        buildable = true;
        modules = [
          "Language/Marlowe/Runtime/App"
          "Language/Marlowe/Runtime/App/Build"
          "Language/Marlowe/Runtime/App/Channel"
          "Language/Marlowe/Runtime/App/List"
          "Language/Marlowe/Runtime/App/Parser"
          "Language/Marlowe/Runtime/App/Run"
          "Language/Marlowe/Runtime/App/Sign"
          "Language/Marlowe/Runtime/App/Stream"
          "Language/Marlowe/Runtime/App/Submit"
          "Language/Marlowe/Runtime/App/Types"
          "Language/Marlowe/Runtime/App/Transact"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "marlowe-scaling" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-dsl" or (errorHandler.buildDepError "eventuo11y-dsl"))
            (hsPkgs."eventuo11y-json" or (errorHandler.buildDepError "eventuo11y-json"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-apps" or (errorHandler.buildDepError "marlowe-apps"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          hsSourceDirs = [ "scaling" ];
          mainPath = [ "Main.hs" ];
          };
        "marlowe-oracle" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-dsl" or (errorHandler.buildDepError "eventuo11y-dsl"))
            (hsPkgs."eventuo11y-json" or (errorHandler.buildDepError "eventuo11y-json"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-apps" or (errorHandler.buildDepError "marlowe-apps"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Oracle/Detect"
            "Language/Marlowe/Oracle/Process"
            "Network/Oracle"
            "Network/Oracle/CoinGecko"
            "Network/Oracle/Sofr"
            ];
          hsSourceDirs = [ "oracle" ];
          mainPath = [ "Main.hs" ];
          };
        "marlowe-finder" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-dsl" or (errorHandler.buildDepError "eventuo11y-dsl"))
            (hsPkgs."eventuo11y-json" or (errorHandler.buildDepError "eventuo11y-json"))
            (hsPkgs."marlowe-apps" or (errorHandler.buildDepError "marlowe-apps"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "finder" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-apps; }