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
      identifier = { name = "marlowe-integration"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jamie.bertram@iohk.io";
      author = "Jamie Bertram";
      homepage = "";
      url = "";
      synopsis = "Run integration tests in the context of a marlowe runtime";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-integration" or (errorHandler.buildDepError "cardano-integration"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
          (hsPkgs."eventuo11y-extras" or (errorHandler.buildDepError "eventuo11y-extras"))
          (hsPkgs."hasql" or (errorHandler.buildDepError "hasql"))
          (hsPkgs."hasql-pool" or (errorHandler.buildDepError "hasql-pool"))
          (hsPkgs."lifted-async" or (errorHandler.buildDepError "lifted-async"))
          (hsPkgs."marlowe-cli" or (errorHandler.buildDepError "marlowe-cli"))
          (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
          (hsPkgs."marlowe-chain-sync".components.sublibs.chain-indexer or (errorHandler.buildDepError "marlowe-chain-sync:chain-indexer"))
          (hsPkgs."marlowe-chain-sync".components.sublibs.libchainseek or (errorHandler.buildDepError "marlowe-chain-sync:libchainseek"))
          (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
          (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
          (hsPkgs."marlowe-runtime".components.sublibs.discovery or (errorHandler.buildDepError "marlowe-runtime:discovery"))
          (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
          (hsPkgs."marlowe-runtime".components.sublibs.history or (errorHandler.buildDepError "marlowe-runtime:history"))
          (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
          (hsPkgs."marlowe-runtime".components.sublibs.tx or (errorHandler.buildDepError "marlowe-runtime:tx"))
          (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."postgresql-libpq" or (errorHandler.buildDepError "postgresql-libpq"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
        buildable = true;
        modules = [
          "Test/Integration/Marlowe"
          "Test/Integration/Marlowe/Local"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "marlowe-integration-example" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-integration" or (errorHandler.buildDepError "marlowe-integration"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-integration; }