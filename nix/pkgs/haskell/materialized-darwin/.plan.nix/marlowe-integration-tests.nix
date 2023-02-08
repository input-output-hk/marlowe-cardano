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
      identifier = { name = "marlowe-integration-tests"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jamie.bertram@iohk.io";
      author = "Jamie Bertram";
      homepage = "";
      url = "";
      synopsis = "End to end integration tests for the Marlowe Runtime";
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
      exes = {
        "marlowe-integration-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-chain-sync".components.sublibs.plutus-compat or (errorHandler.buildDepError "marlowe-chain-sync:plutus-compat"))
            (hsPkgs."marlowe-integration" or (errorHandler.buildDepError "marlowe-integration"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/Integration/Basic"
            "Language/Marlowe/Runtime/Integration/Intersections"
            "Language/Marlowe/Runtime/Integration/Common"
            "Language/Marlowe/Runtime/Integration/StandardContract"
            "Language/Marlowe/Runtime/IntegrationSpec"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [
            "Spec.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-integration-tests; }