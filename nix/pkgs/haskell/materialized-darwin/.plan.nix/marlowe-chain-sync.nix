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
      specVersion = "2.4";
      identifier = { name = "marlowe-chain-sync"; version = "0.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jamie.bertram@iohk.io";
      author = "Jamie Bertram";
      homepage = "";
      url = "";
      synopsis = "Cardano chain sync system for thee Marlowe Runtime";
      description = "Marlowe runtime component for Cardano node synchronization. Communicates with\ndownstream compoents using the Filtered Chain Sync protocol, which provides\nefficient push and pull-based traversal of the cardano blockchain.";
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
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          ];
        buildable = true;
        modules = [ "Language/Marlowe/Runtime/ChainSync/NodeClient" ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "marlowesyncd" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = true;
          modules = [ "Options" "Paths_marlowe_chain_sync" ];
          hsSourceDirs = [ "app" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-chain-sync; }