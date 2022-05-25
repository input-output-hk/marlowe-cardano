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
      identifier = { name = "marlowe-runtime"; version = "0.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "";
      author = "";
      homepage = "";
      url = "";
      synopsis = "";
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
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unlift" or (errorHandler.buildDepError "unlift"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          ];
        buildable = true;
        modules = [
          "Language/Marlowe/Runtime/Chain"
          "Language/Marlowe/Runtime/Chain/Types"
          "Language/Marlowe/Runtime/History/Types"
          "Paths_marlowe_runtime"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "synctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-config" or (errorHandler.buildDepError "cardano-config"))
            ];
          buildable = true;
          modules = [ "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "synctest" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "banana" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-config" or (errorHandler.buildDepError "cardano-config"))
            (hsPkgs."reactive-banana" or (errorHandler.buildDepError "reactive-banana"))
            ];
          buildable = true;
          modules = [ "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "banana" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "distributed" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-config" or (errorHandler.buildDepError "cardano-config"))
            (hsPkgs."distributed-process" or (errorHandler.buildDepError "distributed-process"))
            (hsPkgs."distributed-process-supervisor" or (errorHandler.buildDepError "distributed-process-supervisor"))
            (hsPkgs."network-transport-tcp" or (errorHandler.buildDepError "network-transport-tcp"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          modules = [ "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "distributed" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-runtime; }