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
      synopsis = "Marlowe ACTUS";
      description = "/marlowe-actus/ implements generators of Marlowe contracts based on ACTUS\ncontract terms.";
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
          (hsPkgs."actus-core" or (errorHandler.buildDepError "actus-core"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          ];
        buildable = true;
        modules = [ "Actus/Marlowe/Instance" "Actus/Marlowe" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "marlowe-actus-test" = {
          depends = [
            (hsPkgs."actus-core" or (errorHandler.buildDepError "actus-core"))
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