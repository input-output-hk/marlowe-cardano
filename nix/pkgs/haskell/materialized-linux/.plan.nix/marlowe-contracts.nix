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
      identifier = { name = "marlowe-contracts"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "yves.hauser@iohk.io";
      author = "Yves Hauser";
      homepage = "";
      url = "";
      synopsis = "Collection of Marlowe contract examples";
      description = "Collection of Marlowe contract examples";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "ReadMe.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        modules = [
          "Marlowe/Contracts"
          "Marlowe/Contracts/Common"
          "Marlowe/Contracts/Escrow"
          "Marlowe/Contracts/Forward"
          "Marlowe/Contracts/Futures"
          "Marlowe/Contracts/Options"
          "Marlowe/Contracts/StructuredProducts"
          "Marlowe/Contracts/Swap"
          "Marlowe/Contracts/Trivial"
          "Marlowe/Contracts/ZeroCouponBond"
          "Marlowe/Contracts/UTC/Common"
          "Marlowe/Contracts/UTC/CouponBond"
          "Marlowe/Contracts/UTC/Forward"
          "Marlowe/Contracts/UTC/Futures"
          "Marlowe/Contracts/UTC/Options"
          "Marlowe/Contracts/UTC/StructuredProducts"
          "Marlowe/Contracts/UTC/Swap"
          "Marlowe/Contracts/UTC/ZeroCouponBond"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "marlowe-contracts-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-ada" or (errorHandler.buildDepError "plutus-ledger-ada"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-contracts" or (errorHandler.buildDepError "marlowe-contracts"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            ];
          buildable = true;
          modules = [ "Spec/Marlowe/Analysis" "Spec/Marlowe/Contracts" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-contracts; }