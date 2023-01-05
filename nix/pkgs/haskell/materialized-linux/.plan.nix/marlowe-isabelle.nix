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
      specVersion = "2.4";
      identifier = { name = "marlowe-isabelle"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "hernan.rajchert@iohk.io";
      author = "Hernan Rajchert";
      homepage = "";
      url = "";
      synopsis = "Exported version of the Marlowe Semantics using the isabelle proof assistant";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
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
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          ];
        buildable = true;
        modules = [
          "ByteString"
          "HOL"
          "List"
          "ListTools"
          "MList"
          "Option"
          "Orderings"
          "OptBoundTimeInterval"
          "Product_Lexorder"
          "Product_Type"
          "SemanticsGuarantees"
          "SList"
          "Stringa"
          "Arith"
          "ArithNumInstance"
          "CoreOrphanEq"
          "Examples/Swap"
          "MarloweCoreJson"
          "Semantics"
          "SemanticsTypes"
          ];
        hsSourceDirs = [ "generated" "haskell" ];
        };
      tests = {
        "marlowe-spec-test-suite" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."marlowe-isabelle" or (errorHandler.buildDepError "marlowe-isabelle"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            ];
          buildable = true;
          modules = [
            "Spec/Core/Examples/Swap"
            "Spec/Core/Serialization/Json"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "0";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "0";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/isabelle; echo source root reset to $sourceRoot";
    }