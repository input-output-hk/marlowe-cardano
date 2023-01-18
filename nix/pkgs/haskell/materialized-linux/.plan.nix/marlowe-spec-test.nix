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
      identifier = { name = "marlowe-spec-test"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "hernan.rajchert@iohk.io";
      author = "Hernan Rajchert";
      homepage = "";
      url = "";
      synopsis = "Marlowe spec compliance utility tool";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."marlowe" or (errorHandler.buildDepError "marlowe"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."QuickCheck-GenT" or (errorHandler.buildDepError "QuickCheck-GenT"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          ];
        buildable = true;
        modules = [
          "Marlowe/Spec"
          "Marlowe/Spec/ClientProcess"
          "Marlowe/Spec/Core"
          "Marlowe/Spec/Core/Arbitrary"
          "Marlowe/Spec/Core/Examples"
          "Marlowe/Spec/Core/Examples/Swap"
          "Marlowe/Spec/Core/Serialization/Json"
          "Marlowe/Spec/Interpret"
          "Marlowe/Spec/LocalInterpret"
          "Marlowe/Spec/TypeId"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "marlowe-spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."marlowe-spec-test" or (errorHandler.buildDepError "marlowe-spec-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "marlowe-spec-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."marlowe-spec-test" or (errorHandler.buildDepError "marlowe-spec-test"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "LocalSpec.hs" ];
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
    postUnpack = "sourceRoot+=/marlowe-spec-test; echo source root reset to $sourceRoot";
    }