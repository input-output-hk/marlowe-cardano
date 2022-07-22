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
      identifier = { name = "hjsonschema"; version = "1.10.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "ian@housejeffries.com";
      author = "Ian Grant Jeffries";
      homepage = "https://github.com/seagreen/hjsonschema";
      url = "";
      synopsis = "JSON Schema library";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "MIT-LICENSE.txt" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "changelog.md"
        "JSON-Schema-Test-Suite/remotes/*.json"
        "JSON-Schema-Test-Suite/remotes/folder/*.json"
        "JSON-Schema-Test-Suite/tests/draft4/*.json"
        "JSON-Schema-Test-Suite/tests/draft4/optional/*.json"
        "README.md"
        "src/draft4.json"
        "test/Local/*.json"
        "test/supplement/*.json"
        "examples/json/*.json"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hjsonpointer" or (errorHandler.buildDepError "hjsonpointer"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."hw-aeson" or (errorHandler.buildDepError "hw-aeson"))
          (hsPkgs."pcre-heavy" or (errorHandler.buildDepError "pcre-heavy"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "JSONSchema/Validator/Draft4/Object/Properties"
          "Import"
          "JSONSchema/Draft4"
          "JSONSchema/Draft4/Failure"
          "JSONSchema/Draft4/Schema"
          "JSONSchema/Draft4/Spec"
          "JSONSchema/Fetch"
          "JSONSchema/Types"
          "JSONSchema/Validator/Draft4"
          "JSONSchema/Validator/Draft4/Any"
          "JSONSchema/Validator/Draft4/Array"
          "JSONSchema/Validator/Draft4/Number"
          "JSONSchema/Validator/Draft4/Object"
          "JSONSchema/Validator/Draft4/String"
          "JSONSchema/Validator/Reference"
          "JSONSchema/Validator/Types"
          "JSONSchema/Validator/Utils"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hjsonpointer" or (errorHandler.buildDepError "hjsonpointer"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."pcre-heavy" or (errorHandler.buildDepError "pcre-heavy"))
            (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Import"
            "JSONSchema/Validator/Draft4/String"
            "JSONSchema/Validator/Draft4/StringSpec"
            ];
          hsSourceDirs = [ "test" "src" ];
          mainPath = [ "Spec.hs" ];
          };
        "local" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hjsonpointer" or (errorHandler.buildDepError "hjsonpointer"))
            (hsPkgs."hjsonschema" or (errorHandler.buildDepError "hjsonschema"))
            (hsPkgs."hw-aeson" or (errorHandler.buildDepError "hw-aeson"))
            (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = true;
          modules = [
            "Local/Failure"
            "Local/Reference"
            "Local/Validation"
            "Shared"
            "AlternateSchema"
            "Simple"
            "TwoStep"
            ];
          hsSourceDirs = [ "test" "examples" ];
          mainPath = [ "Local.hs" ];
          };
        "remote" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hjsonpointer" or (errorHandler.buildDepError "hjsonpointer"))
            (hsPkgs."hjsonschema" or (errorHandler.buildDepError "hjsonschema"))
            (hsPkgs."hw-aeson" or (errorHandler.buildDepError "hw-aeson"))
            (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = true;
          modules = [ "Shared" "AlternateSchema" ];
          hsSourceDirs = [ "test" "examples" ];
          mainPath = [ "Remote.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "21";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "21";
      rev = "minimal";
      sha256 = "";
      };
    }