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
      identifier = { name = "marlowe-runtime-web"; version = "0.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jamie.bertram@iohk.io";
      author = "Jamie Bertram";
      homepage = "";
      url = "";
      synopsis = "Web server for Marlowe Runtime on Cardano.";
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
          (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-pagination" or (errorHandler.buildDepError "servant-pagination"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        modules = [
          "Language/Marlowe/Runtime/Web/API"
          "Language/Marlowe/Runtime/Web/Types"
          "Language/Marlowe/Runtime/Web/Orphans"
          "Language/Marlowe/Runtime/Web"
          "Language/Marlowe/Runtime/Web/Client"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-dsl" or (errorHandler.buildDepError "eventuo11y-dsl"))
            (hsPkgs."eventuo11y-json" or (errorHandler.buildDepError "eventuo11y-json"))
            (hsPkgs."eventuo11y-batteries" or (errorHandler.buildDepError "eventuo11y-batteries"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime-web" or (errorHandler.buildDepError "marlowe-runtime-web"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.proxy-api or (errorHandler.buildDepError "marlowe-runtime:proxy-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.sync-api or (errorHandler.buildDepError "marlowe-runtime:sync-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."servant-openapi3" or (errorHandler.buildDepError "servant-openapi3"))
            (hsPkgs."servant-pagination" or (errorHandler.buildDepError "servant-pagination"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-delay" or (errorHandler.buildDepError "stm-delay"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/Web/Server/Monad"
            "Language/Marlowe/Runtime/Web/Server/OpenAPI"
            "Language/Marlowe/Runtime/Web/Server/REST"
            "Language/Marlowe/Runtime/Web/Server/REST/ApiError"
            "Language/Marlowe/Runtime/Web/Server/REST/Contracts"
            "Language/Marlowe/Runtime/Web/Server/REST/Transactions"
            "Language/Marlowe/Runtime/Web/Server/REST/Withdrawals"
            "Language/Marlowe/Runtime/Web/Server/SyncClient"
            "Language/Marlowe/Runtime/Web/Server/TxClient"
            "Language/Marlowe/Runtime/Web/Server"
            "Language/Marlowe/Runtime/Web/Server/DTO"
            "Language/Marlowe/Runtime/Web/Server/Util"
            ];
          hsSourceDirs = [ "server" ];
          };
        };
      exes = {
        "marlowe-web-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."eventuo11y-json" or (errorHandler.buildDepError "eventuo11y-json"))
            (hsPkgs."marlowe-client" or (errorHandler.buildDepError "marlowe-client"))
            (hsPkgs."marlowe-runtime-web".components.sublibs.server or (errorHandler.buildDepError "marlowe-runtime-web:server"))
            (hsPkgs."marlowe-runtime".components.sublibs.config or (errorHandler.buildDepError "marlowe-runtime:config"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = true;
          modules = [ "Options" "Paths_marlowe_runtime_web" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "marlowe-runtime-web-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."marlowe-runtime-web" or (errorHandler.buildDepError "marlowe-runtime-web"))
            (hsPkgs."marlowe-test" or (errorHandler.buildDepError "marlowe-test"))
            (hsPkgs."network-arbitrary" or (errorHandler.buildDepError "network-arbitrary"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
            (hsPkgs."servant-openapi3" or (errorHandler.buildDepError "servant-openapi3"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [ "Paths_marlowe_runtime_web" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-runtime-web; }