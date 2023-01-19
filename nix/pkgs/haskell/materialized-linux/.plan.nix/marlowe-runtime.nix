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
      identifier = { name = "marlowe-runtime"; version = "0.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jamie.bertram@iohk.io";
      author = "Jamie Bertram";
      homepage = "";
      url = "";
      synopsis = "Runtime system for running Marlowe financial contracts on the Cardano Computation Layer";
      description = "Runtime system for running and monitoring Marlowe financial contracts on\nCardano. It provides query access to search for and inspect contracts,\ncommand access for creating and interacting with contracts, and streaming\naccess for real-time updates to contracts.";
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
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        modules = [
          "Language/Marlowe/Runtime/Core/Api"
          "Language/Marlowe/Runtime/Core/ScriptRegistry"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "history-api" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Protocol/Sync/Client"
            "Language/Marlowe/Protocol/Sync/Codec"
            "Language/Marlowe/Protocol/Sync/Server"
            "Language/Marlowe/Protocol/Sync/Types"
            "Language/Marlowe/Runtime/History/Api"
            ];
          hsSourceDirs = [ "history-api" ];
          };
        "history" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/History"
            "Language/Marlowe/Runtime/History/Follower"
            "Language/Marlowe/Runtime/History/FollowerSupervisor"
            "Language/Marlowe/Runtime/History/JobServer"
            "Language/Marlowe/Runtime/History/QueryServer"
            "Language/Marlowe/Runtime/History/Store"
            "Language/Marlowe/Runtime/History/Store/Memory"
            "Language/Marlowe/Runtime/History/Store/Model"
            "Language/Marlowe/Runtime/History/SyncServer"
            ];
          hsSourceDirs = [ "history" ];
          };
        "discovery-api" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Protocol/HeaderSync/Client"
            "Language/Marlowe/Protocol/HeaderSync/Codec"
            "Language/Marlowe/Protocol/HeaderSync/Server"
            "Language/Marlowe/Protocol/HeaderSync/Types"
            "Language/Marlowe/Runtime/Discovery/Api"
            ];
          hsSourceDirs = [ "discovery-api" ];
          };
        "discovery" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/Discovery"
            "Language/Marlowe/Runtime/Discovery/Chain"
            "Language/Marlowe/Runtime/Discovery/Store"
            "Language/Marlowe/Runtime/Discovery/QueryServer"
            "Language/Marlowe/Runtime/Discovery/SyncServer"
            ];
          hsSourceDirs = [ "discovery" ];
          };
        "tx-api" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          modules = [ "Language/Marlowe/Runtime/Transaction/Api" ];
          hsSourceDirs = [ "tx-api" ];
          };
        "tx" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-extras" or (errorHandler.buildDepError "eventuo11y-extras"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-chain-sync".components.sublibs.plutus-compat or (errorHandler.buildDepError "marlowe-chain-sync:plutus-compat"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.plutus-scripts or (errorHandler.buildDepError "marlowe-runtime:plutus-scripts"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/Transaction"
            "Language/Marlowe/Runtime/Transaction/BuildConstraints"
            "Language/Marlowe/Runtime/Transaction/Chain"
            "Language/Marlowe/Runtime/Transaction/Constraints"
            "Language/Marlowe/Runtime/Transaction/Query"
            "Language/Marlowe/Runtime/Transaction/Server"
            "Language/Marlowe/Runtime/Transaction/Submit"
            ];
          hsSourceDirs = [ "tx" ];
          };
        "plutus-scripts" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/Plutus/V2/Scripts/MarloweV1/RoleTokensPolicy"
            "Language/Marlowe/Runtime/Plutus/V2/Scripts/MarloweV1/RoleTokensPolicy/Types"
            ];
          hsSourceDirs = [ "plutus-scripts" ];
          };
        "config" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            ];
          buildable = true;
          modules = [ "Language/Marlowe/Runtime/CLI/Option" ];
          hsSourceDirs = [ "config" ];
          };
        "web" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
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
            ];
          hsSourceDirs = [ "web" ];
          };
        "web-server" = {
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
            (hsPkgs."marlowe-runtime".components.sublibs.web or (errorHandler.buildDepError "marlowe-runtime:web"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-chain-sync".components.sublibs.plutus-compat or (errorHandler.buildDepError "marlowe-chain-sync:plutus-compat"))
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
            "Language/Marlowe/Runtime/Web/Server/DTO"
            "Language/Marlowe/Runtime/Web/Server/Monad"
            "Language/Marlowe/Runtime/Web/Server/OpenAPI"
            "Language/Marlowe/Runtime/Web/Server/ContractHeaderIndexer"
            "Language/Marlowe/Runtime/Web/Server/HistoryClient"
            "Language/Marlowe/Runtime/Web/Server/REST"
            "Language/Marlowe/Runtime/Web/Server/REST/Contracts"
            "Language/Marlowe/Runtime/Web/Server/REST/Transactions"
            "Language/Marlowe/Runtime/Web/Server/TxClient"
            "Language/Marlowe/Runtime/Web/Server"
            "Language/Marlowe/Runtime/Web/Server/Util"
            ];
          hsSourceDirs = [ "web-server" ];
          };
        };
      exes = {
        "marlowe" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.config or (errorHandler.buildDepError "marlowe-runtime:config"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-delay" or (errorHandler.buildDepError "stm-delay"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."wl-pprint" or (errorHandler.buildDepError "wl-pprint"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/CLI/Command"
            "Language/Marlowe/Runtime/CLI/Command/Add"
            "Language/Marlowe/Runtime/CLI/Command/Apply"
            "Language/Marlowe/Runtime/CLI/Command/Create"
            "Language/Marlowe/Runtime/CLI/Command/Log"
            "Language/Marlowe/Runtime/CLI/Command/Ls"
            "Language/Marlowe/Runtime/CLI/Command/Rm"
            "Language/Marlowe/Runtime/CLI/Command/Submit"
            "Language/Marlowe/Runtime/CLI/Command/Tx"
            "Language/Marlowe/Runtime/CLI/Command/Withdraw"
            "Language/Marlowe/Runtime/CLI/Env"
            "Language/Marlowe/Runtime/CLI/Monad"
            "Paths_marlowe_runtime"
            ];
          hsSourceDirs = [ "cli" ];
          mainPath = ([
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "") ++ (pkgs.lib).optional (!system.isWindows) "";
          };
        "marlowe-history" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-extras" or (errorHandler.buildDepError "eventuo11y-extras"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime".components.sublibs.history or (errorHandler.buildDepError "marlowe-runtime:history"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            ];
          buildable = true;
          modules = [ "Logging" "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "marlowe-history" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "marlowe-discovery" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-extras" or (errorHandler.buildDepError "eventuo11y-extras"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery or (errorHandler.buildDepError "marlowe-runtime:discovery"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            ];
          buildable = true;
          modules = [ "Logging" "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "marlowe-discovery" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "marlowe-tx" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
            (hsPkgs."eventuo11y-extras" or (errorHandler.buildDepError "eventuo11y-extras"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx or (errorHandler.buildDepError "marlowe-runtime:tx"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            ];
          buildable = true;
          modules = [ "Logging" "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "marlowe-tx" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        "marlowe-web-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."eventuo11y-json" or (errorHandler.buildDepError "eventuo11y-json"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-runtime".components.sublibs.config or (errorHandler.buildDepError "marlowe-runtime:config"))
            (hsPkgs."marlowe-runtime".components.sublibs.web-server or (errorHandler.buildDepError "marlowe-runtime:web-server"))
            (hsPkgs."marlowe-runtime".components.sublibs.discovery-api or (errorHandler.buildDepError "marlowe-runtime:discovery-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = true;
          modules = [ "Options" "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "web-server-app" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        };
      tests = {
        "marlowe-runtime-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-api".components.sublibs.gen or (errorHandler.buildDepError "cardano-api:gen"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
            (hsPkgs."marlowe-chain-sync" or (errorHandler.buildDepError "marlowe-chain-sync"))
            (hsPkgs."marlowe-chain-sync".components.sublibs.plutus-compat or (errorHandler.buildDepError "marlowe-chain-sync:plutus-compat"))
            (hsPkgs."marlowe-protocols" or (errorHandler.buildDepError "marlowe-protocols"))
            (hsPkgs."marlowe-protocols-test" or (errorHandler.buildDepError "marlowe-protocols-test"))
            (hsPkgs."marlowe-runtime" or (errorHandler.buildDepError "marlowe-runtime"))
            (hsPkgs."marlowe-runtime".components.sublibs.history or (errorHandler.buildDepError "marlowe-runtime:history"))
            (hsPkgs."marlowe-runtime".components.sublibs.history-api or (errorHandler.buildDepError "marlowe-runtime:history-api"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx or (errorHandler.buildDepError "marlowe-runtime:tx"))
            (hsPkgs."marlowe-runtime".components.sublibs.tx-api or (errorHandler.buildDepError "marlowe-runtime:tx-api"))
            (hsPkgs."marlowe-test" or (errorHandler.buildDepError "marlowe-test"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."some" or (errorHandler.buildDepError "some"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Language/Marlowe/Runtime/Core/ScriptRegistrySpec"
            "Language/Marlowe/Runtime/History/FollowerSpec"
            "Language/Marlowe/Runtime/History/Script"
            "Language/Marlowe/Runtime/History/StoreSpec"
            "Language/Marlowe/Runtime/History/Store/ModelSpec"
            "Language/Marlowe/Runtime/HistorySpec"
            "Language/Marlowe/Runtime/Transaction/BuildConstraintsSpec"
            "Language/Marlowe/Runtime/Transaction/ConstraintsSpec"
            "Paths_marlowe_runtime"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        "web-server-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."network-arbitrary" or (errorHandler.buildDepError "network-arbitrary"))
            (hsPkgs."marlowe-runtime".components.sublibs.web or (errorHandler.buildDepError "marlowe-runtime:web"))
            (hsPkgs."marlowe-test" or (errorHandler.buildDepError "marlowe-test"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
            (hsPkgs."servant-openapi3" or (errorHandler.buildDepError "servant-openapi3"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [ "Paths_marlowe_runtime" ];
          hsSourceDirs = [ "web-server-test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-runtime; }