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
      identifier = { name = "marlowe-cli"; version = "0.0.10.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "brian.bush@iohk.io";
      author = "Brian W Bush";
      homepage = "";
      url = "";
      synopsis = "Command-line tool for running Marlowe financial contracts on Cardano Computation Layer";
      description = "Command-line tool for the reference implementation of Marlowe, domain-specific language targeted at\nthe execution of financial contracts in the style of Peyton Jones et al on Cardano Computation Layer.";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."appendmap" or (errorHandler.buildDepError "appendmap"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."cborg-json" or (errorHandler.buildDepError "cborg-json"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."marlowe-cardano" or (errorHandler.buildDepError "marlowe-cardano"))
          (hsPkgs."marlowe-actus" or (errorHandler.buildDepError "marlowe-actus"))
          (hsPkgs."marlowe-contracts" or (errorHandler.buildDepError "marlowe-contracts"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."one-line-aeson-text" or (errorHandler.buildDepError "one-line-aeson-text"))
          (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-ledger-ada" or (errorHandler.buildDepError "plutus-ledger-ada"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-ledger-slot" or (errorHandler.buildDepError "plutus-ledger-slot"))
          (hsPkgs."plutus-script-utils" or (errorHandler.buildDepError "plutus-script-utils"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ];
        buildable = true;
        modules = [
          "Language/Marlowe/CLI/Analyze"
          "Language/Marlowe/CLI/Cardano/Api"
          "Language/Marlowe/CLI/Cardano/Api/Address"
          "Language/Marlowe/CLI/Cardano/Api/Address/ProofOfBurn"
          "Language/Marlowe/CLI/Cardano/Api/PlutusScript"
          "Language/Marlowe/CLI/Cardano/Api/Value"
          "Language/Marlowe/CLI/Codec"
          "Language/Marlowe/CLI/Command"
          "Language/Marlowe/CLI/Command/Contract"
          "Language/Marlowe/CLI/Command/Input"
          "Language/Marlowe/CLI/Command/Parse"
          "Language/Marlowe/CLI/Command/Role"
          "Language/Marlowe/CLI/Command/Run"
          "Language/Marlowe/CLI/Command/Template"
          "Language/Marlowe/CLI/Command/Test"
          "Language/Marlowe/CLI/Command/Transaction"
          "Language/Marlowe/CLI/Command/Util"
          "Language/Marlowe/CLI/Data/Aeson/Traversals"
          "Language/Marlowe/CLI/Data/Foldable"
          "Language/Marlowe/CLI/Examples"
          "Language/Marlowe/CLI/Export"
          "Language/Marlowe/CLI/IO"
          "Language/Marlowe/CLI/Merkle"
          "Language/Marlowe/CLI/Orphans"
          "Language/Marlowe/CLI/Plutus/Script/Utils"
          "Language/Marlowe/CLI/Run"
          "Language/Marlowe/CLI/Sync"
          "Language/Marlowe/CLI/Sync/Types"
          "Language/Marlowe/CLI/Test"
          "Language/Marlowe/CLI/Test/Script"
          "Language/Marlowe/CLI/Test/Script/Debug"
          "Language/Marlowe/CLI/Test/Types"
          "Language/Marlowe/CLI/Transaction"
          "Language/Marlowe/CLI/Types"
          "Paths_marlowe_cli"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "marlowe-cli" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-config" or (errorHandler.buildDepError "cardano-config"))
            (hsPkgs."marlowe-cli" or (errorHandler.buildDepError "marlowe-cli"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          modules = [ "Paths_marlowe_cli" ];
          hsSourceDirs = [ "app" ];
          mainPath = [
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.defer-plugin-errors) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-cli; }