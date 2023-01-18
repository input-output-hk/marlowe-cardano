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
      identifier = { name = "marlowe-cardano"; version = "0.1.0.3"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "alexander.nemish@iohk.io";
      author = "Alexander Nemish";
      homepage = "";
      url = "";
      synopsis = "Marlowe: financial contracts on Cardano Computation Layer";
      description = "A reference implementation of Marlowe, domain-specific language targeted at\nthe execution of financial contracts in the style of Peyton Jones et al\non Cardano Computation Layer.";
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-aeson" or (errorHandler.buildDepError "base16-aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."multiplate" or (errorHandler.buildDepError "multiplate"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."ordered-containers" or (errorHandler.buildDepError "ordered-containers"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-script-utils" or (errorHandler.buildDepError "plutus-script-utils"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."wl-pprint" or (errorHandler.buildDepError "wl-pprint"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"));
        buildable = true;
        modules = [
          "Language/Marlowe"
          "Language/Marlowe/Extended/V1"
          "Language/Marlowe/Extended/V1/Metadata/Types"
          "Language/Marlowe/Core/V1/Plate"
          "Language/Marlowe/Core/V1/Semantics"
          "Language/Marlowe/Core/V1/Semantics/Types"
          "Language/Marlowe/Core/V1/Semantics/Types/Address"
          "Language/Marlowe/FindInputs"
          "Language/Marlowe/Client"
          "Language/Marlowe/Client/History"
          "Language/Marlowe/Util"
          "Language/Marlowe/ParserUtil"
          "Language/Marlowe/Scripts"
          "Language/Marlowe/Pretty"
          "Language/Marlowe/Analysis/FSSemantics"
          "Plutus/Debug"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe; }