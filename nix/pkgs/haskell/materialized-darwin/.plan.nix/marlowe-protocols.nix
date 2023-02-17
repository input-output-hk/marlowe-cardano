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
      identifier = { name = "marlowe-protocols"; version = "0.0.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "jamie.bertram@iohk.io";
      author = "Jamie Bertram";
      homepage = "";
      url = "";
      synopsis = "Protocol definitions for Marlowe";
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
          (hsPkgs."async-components" or (errorHandler.buildDepError "async-components"))
          (hsPkgs."base16" or (errorHandler.buildDepError "base16"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
          (hsPkgs."eventuo11y-extras" or (errorHandler.buildDepError "eventuo11y-extras"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-run" or (errorHandler.buildDepError "network-run"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
        buildable = true;
        modules = [
          "Network/Channel"
          "Network/Protocol/ChainSeek/Client"
          "Network/Protocol/ChainSeek/Server"
          "Network/Protocol/ChainSeek/TH"
          "Network/Protocol/ChainSeek/Types"
          "Network/Protocol/Codec"
          "Network/Protocol/Codec/Spec"
          "Network/Protocol/Driver"
          "Network/Protocol/Handshake/Client"
          "Network/Protocol/Handshake/Server"
          "Network/Protocol/Handshake/Types"
          "Network/Protocol/Job/Client"
          "Network/Protocol/Job/Server"
          "Network/Protocol/Job/Types"
          "Network/Protocol/Peer"
          "Network/Protocol/Query/Client"
          "Network/Protocol/Query/Server"
          "Network/Protocol/Query/Types"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-protocols; }