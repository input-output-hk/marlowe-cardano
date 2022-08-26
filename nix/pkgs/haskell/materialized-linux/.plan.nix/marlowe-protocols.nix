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
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          ];
        buildable = true;
        modules = [
          "Network/Channel"
          "Network/Protocol/Driver"
          "Network/Protocol/ChainSeek/Client"
          "Network/Protocol/ChainSeek/Codec"
          "Network/Protocol/ChainSeek/Server"
          "Network/Protocol/ChainSeek/Types"
          "Network/Protocol/Job/Client"
          "Network/Protocol/Job/Codec"
          "Network/Protocol/Job/Server"
          "Network/Protocol/Job/Types"
          "Network/Protocol/Query/Client"
          "Network/Protocol/Query/Codec"
          "Network/Protocol/Query/Server"
          "Network/Protocol/Query/Types"
          "Network/Protocol/Codec"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../marlowe-protocols; }