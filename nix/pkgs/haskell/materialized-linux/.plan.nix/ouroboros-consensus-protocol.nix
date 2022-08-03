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
    flags = { asserts = false; };
    package = {
      specVersion = "3.0";
      identifier = {
        name = "ouroboros-consensus-protocol";
        version = "0.1.0.0";
        };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK Formal methods team";
      homepage = "";
      url = "";
      synopsis = "Cardano consensus protocols.";
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
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-data" or (errorHandler.buildDepError "cardano-data"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [
          "Ouroboros/Consensus/Protocol/Ledger/HotKey"
          "Ouroboros/Consensus/Protocol/Ledger/Util"
          "Ouroboros/Consensus/Protocol/Praos"
          "Ouroboros/Consensus/Protocol/Praos/Common"
          "Ouroboros/Consensus/Protocol/Praos/Header"
          "Ouroboros/Consensus/Protocol/Praos/Translate"
          "Ouroboros/Consensus/Protocol/Praos/Views"
          "Ouroboros/Consensus/Protocol/Praos/VRF"
          "Ouroboros/Consensus/Protocol/TPraos"
          "Ouroboros/Consensus/Protocol/Translate"
          ];
        hsSourceDirs = [ "src" ];
        };
      sublibs = {
        "ouroboros-consensus-protocol-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-protocol-tpraos" or (errorHandler.buildDepError "cardano-protocol-tpraos"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."ouroboros-consensus-protocol" or (errorHandler.buildDepError "ouroboros-consensus-protocol"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          modules = [ "Test/Consensus/Protocol/Serialisation/Generators" ];
          hsSourceDirs = [ "test-src" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "12";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "12";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/ouroboros-consensus-protocol; echo source root reset to $sourceRoot";
    }