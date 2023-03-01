{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "network-ip"; version = "0.3.0.3"; };
      license = "BSD-3-Clause";
      copyright = "2011-2016 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/network-ip";
      url = "";
      synopsis = "Internet Protocol data structures";
      description = "This package provides Internet Protocol data structures";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."data-endian" or (errorHandler.buildDepError "data-endian"))
          (hsPkgs."data-dword" or (errorHandler.buildDepError "data-dword"))
          (hsPkgs."type-hint" or (errorHandler.buildDepError "type-hint"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."data-serializer" or (errorHandler.buildDepError "data-serializer"))
          (hsPkgs."text-printer" or (errorHandler.buildDepError "text-printer"))
          (hsPkgs."data-textual" or (errorHandler.buildDepError "data-textual"))
          (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."data-dword" or (errorHandler.buildDepError "data-dword"))
            (hsPkgs."text-printer" or (errorHandler.buildDepError "text-printer"))
            (hsPkgs."data-textual" or (errorHandler.buildDepError "data-textual"))
            (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
            (hsPkgs."network-ip" or (errorHandler.buildDepError "network-ip"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-ip-0.3.0.3.tar.gz";
      sha256 = "e01dcc4389f3800536066ca150b6b5130d9d4b7fe7ed8e98ae7d92f3f7b1955c";
      });
    }) // {
    package-description-override = "Name: network-ip\nVersion: 0.3.0.3\nCategory: Network\nStability: experimental\nSynopsis: Internet Protocol data structures\nDescription:\n  This package provides Internet Protocol data structures\n\nHomepage: https://github.com/mvv/network-ip\nBug-Reports: https://github.com/mvv/network-ip/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2011-2016 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.4, GHC==8.6.5,\n             GHC==8.8.1\n\nCabal-Version: >= 1.10.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/network-ip.git\n\nLibrary\n  Default-Language: Haskell2010\n  Build-Depends: base >= 4.3 && < 5\n               , data-default-class\n               , data-endian >= 0.0.1\n               , data-dword >= 0.2\n               , type-hint >= 0.1\n               , hashable >= 1.1\n               , data-serializer >= 0.2\n               , text-printer >= 0.4\n               , data-textual >= 0.3\n               , parsers >= 0.5\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Network.IP.Addr\n\nTest-Suite tests\n  Default-Language: Haskell2010\n  Type: exitcode-stdio-1.0\n  Build-Depends:\n    base,\n    tasty            >= 0.4,\n    tasty-quickcheck >= 0.3,\n    data-dword,\n    text-printer,\n    data-textual,\n    parsers,\n    network-ip\n  Hs-Source-Dirs: tests\n  GHC-Options: -Wall\n  Main-Is: Tests.hs\n";
    }