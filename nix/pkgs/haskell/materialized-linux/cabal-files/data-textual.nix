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
      identifier = { name = "data-textual"; version = "0.3.0.3"; };
      license = "BSD-3-Clause";
      copyright = "2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/data-textual";
      url = "";
      synopsis = "Human-friendly textual representations.";
      description = "This package provides an interface for converting between data and its\n(human-friendly) textual representation.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-latin1" or (errorHandler.buildDepError "text-latin1"))
          (hsPkgs."text-printer" or (errorHandler.buildDepError "text-printer"))
          (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."text-printer" or (errorHandler.buildDepError "text-printer"))
            (hsPkgs."type-hint" or (errorHandler.buildDepError "type-hint"))
            (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
            (hsPkgs."data-textual" or (errorHandler.buildDepError "data-textual"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-textual-0.3.0.3.tar.gz";
      sha256 = "4b9ee8ccd03f24203dd9307bf9aa67180ff0f07b45c3a01e33d8185ff275ec9a";
      });
    }) // {
    package-description-override = "Name: data-textual\nVersion: 0.3.0.3\nCategory: Data, Text\nStability: experimental\nSynopsis: Human-friendly textual representations.\nDescription:\n  This package provides an interface for converting between data and its\n  (human-friendly) textual representation.\n\nHomepage: https://github.com/mvv/data-textual\nBug-Reports: https://github.com/mvv/data-textual/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2,\n             GHC==8.4.4, GHC==8.6.5, GHC==8.8.1\n\nCabal-Version: >= 1.10.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/data-textual.git\n\nLibrary\n  Default-Language: Haskell2010\n  Build-Depends:\n    base >= 4 && < 5,\n    bytestring >= 0.10,\n    text,\n    text-latin1 >= 0.3.1,\n    text-printer >= 0.4,\n    parsers >= 0.5\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Data.Textual\n    Data.Textual.Integral\n    Data.Textual.Fractional\n\nTest-Suite tests\n  Default-Language: Haskell2010\n  Type: exitcode-stdio-1.0\n  Build-Depends:\n    base                       >= 4 && < 5,\n    test-framework             >= 0.5,\n    test-framework-quickcheck2 >= 0.2,\n    QuickCheck                 >= 2.4,\n    text-printer,\n    type-hint >= 0.1,\n    parsers,\n    data-textual\n  Hs-Source-Dirs: tests\n  GHC-Options: -Wall\n  Main-Is: Tests.hs\n";
    }