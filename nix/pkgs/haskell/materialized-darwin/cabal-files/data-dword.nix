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
      identifier = { name = "data-dword"; version = "0.3.2.1"; };
      license = "BSD-3-Clause";
      copyright = "2011-2019 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/data-dword";
      url = "";
      synopsis = "Stick two binary words together to get a bigger one";
      description = "This package provides Template Haskell utilities for declaring fixed-length\nbinary word data types. Signed and unsigned 96, 128, 160, 192, 224, and\n256-bit types are predefined.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."data-bword" or (errorHandler.buildDepError "data-bword"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
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
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-dword-0.3.2.1.tar.gz";
      sha256 = "1114f1efd8f8e80517f7a1df7e73e8a0f3dcc9414f3ba5669f05b29f90dfdd50";
      });
    }) // {
    package-description-override = "Name: data-dword\nVersion: 0.3.2.1\nCategory: Data\nStability: experimental\nSynopsis: Stick two binary words together to get a bigger one\nDescription:\n  This package provides Template Haskell utilities for declaring fixed-length\n  binary word data types. Signed and unsigned 96, 128, 160, 192, 224, and\n  256-bit types are predefined.\n\nHomepage: https://github.com/mvv/data-dword\nBug-Reports: https://github.com/mvv/data-dword/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2011-2019 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2,\n             GHC==8.4.4, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC==9.0.1,\n             GHC==9.2.1\n\nCabal-Version: >= 1.10.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/data-dword.git\n\nLibrary\n  Default-Language: Haskell2010\n  Build-Depends:\n    base             >= 4.6 && < 5,\n    hashable         >= 1.1,\n    data-bword       >= 0.1,\n    template-haskell >= 2.8,\n    ghc-prim\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Data.DoubleWord\n    Data.DoubleWord.TH\n  Other-Modules:\n    Data.DoubleWord.Base\n\nTest-Suite tests\n  Default-Language: Haskell2010\n  Type: exitcode-stdio-1.0\n  Build-Depends:\n    base             >= 4.5 && < 5,\n    tasty            >= 0.8,\n    tasty-quickcheck >= 0.8,\n    data-dword\n  Hs-Source-Dirs: tests\n  GHC-Options: -Wall\n  Main-Is: Tests.hs\n  Other-Modules:\n    Types\n";
    }