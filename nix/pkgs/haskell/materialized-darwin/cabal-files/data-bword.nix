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
      identifier = { name = "data-bword"; version = "0.1.0.2"; };
      license = "BSD-3-Clause";
      copyright = "2014 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/data-bword";
      url = "";
      synopsis = "Extra operations on binary words of fixed length";
      description = "This package provides extra (vs. 'Data.Bits') operations on binary words of\nfixed length.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.5") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."data-bword" or (errorHandler.buildDepError "data-bword"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-bword-0.1.0.2.tar.gz";
      sha256 = "d64880e7d6c7a2d635d7e79552888f415a417379ee637a29321abf08187e9635";
      });
    }) // {
    package-description-override = "Name: data-bword\nVersion: 0.1.0.2\nCategory: Data\nStability: experimental\nSynopsis: Extra operations on binary words of fixed length\nDescription:\n  This package provides extra (vs. 'Data.Bits') operations on binary words of\n  fixed length.\n\nHomepage: https://github.com/mvv/data-bword\nBug-Reports: https://github.com/mvv/data-bword/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2014 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2,\n             GHC==8.4.4, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7,\n             GHC==9.0.2, GHC==9.2.4, GHC==9.4.2\n\nCabal-Version: >= 1.10.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/data-bword.git\n\nLibrary\n  Default-Language: Haskell2010\n  Build-Depends: base >= 4.5 && < 5\n  if impl(ghc >= 7.5)\n    Build-Depends: ghc-prim\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Data.BinaryWord\n\nTest-Suite tests\n  Default-Language: Haskell2010\n  Type: exitcode-stdio-1.0\n  Build-Depends: base\n               , tasty >= 0.8\n               , tasty-quickcheck >= 0.8\n               , data-bword\n  Hs-Source-Dirs: tests\n  GHC-Options: -Wall\n  Main-Is: Tests.hs\n";
    }