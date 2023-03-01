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
      identifier = { name = "text-printer"; version = "0.5.0.2"; };
      license = "BSD-3-Clause";
      copyright = "2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/text-printer";
      url = "";
      synopsis = "Abstract interface for text builders/printers.";
      description = "This package provides an interface for injecting text into monoids such as\nbuilders and printers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."text-latin1" or (errorHandler.buildDepError "text-latin1"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
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
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/text-printer-0.5.0.2.tar.gz";
      sha256 = "b40929b1f0a1f4d0b43966f723ce3f6c64acba72763f222019bfae90197ef5e3";
      });
    }) // {
    package-description-override = "Name: text-printer\nVersion: 0.5.0.2\nCategory: Text\nStability: experimental\nSynopsis: Abstract interface for text builders/printers.\nDescription:\n  This package provides an interface for injecting text into monoids such as\n  builders and printers.\n\nHomepage: https://github.com/mvv/text-printer\nBug-Reports: https://github.com/mvv/text-printer/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2,\n             GHC==8.4.4, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC==9.0.1\n\nCabal-Version: >= 1.10.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/text-printer.git\n\nLibrary\n  Default-Language: Haskell2010\n  Build-Depends:\n    base >= 4.4 && < 5,\n    bytestring >= 0.10,\n    text,\n    pretty,\n    text-latin1 >= 0.3.1\n  if impl(ghc < 8.0)\n    Build-Depends: semigroups >= 0.18.2\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Text.Printer\n    Text.Printer.Integral\n    Text.Printer.Fractional\n\nTest-Suite tests\n  Default-Language: Haskell2010\n  Type: exitcode-stdio-1.0\n  Build-Depends:\n    base                       >= 4.4 && < 5,\n    test-framework             >= 0.5,\n    test-framework-quickcheck2 >= 0.2,\n    QuickCheck                 >= 2.4,\n    text-printer\n  Hs-Source-Dirs: tests\n  GHC-Options: -Wall\n  Main-Is: Tests.hs\n\n";
    }