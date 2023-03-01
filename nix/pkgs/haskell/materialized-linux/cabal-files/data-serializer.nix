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
      identifier = { name = "data-serializer"; version = "0.3.5"; };
      license = "BSD-3-Clause";
      copyright = "2016 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/data-serializer";
      url = "";
      synopsis = "Common API for serialization libraries";
      description = "This package provides a common API for serialization libraries like\n<http://hackage.haskell.org/package/binary binary> and\n<http://hackage.haskell.org/package/cereal cereal>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."data-endian" or (errorHandler.buildDepError "data-endian"))
          (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."data-serializer" or (errorHandler.buildDepError "data-serializer"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-serializer-0.3.5.tar.gz";
      sha256 = "b4a0bfdeef7c8c77006682c46addf4ee9e1c8e51b5e01c7ac324813cd16ffd43";
      });
    }) // {
    package-description-override = "Name: data-serializer\nVersion: 0.3.5\nCategory: Data\nStability: experimental\nSynopsis: Common API for serialization libraries\nDescription:\n  This package provides a common API for serialization libraries like\n  <http://hackage.haskell.org/package/binary binary> and\n  <http://hackage.haskell.org/package/cereal cereal>.\n\nHomepage: https://github.com/mvv/data-serializer\nBug-Reports: https://github.com/mvv/data-serializer/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2016 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.4, GHC==8.6.5,\n             GHC==8.8.4, GHC==8.10.4, GHC==9.0.1\n\nCabal-Version: >= 1.10.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/data-serializer.git\n\nLibrary\n  Default-Language: Haskell2010\n  Build-Depends: base >= 4.8 && < 5\n               , bytestring >= 0.10.4\n               , binary >= 0.7.2\n               , cereal >= 0.4.1\n               , data-endian >= 0.1.1\n               , parsers >= 0.12.3\n               , split >= 0.2\n  if impl(ghc < 8.0)\n    Build-Depends: semigroups >= 0.18.2\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Data.Serializer\n    Data.Deserializer\n\nTest-Suite tests\n  Default-Language: Haskell2010\n  Type: exitcode-stdio-1.0\n  Build-Depends: base\n               , bytestring\n               , binary\n               , cereal\n               , tasty >= 0.8\n               , tasty-quickcheck >= 0.8\n               , data-serializer\n  Hs-Source-Dirs: tests\n  GHC-Options: -Wall\n  Main-Is: Tests.hs\n";
    }