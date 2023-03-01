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
      identifier = { name = "newtype-generics"; version = "0.6.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Simon Jakobi <simon.jakobi@gmail.com>";
      author = "Darius Jahandarie, Conor McBride, João Cristóvão, Simon Jakobi";
      homepage = "http://github.com/sjakobi/newtype-generics";
      url = "";
      synopsis = "A typeclass and set of functions for working with newtypes";
      description = "Per Conor McBride, the Newtype typeclass represents the packing and unpacking of a newtype,\nand allows you to operate under that newtype with functions such as ala.\nGenerics support was added in version 0.4, making this package a full replacement\nfor the original newtype package, and a better alternative to newtype-th.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/newtype-generics-0.6.2.tar.gz";
      sha256 = "a1ac6052020a09f1bc5000a141d2edd4b31a82f95ce5957b7eedad40c065a74e";
      });
    }) // {
    package-description-override = "Name:                newtype-generics\r\nVersion:             0.6.2\r\nx-revision: 1\r\nSynopsis:            A typeclass and set of functions for working with newtypes\r\nDescription:         Per Conor McBride, the Newtype typeclass represents the packing and unpacking of a newtype,\r\n                     and allows you to operate under that newtype with functions such as ala.\r\n                     Generics support was added in version 0.4, making this package a full replacement\r\n                     for the original newtype package, and a better alternative to newtype-th.\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nAuthor:              Darius Jahandarie, Conor McBride, João Cristóvão, Simon Jakobi\r\nMaintainer:          Simon Jakobi <simon.jakobi@gmail.com>\r\nHomepage:            http://github.com/sjakobi/newtype-generics\r\nCategory:            Control\r\nBuild-type:          Simple\r\nExtra-source-files:  CHANGELOG.md\r\nCabal-version:       >=1.10\r\nTested-with:\r\n  GHC==9.2.1,\r\n  GHC==9.0.1,\r\n  GHC==8.10.4,\r\n  GHC==8.8.4,\r\n  GHC==8.6.5,\r\n  GHC==8.4.4,\r\n  GHC==8.2.2,\r\n  GHC==8.0.2\r\n\r\nLibrary\r\n  Exposed-modules:     Control.Newtype.Generics\r\n  Build-depends:       base >= 4.9 && < 4.18\r\n  Ghc-options: -Wall\r\n  default-language:   Haskell2010\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/sjakobi/newtype-generics\r\n\r\ntest-suite test\r\n  type:               exitcode-stdio-1.0\r\n  main-is:            main.hs\r\n  hs-source-dirs:     test\r\n  other-modules:      Control.NewtypeSpec\r\n  build-depends:      base\r\n                    , newtype-generics\r\n                    , hspec             >= 2.1\r\n  default-language:   Haskell2010\r\n  build-tool-depends: hspec-discover:hspec-discover >= 2.1\r\n\r\nbenchmark bench\r\n  type:               exitcode-stdio-1.0\r\n  main-is:            main.hs\r\n  hs-source-dirs:     bench\r\n  build-depends:      base >= 4.7\r\n                    , gauge\r\n                    , newtype-generics\r\n                    , semigroups\r\n  ghc-options:        -O2\r\n  default-language:   Haskell2010\r\n";
    }