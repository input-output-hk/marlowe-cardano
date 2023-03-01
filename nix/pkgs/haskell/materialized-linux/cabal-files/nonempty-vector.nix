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
      specVersion = "1.24";
      identifier = { name = "nonempty-vector"; version = "0.2.1.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2019-2020 Emily Pillmore <emilypi@cohomolo.gy>";
      maintainer = "emilypi@cohomolo.gy";
      author = "Emily Pillmore";
      homepage = "https://github.com/emilypi/nonempty-vector";
      url = "";
      synopsis = "Non-empty vectors";
      description = "Performant, non-empty mutable and immutable vectors";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nonempty-vector-0.2.1.0.tar.gz";
      sha256 = "3e88159a1ad63039aba427597ea9d7eaf6e86789279d92e16214391b1bb2ce70";
      });
    }) // {
    package-description-override = "cabal-version:   1.24\r\nname:            nonempty-vector\r\nversion:         0.2.1.0\r\nx-revision: 1\r\nsynopsis:        Non-empty vectors\r\ndescription:     Performant, non-empty mutable and immutable vectors\r\nhomepage:        https://github.com/emilypi/nonempty-vector\r\nbug-reports:     https://github.com/emilypi/nonempty-vector/issues\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\nauthor:          Emily Pillmore\r\nmaintainer:      emilypi@cohomolo.gy\r\ncopyright:       (c) 2019-2020 Emily Pillmore <emilypi@cohomolo.gy>\r\ncategory:        Data\r\nbuild-type:      Custom\r\nextra-doc-files:\r\n  CHANGELOG.md\r\n  README.md\r\n\r\ntested-with:\r\n  GHC ==8.0.2\r\n   || ==8.4.3\r\n   || ==8.4.4\r\n   || ==8.6.3\r\n   || ==8.6.5\r\n   || ==8.8.1\r\n   || ==8.10.2\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/emilypi/nonempty-vector.git\r\n\r\ncustom-setup\r\n  setup-depends:\r\n      base           >=4.9 && <5\r\n    , Cabal\r\n    , cabal-doctest\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Data.Vector.NonEmpty\r\n    Data.Vector.NonEmpty.Internal\r\n    Data.Vector.NonEmpty.Mutable\r\n\r\n  build-depends:\r\n      base       >=4.9  && <5\r\n    , deepseq\r\n    , primitive  >=0.6  && <0.8\r\n    , vector     >=0.12 && <0.14\r\n\r\n  hs-source-dirs:   src\r\n  default-language: Haskell2010\r\n  ghc-options:      -Wall\r\n\r\ntest-suite doctests\r\n  default-language:  Haskell2010\r\n  type:              exitcode-stdio-1.0\r\n  main-is:           doctests.hs\r\n  build-depends:\r\n      base     >=4.9 && <5\r\n    , doctest\r\n\r\n  hs-source-dirs:    test\r\n  ghc-options:       -Wall -threaded\r\n  x-doctest-options: --fast\r\n";
    }