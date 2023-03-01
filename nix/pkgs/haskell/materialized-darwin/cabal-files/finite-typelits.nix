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
      identifier = { name = "finite-typelits"; version = "0.1.6.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "mniip@mniip.com";
      author = "mniip";
      homepage = "https://github.com/mniip/finite-typelits";
      url = "";
      synopsis = "A type inhabited by finitely many values, indexed by type-level naturals";
      description = "A type inhabited by finitely many values, indexed by type-level naturals.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "finite-typelits-tests" = {
          depends = [
            (hsPkgs."finite-typelits" or (errorHandler.buildDepError "finite-typelits"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/finite-typelits-0.1.6.0.tar.gz";
      sha256 = "3046456b3b3b7a202da7dc61f909e5925f9eaa57d5e03ce81d3f76ca7d3b0438";
      });
    }) // {
    package-description-override = "name:                finite-typelits\nversion:             0.1.6.0\nsynopsis:            A type inhabited by finitely many values, indexed by type-level naturals\ndescription:         A type inhabited by finitely many values, indexed by type-level naturals.\nhomepage:            https://github.com/mniip/finite-typelits\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              mniip\nmaintainer:          mniip@mniip.com\ncategory:            Data\nbuild-type:          Simple\ncabal-version:       >=1.10\n\nlibrary\n  exposed-modules:     Data.Finite\n                     , Data.Finite.Internal\n  build-depends:       base >= 4.7 && < 4.18\n                     , deepseq >= 1.4\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  ghc-options:         -Wall\n\ntest-Suite finite-typelits-tests\n  type:                exitcode-stdio-1.0\n  main-is:             test/Main.hs\n  build-depends:       finite-typelits\n                     , base >= 4.9 && < 4.18\n                     , deepseq >= 1.4\n                     , QuickCheck\n  default-language:    Haskell2010\n";
    }