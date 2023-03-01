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
      identifier = { name = "appendmap"; version = "0.1.5"; };
      license = "BSD-3-Clause";
      copyright = "2018 Alexey Kotlyarov";
      maintainer = "a@koterpillar.com";
      author = "Alexey Kotlyarov";
      homepage = "https://github.com/koterpillar/appendmap#readme";
      url = "";
      synopsis = "Map with a Semigroup and Monoid instances delegating to Semigroup of the elements";
      description = "Please see the README on GitHub at <https://github.com/koterpillar/appendmap#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      tests = {
        "appendmap-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."appendmap" or (errorHandler.buildDepError "appendmap"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/appendmap-0.1.5.tar.gz";
      sha256 = "2dbfa21a3702c30e0bdf764f5775f4ed8ac63b48a830b8931ea994f52030b90e";
      });
    }) // {
    package-description-override = "-- This file has been generated from package.yaml by hpack version 0.28.2.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 3bb3e55562b1f29834565ea4cbe850f15c1e6069067b7876d0a414c62e1da4b7\n\nname:           appendmap\nversion:        0.1.5\nsynopsis:       Map with a Semigroup and Monoid instances delegating to Semigroup of the elements\ndescription:    Please see the README on GitHub at <https://github.com/koterpillar/appendmap#readme>\ncategory:       Data Structures\nhomepage:       https://github.com/koterpillar/appendmap#readme\nbug-reports:    https://github.com/koterpillar/appendmap/issues\nauthor:         Alexey Kotlyarov\nmaintainer:     a@koterpillar.com\ncopyright:      2018 Alexey Kotlyarov\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\ncabal-version:  >= 1.10\nextra-source-files:\n    ChangeLog.md\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/koterpillar/appendmap\n\nlibrary\n  exposed-modules:\n      Data.Map.Append\n      Data.Map.Append.Lazy\n      Data.Map.Append.Strict\n      Data.Map.Counter\n  other-modules:\n      Paths_appendmap\n  hs-source-dirs:\n      src\n  build-depends:\n      base >=4.7 && <5\n    , containers\n  default-language: Haskell2010\n\ntest-suite appendmap-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Paths_appendmap\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      QuickCheck\n    , appendmap\n    , base >=4.7 && <5\n    , containers\n    , hspec\n  default-language: Haskell2010\n";
    }