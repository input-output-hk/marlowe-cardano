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
      specVersion = "1.12";
      identifier = { name = "nonempty-containers"; version = "0.3.4.4"; };
      license = "BSD-3-Clause";
      copyright = "(c) Justin Le 2018";
      maintainer = "justin@jle.im";
      author = "Justin Le";
      homepage = "https://github.com/mstksg/nonempty-containers#readme";
      url = "";
      synopsis = "Non-empty variants of containers data types, with full API";
      description = "Efficient and optimized non-empty versions of types from /containers/.\nInspired by /non-empty-containers/ library, except attempting a more\nfaithful port (with under-the-hood optimizations) of the full /containers/\nAPI. Also contains a convenient typeclass abstraction for converting\nbetwewen non-empty and possibly-empty variants. See README.md for more\ninformation.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
          (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "nonempty-containers-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-fn" or (errorHandler.buildDepError "hedgehog-fn"))
            (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
            (hsPkgs."nonempty-containers" or (errorHandler.buildDepError "nonempty-containers"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nonempty-containers-0.3.4.4.tar.gz";
      sha256 = "90cdde876693371e42da7ef42b463b3e7d85bb69a0686308dcebbd0aff03e48a";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.2.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: bb13585a77cf2559f37215ac9805633bb0d48afb450f60b68bbad03aa7770ac4\n\nname:           nonempty-containers\nversion:        0.3.4.4\nsynopsis:       Non-empty variants of containers data types, with full API\ndescription:    Efficient and optimized non-empty versions of types from /containers/.\n                Inspired by /non-empty-containers/ library, except attempting a more\n                faithful port (with under-the-hood optimizations) of the full /containers/\n                API. Also contains a convenient typeclass abstraction for converting\n                betwewen non-empty and possibly-empty variants. See README.md for more\n                information.\ncategory:       Data Structures\nhomepage:       https://github.com/mstksg/nonempty-containers#readme\nbug-reports:    https://github.com/mstksg/nonempty-containers/issues\nauthor:         Justin Le\nmaintainer:     justin@jle.im\ncopyright:      (c) Justin Le 2018\nlicense:        BSD3\nlicense-file:   LICENSE\ntested-with:    GHC >= 8.4\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/mstksg/nonempty-containers\n\nlibrary\n  exposed-modules:\n      Data.Containers.NonEmpty\n      Data.IntMap.NonEmpty\n      Data.IntMap.NonEmpty.Internal\n      Data.IntSet.NonEmpty\n      Data.IntSet.NonEmpty.Internal\n      Data.Map.NonEmpty\n      Data.Map.NonEmpty.Internal\n      Data.Sequence.NonEmpty\n      Data.Sequence.NonEmpty.Internal\n      Data.Set.NonEmpty\n      Data.Set.NonEmpty.Internal\n  other-modules:\n      Paths_nonempty_containers\n  hs-source-dirs:\n      src\n  ghc-options: -Wall -Wcompat -Wredundant-constraints\n  build-depends:\n      aeson\n    , base >=4.9 && <5\n    , comonad\n    , containers >=0.5.9\n    , deepseq\n    , invariant\n    , nonempty-vector\n    , semigroupoids\n    , these\n    , vector\n  default-language: Haskell2010\n\ntest-suite nonempty-containers-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Tests.IntMap\n      Tests.IntSet\n      Tests.Map\n      Tests.Sequence\n      Tests.Set\n      Tests.Util\n      Paths_nonempty_containers\n  hs-source-dirs:\n      test\n  ghc-options: -Wall -Wcompat -Wredundant-constraints -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.9 && <5\n    , comonad\n    , containers >=0.5.9\n    , hedgehog >=1.0\n    , hedgehog-fn >=1.0\n    , invariant\n    , nonempty-containers\n    , nonempty-vector\n    , semigroupoids\n    , tasty\n    , tasty-hedgehog >=1.0\n    , text\n    , these\n    , vector\n  default-language: Haskell2010\n";
    }