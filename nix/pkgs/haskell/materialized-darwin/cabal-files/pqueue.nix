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
      identifier = { name = "pqueue"; version = "1.4.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Lennart Spitzner <hexagoxel@hexagoxel.de>,\nLouis Wasserman <wasserman.louis@gmail.com>,\nkonsumlamm <konsumlamm@gmail.com>,\nDavid Feuer <David.Feuer@gmail.com>";
      author = "Louis Wasserman";
      homepage = "https://github.com/lspitzner/pqueue";
      url = "";
      synopsis = "Reliable, persistent, fast priority queues.";
      description = "A fast, reliable priority queue implementation based on a binomial heap.";
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
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."pqueue" or (errorHandler.buildDepError "pqueue"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "minqueue-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pqueue" or (errorHandler.buildDepError "pqueue"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            ];
          buildable = true;
          };
        "minpqueue-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pqueue" or (errorHandler.buildDepError "pqueue"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/pqueue-1.4.2.0.tar.gz";
      sha256 = "3c63d942cd322993ea6b08d1cdc8e8b48d68eb794707f1b7005c2900dc03eaac";
      });
    }) // {
    package-description-override = "name:               pqueue\nversion:            1.4.2.0\ncategory:           Data Structures\nauthor:             Louis Wasserman\nlicense:            BSD3\nlicense-file:       LICENSE\nstability:          experimental\nsynopsis:           Reliable, persistent, fast priority queues.\ndescription:        A fast, reliable priority queue implementation based on a binomial heap.\nmaintainer:         Lennart Spitzner <hexagoxel@hexagoxel.de>,\n                    Louis Wasserman <wasserman.louis@gmail.com>,\n                    konsumlamm <konsumlamm@gmail.com>,\n                    David Feuer <David.Feuer@gmail.com>\nhomepage:           https://github.com/lspitzner/pqueue\nbug-reports:        https://github.com/lspitzner/pqueue/issues\nbuild-type:         Simple\ncabal-version:      >= 1.10\ntested-with:        GHC == 7.10.3, GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.4, GHC == 8.6.5, GHC == 8.8.4, GHC == 8.10.7, GHC == 9.0.2, GHC == 9.2.2\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/lspitzner/pqueue.git\n\nlibrary\n  hs-source-dirs: src\n  default-language:\n    Haskell2010\n  build-depends:\n  { base >= 4.8 && < 4.17\n  , deepseq >= 1.3 && < 1.5\n  }\n  exposed-modules:\n    Data.PQueue.Prio.Min\n    Data.PQueue.Prio.Max\n    Data.PQueue.Min\n    Data.PQueue.Max\n  other-modules:\n    Data.PQueue.Prio.Internals\n    Data.PQueue.Internals\n    BinomialQueue.Internals\n    BinomialQueue.Min\n    BinomialQueue.Max\n    Data.PQueue.Internals.Down\n    Data.PQueue.Internals.Foldable\n    Data.PQueue.Prio.Max.Internals\n  if impl(ghc) {\n    default-extensions: DeriveDataTypeable\n  }\n  other-extensions:\n      BangPatterns\n    , CPP\n  ghc-options:\n    -- We currently need -fspec-constr to get GHC to compile conversions\n    -- from lists well. We could (and probably should) write those a\n    -- bit differently so we won't need it.\n    -fspec-constr\n    -fdicts-strict\n    -Wall\n  if impl(ghc >= 8.0)\n    ghc-options:\n      -fno-warn-unused-imports\n\ntest-suite test\n  hs-source-dirs: tests\n  default-language: Haskell2010\n  type: exitcode-stdio-1.0\n  main-is: PQueueTests.hs\n  build-depends:\n  { base >= 4.8 && < 4.17\n  , deepseq >= 1.3 && < 1.5\n  , tasty\n  , tasty-quickcheck\n  , pqueue\n  }\n  ghc-options:\n    -Wall\n    -fno-warn-type-defaults\n\nbenchmark minqueue-benchmarks\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   benchmarks\n  main-is:          BenchMinQueue.hs\n  other-modules:\n    KWay.MergeAlg\n    HeapSort\n    KWay.RandomIncreasing\n  ghc-options:      -O2\n  build-depends:\n      base          >= 4.8 && < 5\n    , pqueue\n    , deepseq       >= 1.3 && < 1.5\n    , random        >= 1.2 && < 1.3\n    , tasty-bench   >= 0.3 && < 0.4\n\nbenchmark minpqueue-benchmarks\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   benchmarks\n  main-is:          BenchMinPQueue.hs\n  other-modules:\n    KWay.PrioMergeAlg\n    PHeapSort\n    KWay.RandomIncreasing\n  ghc-options:      -O2\n  build-depends:\n      base          >= 4.8 && < 5\n    , pqueue\n    , deepseq       >= 1.3 && < 1.5\n    , random        >= 1.2 && < 1.3\n    , tasty-bench   >= 0.3 && < 0.4\n";
    }