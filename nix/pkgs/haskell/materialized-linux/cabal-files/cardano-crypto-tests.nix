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
    flags = { development = false; secp256k1-support = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-crypto-tests"; version = "2.0.0.0.1"; };
      license = "Apache-2.0";
      copyright = "2020-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Tests for cardano-crypto-class and -praos";
      description = "Tests for cardano-crypto-class and -praos";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-praos" or (errorHandler.buildDepError "cardano-crypto-praos"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
          (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
          ];
        buildable = true;
        };
      tests = {
        "test-crypto" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-crypto" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-tests" or (errorHandler.buildDepError "cardano-crypto-tests"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-crypto-tests-2.0.0.0.1.tar.gz";
      sha256 = "d3aee75010cfb58dade4717a28df79829365cdb39a8bb7516213c1d54f6c6b86";
      });
    }) // {
    package-description-override = "cabal-version:      2.2\nname:               cardano-crypto-tests\nversion:            2.0.0.0.1\nlicense:            Apache-2.0\nlicense-file:       LICENSE NOTICE\ncopyright:          2020-2021 IOHK\nmaintainer:         operations@iohk.io\nauthor:             IOHK\nsynopsis:           Tests for cardano-crypto-class and -praos\ndescription:        Tests for cardano-crypto-class and -praos\ncategory:           Currency\nbuild-type:         Simple\nextra-source-files: README.md\n\nflag development\n    description: Disable `-Werror`\n    default:     False\n    manual:      True\n\nflag secp256k1-support\n    description:\n        Enable support for functions from libsecp256k1. Requires\n        a recent libsecp256k1 with support for Schnorr signatures.\n\n    manual:      True\n\nlibrary\n    exposed-modules:\n        Test.Crypto.DSIGN\n        Test.Crypto.Hash\n        Test.Crypto.KES\n        Test.Crypto.Util\n        Test.Crypto.VRF\n        Test.Crypto.Regressions\n        Test.Crypto.Instances\n        Bench.Crypto.VRF\n        Bench.Crypto.KES\n\n    hs-source-dirs:   src\n    default-language: Haskell2010\n    ghc-options:\n        -Wall -Wcompat -Wincomplete-record-updates\n        -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints\n        -Wunused-packages\n\n    build-depends:\n        base >=4.14 && <4.15,\n        base,\n        bytestring,\n        cardano-binary,\n        cardano-crypto-class,\n        cardano-crypto-praos,\n        cardano-prelude,\n        cborg,\n        cryptonite,\n        formatting,\n        nothunks,\n        pretty-show,\n        QuickCheck,\n        quickcheck-instances,\n        tasty,\n        tasty-hunit,\n        tasty-quickcheck,\n        criterion\n\n    if !flag(development)\n        ghc-options: -Werror\n\n    if flag(secp256k1-support)\n        cpp-options: -DSECP256K1_ENABLED\n\ntest-suite test-crypto\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    hs-source-dirs:   test\n    default-language: Haskell2010\n    ghc-options:\n        -Wall -Wcompat -Wincomplete-record-updates\n        -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints\n        -Wunused-packages -threaded -rtsopts -with-rtsopts=-N\n\n    build-depends:\n        base >=4.14 && <4.15,\n        base,\n        cardano-crypto-class,\n        cardano-crypto-tests,\n        tasty,\n        tasty-quickcheck\n\n    if !flag(development)\n        ghc-options: -Werror\n\nbenchmark bench-crypto\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    hs-source-dirs:   bench\n    default-language: Haskell2010\n    ghc-options:\n        -Wall -Wcompat -Wincomplete-record-updates\n        -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints\n        -Wunused-packages -threaded\n\n    build-depends:\n        base >=4.14 && <4.15,\n        base,\n        cardano-crypto-class,\n        cardano-crypto-tests,\n        criterion\n\n    if !flag(development)\n        ghc-options: -Werror\n";
    }