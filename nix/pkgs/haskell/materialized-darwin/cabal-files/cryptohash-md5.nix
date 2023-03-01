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
      identifier = { name = "cryptohash-md5"; version = "0.11.101.0"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez, Herbert Valerio Riedel";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>";
      author = "";
      homepage = "https://github.com/hvr/cryptohash-md5";
      url = "";
      synopsis = "Fast, pure and practical MD5 implementation";
      description = "A practical incremental and one-pass, pure API to the\n<https://en.wikipedia.org/wiki/MD5 MD5 hash algorithm>\n(including <https://en.wikipedia.org/wiki/HMAC HMAC> support)\nwith performance close to the fastest implementations available in other languages.\n\nThe implementation is made in C with a haskell FFI wrapper that hides the C implementation.\n\nNOTE: This package has been forked off @cryptohash-0.11.7@ because the @cryptohash@ package\nhas been deprecated and so this package continues to satisfy the need for a lightweight package\nproviding the MD5 hash algorithm without any dependencies on packages other than\n@base@ and @bytestring@.\n\nConsequently, this package can be used as a drop-in replacement for @cryptohash@'s\n\"Crypto.Hash.MD5\" module, though with a clearly smaller footprint.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "test-md5" = {
          depends = [
            (hsPkgs."cryptohash-md5" or (errorHandler.buildDepError "cryptohash-md5"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."pureMD5" or (errorHandler.buildDepError "pureMD5"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-md5" = {
          depends = [
            (hsPkgs."cryptohash-md5" or (errorHandler.buildDepError "cryptohash-md5"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cryptohash-md5-0.11.101.0.tar.gz";
      sha256 = "3b08db0ae39df2b44e83053ad30d7546a4c6200a852c22a240a7e03ae1080f05";
      });
    }) // {
    package-description-override = "name:                cryptohash-md5\nversion:             0.11.101.0\nx-revision:          1\ndescription:\n    A practical incremental and one-pass, pure API to the\n    <https://en.wikipedia.org/wiki/MD5 MD5 hash algorithm>\n    (including <https://en.wikipedia.org/wiki/HMAC HMAC> support)\n    with performance close to the fastest implementations available in other languages.\n    .\n    The implementation is made in C with a haskell FFI wrapper that hides the C implementation.\n    .\n    NOTE: This package has been forked off @cryptohash-0.11.7@ because the @cryptohash@ package\n    has been deprecated and so this package continues to satisfy the need for a lightweight package\n    providing the MD5 hash algorithm without any dependencies on packages other than\n    @base@ and @bytestring@.\n    .\n    Consequently, this package can be used as a drop-in replacement for @cryptohash@'s\n    \"Crypto.Hash.MD5\" module, though with a clearly smaller footprint.\n\nlicense:             BSD3\nlicense-file:        LICENSE\ncopyright:           Vincent Hanquez, Herbert Valerio Riedel\nmaintainer:          Herbert Valerio Riedel <hvr@gnu.org>\nhomepage:            https://github.com/hvr/cryptohash-md5\nbug-reports:         https://github.com/hvr/cryptohash-md5/issues\nsynopsis:            Fast, pure and practical MD5 implementation\ncategory:            Data, Cryptography\nbuild-type:          Simple\ncabal-version:       >=1.10\ntested-with:         GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.4\n                   , GHC == 9.4.1\n\nextra-source-files:  cbits/md5.h\n                     changelog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/hvr/cryptohash-md5.git\n\nlibrary\n  default-language:  Haskell2010\n  build-depends:     base             >= 4.5   && < 4.18\n                   , bytestring       >= 0.9.2 && < 0.12\n\n  hs-source-dirs:    src\n  exposed-modules:   Crypto.Hash.MD5\n  other-modules:     Crypto.Hash.MD5.FFI Compat\n  ghc-options:       -Wall -fno-cse -O2\n  cc-options:        -Wall\n  include-dirs:      cbits\n\ntest-suite test-md5\n  default-language:  Haskell2010\n  other-extensions:  OverloadedStrings\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    src-tests\n  main-is:           test-md5.hs\n  ghc-options:       -Wall -threaded\n  build-depends:     cryptohash-md5\n                   , base\n                   , bytestring\n\n                   , base16-bytestring >= 1.0.1.0 && < 1.1\n                   , pureMD5           >= 2.1.3  && < 2.2\n                   , tasty             >= 1.4 && <1.5\n                   , tasty-quickcheck  == 0.10.*\n                   , tasty-hunit       == 0.10.*\n\nbenchmark bench-md5\n  default-language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  main-is:           bench-md5.hs\n  hs-source-dirs:    src-bench\n  build-depends:     cryptohash-md5\n                   , base\n                   , bytestring\n                   , criterion        >= 1.5 && <1.7\n";
    }