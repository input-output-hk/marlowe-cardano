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
      identifier = { name = "cryptohash-sha1"; version = "0.11.101.0"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez, Herbert Valerio Riedel";
      maintainer = "Herbert Valerio Riedel <hvr@gnu.org>";
      author = "";
      homepage = "https://github.com/hvr/cryptohash-sha1";
      url = "";
      synopsis = "Fast, pure and practical SHA-1 implementation";
      description = "A practical incremental and one-pass, pure API to the\n<https://en.wikipedia.org/wiki/SHA-1 SHA-1 hash algorithm>\n(including <https://en.wikipedia.org/wiki/HMAC HMAC> support)\nwith performance close to the fastest implementations available in other languages.\n\nThe implementation is made in C with a haskell FFI wrapper that hides the C implementation.\n\nNOTE: This package has been forked off @cryptohash-0.11.7@ because the @cryptohash@ package has been\ndeprecated and so this package continues to satisfy the need for a lightweight package\nproviding the SHA1 hash algorithm without any dependencies on packages other than\n@base@ and @bytestring@.\n\nConsequently, this package can be used as a drop-in replacement for @cryptohash@'s\n\"Crypto.Hash.SHA1\" module, though with a clearly smaller footprint.";
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
        "test-sha1" = {
          depends = [
            (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."SHA" or (errorHandler.buildDepError "SHA"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench-sha1" = {
          depends = [
            (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
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
      url = "http://hackage.haskell.org/package/cryptohash-sha1-0.11.101.0.tar.gz";
      sha256 = "a4042c97ad02eb68e766577ca35c01970c33e96cfd74ccb4dd403e3476a23241";
      });
    }) // {
    package-description-override = "name:                cryptohash-sha1\nversion:             0.11.101.0\nx-revision:          1\ndescription:\n    A practical incremental and one-pass, pure API to the\n    <https://en.wikipedia.org/wiki/SHA-1 SHA-1 hash algorithm>\n    (including <https://en.wikipedia.org/wiki/HMAC HMAC> support)\n    with performance close to the fastest implementations available in other languages.\n    .\n    The implementation is made in C with a haskell FFI wrapper that hides the C implementation.\n    .\n    NOTE: This package has been forked off @cryptohash-0.11.7@ because the @cryptohash@ package has been\n    deprecated and so this package continues to satisfy the need for a lightweight package\n    providing the SHA1 hash algorithm without any dependencies on packages other than\n    @base@ and @bytestring@.\n    .\n    Consequently, this package can be used as a drop-in replacement for @cryptohash@'s\n    \"Crypto.Hash.SHA1\" module, though with a clearly smaller footprint.\n\nlicense:             BSD3\nlicense-file:        LICENSE\ncopyright:           Vincent Hanquez, Herbert Valerio Riedel\nmaintainer:          Herbert Valerio Riedel <hvr@gnu.org>\nhomepage:            https://github.com/hvr/cryptohash-sha1\nbug-reports:         https://github.com/hvr/cryptohash-sha1/issues\nsynopsis:            Fast, pure and practical SHA-1 implementation\ncategory:            Data, Cryptography\nbuild-type:          Simple\ncabal-version:       >=1.10\ntested-with:         GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.4\n                   , GHC == 9.4.1\n\nextra-source-files:  cbits/sha1.h\n                     changelog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/hvr/cryptohash-sha1.git\n\nlibrary\n  default-language:  Haskell2010\n  build-depends:     base             >= 4.5   && < 4.18\n                   , bytestring       >= 0.9.2 && < 0.12\n\n  hs-source-dirs:    src\n  exposed-modules:   Crypto.Hash.SHA1\n  other-modules:     Crypto.Hash.SHA1.FFI Compat\n  ghc-options:       -Wall -fno-cse -O2\n  cc-options:        -Wall\n  include-dirs:      cbits\n\ntest-suite test-sha1\n  default-language:  Haskell2010\n  other-extensions:  OverloadedStrings\n  type:              exitcode-stdio-1.0\n  hs-source-dirs:    src-tests\n  main-is:           test-sha1.hs\n  build-depends:     cryptohash-sha1\n                   , base\n                   , bytestring\n\n                   , base16-bytestring >= 1.0.1.0 && < 1.1\n                   , SHA               >= 1.6.4   && < 1.7\n                   , tasty             >= 1.4     && < 1.5\n                   , tasty-quickcheck  == 0.10.*\n                   , tasty-hunit       == 0.10.*\n\nbenchmark bench-sha1\n  default-language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  main-is:           bench-sha1.hs\n  hs-source-dirs:    src-bench\n  build-depends:     cryptohash-sha1\n                   , base\n                   , bytestring\n                   , criterion        >=1.5 && <1.7\n";
    }