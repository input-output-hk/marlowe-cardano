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
      identifier = { name = "network-arbitrary"; version = "0.7.0.0"; };
      license = "MIT";
      copyright = "(c) 2018 Alex Brandt";
      maintainer = "alunduil@gmail.com";
      author = "Alex Brandt";
      homepage = "https://github.com/alunduil/network-arbitrary";
      url = "";
      synopsis = "Arbitrary Instances for Network Types";
      description = "Arbitrary instances for Network types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        };
      tests = {
        "network-arbitrary-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-invariant" or (errorHandler.buildDepError "test-invariant"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-arbitrary-0.7.0.0.tar.gz";
      sha256 = "5ba37fe14e16f94fb812f78b2a126594c4a39c227488ae676c04908b2a4c6c58";
      });
    }) // {
    package-description-override = "name:                network-arbitrary\nversion:             0.7.0.0\n\nlicense:             MIT\nlicense-file:        LICENSE\n\ncopyright:           (c) 2018 Alex Brandt\n\nauthor:              Alex Brandt\nmaintainer:          alunduil@gmail.com\n\nstability:           stable\n\nhomepage:            https://github.com/alunduil/network-arbitrary\nbug-reports:         https://github.com/alunduil/network-arbitrary/issues\n\nsynopsis:            Arbitrary Instances for Network Types\ndescription:\n  Arbitrary instances for Network types.\n\ncategory:            Testing\n\ncabal-version:       >= 1.10\nbuild-type:          Simple\ntested-with:         GHC >= 7.6 && < 8.2.1 || > 8.2.1 && < 9.0\n\nextra-source-files:\n    ChangeLog.md\n  , COPYRIGHT\n  , LICENSE\n  , README.md\n  , Setup.hs\n\nlibrary\n  ghc-options:\n    -Wall\n  if impl(ghc >= 8) {\n  ghc-options:\n    -Wcompat\n  } else {\n  ghc-options:\n    -fwarn-monomorphism-restriction\n    -fwarn-tabs\n    -fwarn-unused-do-bind\n  }\n\n  default-language:    Haskell2010\n\n  build-depends:\n      base >= 4.6 && < 4.15\n    , bytestring == 0.10.*\n    , http-media >= 0.6 && < 0.9\n    , http-types >= 0.9 && < 0.13\n    , network-uri >= 2.6 && < 2.8\n    , QuickCheck >= 2.9 && < 2.15\n\n  exposed-modules:\n      Network.Arbitrary\n    , Network.HTTP.Media.MediaType.Arbitrary\n    , Network.HTTP.Types.Method.Arbitrary\n    , Network.URI.Arbitrary\n\n  other-modules:\n\n  hs-source-dirs:\n      src\n\ntest-suite network-arbitrary-tests\n  type:             exitcode-stdio-1.0\n  main-is:          Spec.hs\n\n  ghc-options:\n    -Wall\n  if impl(ghc >= 8) {\n  ghc-options:\n    -Wcompat\n  } else {\n  ghc-options:\n    -fwarn-monomorphism-restriction\n    -fwarn-tabs\n    -fwarn-unused-do-bind\n  }\n\n  default-language: Haskell2010\n\n  build-depends:\n      base >= 4.6 && < 4.15\n    , bytestring == 0.10.*\n    , case-insensitive == 1.2.*\n    , hspec >= 2.4 && < 2.8\n    , http-media >= 0.6 && < 0.9\n    , http-types >= 0.9 && < 0.13\n    , network-uri >= 2.6 && < 2.8\n    , QuickCheck >= 2.9 && < 2.15\n    , test-invariant == 0.4.*\n\n  build-tool-depends:\n      hspec-discover:hspec-discover >= 2.4 && < 2.8\n\n  other-modules:\n      Network.HTTP.Media.MediaType.Arbitrary\n    , Network.HTTP.Media.MediaType.ArbitrarySpec\n    , Network.HTTP.Types.Method.Arbitrary\n    , Network.HTTP.Types.Method.ArbitrarySpec\n    , Network.URI.Arbitrary\n    , Network.URI.ArbitrarySpec\n\n  hs-source-dirs:\n      src\n    , test\n\nsource-repository head\n  type:     git\n  location: https://github.com/alunduil/network-arbitrary\n  branch:   develop\n";
    }