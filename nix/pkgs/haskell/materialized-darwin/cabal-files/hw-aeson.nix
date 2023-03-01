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
      specVersion = "2.2";
      identifier = { name = "hw-aeson"; version = "0.1.8.0"; };
      license = "BSD-3-Clause";
      copyright = "2018-2022 John Ky";
      maintainer = "newhoggy@gmail.com";
      author = "John Ky";
      homepage = "https://github.com/haskell-works/hw-aeson#readme";
      url = "";
      synopsis = "Convenience functions for Aeson";
      description = "Convenience functions for Aeson.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-short" or (errorHandler.buildDepError "text-short"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      tests = {
        "hw-aeson-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hw-aeson" or (errorHandler.buildDepError "hw-aeson"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."doctest-discover" or (errorHandler.buildDepError "doctest-discover"))
            (hsPkgs."hw-aeson" or (errorHandler.buildDepError "hw-aeson"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover.components.exes.doctest-discover or (pkgs.buildPackages.doctest-discover or (errorHandler.buildToolDepError "doctest-discover:doctest-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hw-aeson-0.1.8.0.tar.gz";
      sha256 = "a20a8e21a2bf49fb33cefd09ab0fd521757280ab15603e837d9b5188df6d07f4";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\r\n\r\nname:                   hw-aeson\r\nversion:                0.1.8.0\r\nx-revision: 2\r\nsynopsis:               Convenience functions for Aeson\r\ndescription:            Convenience functions for Aeson.\r\ncategory:               Data, JSON\r\nhomepage:               https://github.com/haskell-works/hw-aeson#readme\r\nbug-reports:            https://github.com/haskell-works/hw-aeson/issues\r\nauthor:                 John Ky\r\nmaintainer:             newhoggy@gmail.com\r\ncopyright:              2018-2022 John Ky\r\nlicense:                BSD-3-Clause\r\nlicense-file:           LICENSE\r\ntested-with:            GHC == 9.2.2, GHC == 9.0.2, GHC == 8.10.7, GHC == 8.8.4, GHC == 8.6.5\r\nbuild-type:             Simple\r\nextra-source-files:     README.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/haskell-works/hw-aeson\r\n\r\ncommon base                       { build-depends: base                       >= 4.11       && < 5      }\r\n\r\ncommon aeson                      { build-depends: aeson                      >= 1.4        && < 2.2    }\r\ncommon bytestring                 { build-depends: bytestring                 >= 0.10       && < 0.12   }\r\ncommon containers                 { build-depends: containers                 >= 0.6        && < 0.7    }\r\ncommon doctest                    { build-depends: doctest                    >= 0.16.2     && < 0.21   }\r\ncommon doctest-discover           { build-depends: doctest-discover           >= 0.2        && < 0.3    }\r\ncommon hashable                   { build-depends: hashable                   >= 1.3        && < 1.5    }\r\ncommon hedgehog                   { build-depends: hedgehog                   >= 0.6        && < 1.3    }\r\ncommon hspec                      { build-depends: hspec                      >= 2.4        && < 3      }\r\ncommon text                       { build-depends: text                       >= 1.2        && < 3      }\r\ncommon text-short                 { build-depends: text-short                 >= 0.1.3      && < 0.2    }\r\ncommon unordered-containers       { build-depends: unordered-containers       >= 0.2        && < 0.3    }\r\n\r\ncommon config\r\n\r\ncommon hw-aeson\r\n  build-depends:        hw-aeson\r\n\r\nlibrary\r\n  import:               base, config\r\n                      , aeson\r\n                      , bytestring\r\n                      , containers\r\n                      , hashable\r\n                      , text\r\n                      , text-short\r\n                      , unordered-containers\r\n  other-modules:        Paths_hw_aeson\r\n  autogen-modules:      Paths_hw_aeson\r\n  hs-source-dirs:       src\r\n  default-language:     Haskell2010\r\n  exposed-modules:      HaskellWorks.Data.Aeson\r\n                        HaskellWorks.Data.Aeson.Compat\r\n                        HaskellWorks.Data.Aeson.Compat.Map\r\n                        HaskellWorks.Data.Aeson.Compat.Map.V1\r\n                        HaskellWorks.Data.Aeson.Compat.Map.V2\r\n\r\ntest-suite hw-aeson-test\r\n  import:               base, config\r\n                      , aeson\r\n                      , hedgehog\r\n                      , hspec\r\n                      , hw-aeson\r\n  type:                 exitcode-stdio-1.0\r\n  main-is:              Spec.hs\r\n  ghc-options:          -threaded -rtsopts -with-rtsopts=-N\r\n  hs-source-dirs:       test\r\n  default-language:     Haskell2010\r\n  autogen-modules:      Paths_hw_aeson\r\n  build-tool-depends:   hspec-discover:hspec-discover\r\n  other-modules:        HaskellWorks.Data.AesonSpec\r\n                        Paths_hw_aeson\r\n\r\ntest-suite doctest\r\n  import:               base, config\r\n                      , doctest\r\n                      , doctest-discover\r\n                      , hw-aeson\r\n  default-language:     Haskell2010\r\n  type:                 exitcode-stdio-1.0\r\n  ghc-options:          -threaded -rtsopts -with-rtsopts=-N\r\n  main-is:              DoctestDriver.hs\r\n  HS-Source-Dirs:       doctest\r\n  build-tool-depends:   doctest-discover:doctest-discover\r\n";
    }