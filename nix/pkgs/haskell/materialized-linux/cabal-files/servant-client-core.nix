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
      identifier = { name = "servant-client-core"; version = "0.19"; };
      license = "BSD-3-Clause";
      copyright = "2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors";
      maintainer = "haskell-servant-maintainers@googlegroups.com";
      author = "Servant Contributors";
      homepage = "http://docs.servant.dev/";
      url = "";
      synopsis = "Core functionality and class for client function generation for servant APIs";
      description = "This library provides backend-agnostic generation of client functions. For\nmore information, see the README.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
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
      url = "http://hackage.haskell.org/package/servant-client-core-0.19.tar.gz";
      sha256 = "ace6cdafbe1981237935914f188432a35dad4440f905c83267c70cea59613a32";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\r\nname:                servant-client-core\r\nversion:             0.19\r\nx-revision: 4\r\n\r\nsynopsis:            Core functionality and class for client function generation for servant APIs\r\ncategory:            Servant, Web\r\ndescription:\r\n  This library provides backend-agnostic generation of client functions. For\r\n  more information, see the README.\r\n\r\nhomepage:            http://docs.servant.dev/\r\nbug-reports:         http://github.com/haskell-servant/servant/issues\r\nlicense:             BSD-3-Clause\r\nlicense-file:        LICENSE\r\nauthor:              Servant Contributors\r\nmaintainer:          haskell-servant-maintainers@googlegroups.com\r\ncopyright:           2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors\r\nbuild-type:          Simple\r\ntested-with: GHC ==8.6.5 || ==8.8.4 || ==8.10.2 || ==9.0.1\r\n           , GHCJS ==8.6.0.1\r\n\r\nextra-source-files:\r\n  CHANGELOG.md\r\n  README.md\r\n\r\nsource-repository head\r\n  type:              git\r\n  location:          http://github.com/haskell-servant/servant.git\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Servant.Client.Core\r\n      Servant.Client.Free\r\n      Servant.Client.Generic\r\n      Servant.Client.Core.Reexport\r\n      Servant.Client.Core.Auth\r\n      Servant.Client.Core.BaseUrl\r\n      Servant.Client.Core.BasicAuth\r\n      Servant.Client.Core.ClientError\r\n      Servant.Client.Core.HasClient\r\n      Servant.Client.Core.Request\r\n      Servant.Client.Core.Response\r\n      Servant.Client.Core.RunClient\r\n\r\n  other-modules:\r\n      Servant.Client.Core.Internal\r\n\r\n   -- Bundled with GHC: Lower bound to not force re-installs\r\n  -- text and mtl are bundled starting with GHC-8.4\r\n  --\r\n  -- note: mtl lower bound is so low because of GHC-7.8\r\n  build-depends:\r\n      base                  >= 4.9      && < 4.18\r\n    , bytestring            >= 0.10.8.1 && < 0.12\r\n    , constraints           >= 0.2      && < 0.14\r\n    , containers            >= 0.5.7.1  && < 0.7\r\n    , deepseq               >= 1.4.2.0  && < 1.5\r\n    , text                  >= 1.2.3.0  && < 2.1\r\n    , transformers          >= 0.5.2.0  && < 0.6\r\n    , template-haskell      >= 2.11.1.0 && < 2.20\r\n\r\n  if !impl(ghc >= 8.2)\r\n    build-depends:\r\n      bifunctors >= 5.5.3 && < 5.6\r\n\r\n  -- Servant dependencies\r\n  build-depends:\r\n      servant            >= 0.19\r\n\r\n  -- Other dependencies: Lower bound around what is in the latest Stackage LTS.\r\n  -- Here can be exceptions if we really need features from the newer versions.\r\n  build-depends:\r\n      aeson                 >= 1.4.1.0  && < 3\r\n    , base-compat           >= 0.10.5   && < 0.13\r\n    , base64-bytestring     >= 1.0.0.1  && < 1.3\r\n    , exceptions            >= 0.10.0   && < 0.11\r\n    , free                  >= 5.1      && < 5.2\r\n    , http-media            >= 0.7.1.3  && < 0.9\r\n    , http-types            >= 0.12.2   && < 0.13\r\n    , network-uri           >= 2.6.1.0  && < 2.7\r\n    , safe                  >= 0.3.17   && < 0.4\r\n    , sop-core              >= 0.4.0.0  && < 0.6\r\n\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n\r\ntest-suite spec\r\n  type:                exitcode-stdio-1.0\r\n  ghc-options:         -Wall\r\n  default-language:    Haskell2010\r\n  hs-source-dirs:      test\r\n  main-is:             Spec.hs\r\n  other-modules:\r\n      Servant.Client.Core.Internal.BaseUrlSpec\r\n      Servant.Client.Core.RequestSpec\r\n\r\n  -- Dependencies inherited from the library. No need to specify bounds.\r\n  build-depends:\r\n      base\r\n    , base-compat\r\n    , servant-client-core\r\n\r\n  -- Additional dependencies\r\n  build-depends:\r\n      deepseq    >= 1.4.2.0  && < 1.5\r\n    , hspec      >= 2.6.0    && < 2.10\r\n    , QuickCheck >= 2.12.6.1 && < 2.15\r\n\r\n  build-tool-depends:\r\n    hspec-discover:hspec-discover >= 2.6.0 && <2.10\r\n";
    }