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
      identifier = { name = "servant-server"; version = "0.19.2"; };
      license = "BSD-3-Clause";
      copyright = "2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors";
      maintainer = "haskell-servant-maintainers@googlegroups.com";
      author = "Servant Contributors";
      homepage = "http://docs.servant.dev/";
      url = "";
      synopsis = "A family of combinators for defining webservices APIs and serving them";
      description = "A family of combinators for defining webservices APIs and serving them\n\nYou can learn about the basics in the <http://docs.servant.dev/en/stable/tutorial/index.html tutorial>.\n\n<https://github.com/haskell-servant/servant/blob/master/servant-server/example/greet.hs Here>\nis a runnable example, with comments, that defines a dummy API and implements\na webserver that serves this API, using this package.\n\n<https://github.com/haskell-servant/servant/blob/master/servant-server/CHANGELOG.md CHANGELOG>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."wai-app-static" or (errorHandler.buildDepError "wai-app-static"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          ];
        buildable = true;
        };
      exes = {
        "greet" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-wai" or (errorHandler.buildDepError "hspec-wai"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."should-not-typecheck" or (errorHandler.buildDepError "should-not-typecheck"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
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
      url = "http://hackage.haskell.org/package/servant-server-0.19.2.tar.gz";
      sha256 = "beab908f65d681771e03d97e2dbfc05c8227874e911669f5ab65a77211d4f5a8";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\nname:                servant-server\nversion:             0.19.2\n\nsynopsis:            A family of combinators for defining webservices APIs and serving them\ncategory:            Servant, Web\ndescription:\n  A family of combinators for defining webservices APIs and serving them\n  .\n  You can learn about the basics in the <http://docs.servant.dev/en/stable/tutorial/index.html tutorial>.\n  .\n  <https://github.com/haskell-servant/servant/blob/master/servant-server/example/greet.hs Here>\n  is a runnable example, with comments, that defines a dummy API and implements\n  a webserver that serves this API, using this package.\n  .\n  <https://github.com/haskell-servant/servant/blob/master/servant-server/CHANGELOG.md CHANGELOG>\n\nhomepage:            http://docs.servant.dev/\nbug-reports:         http://github.com/haskell-servant/servant/issues\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Servant Contributors\nmaintainer:          haskell-servant-maintainers@googlegroups.com\ncopyright:           2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors\nbuild-type:          Simple\ntested-with: GHC ==8.6.5 || ==8.8.4 || ==8.10.2 || ==9.0.1\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type: git\n  location: http://github.com/haskell-servant/servant.git\n\nlibrary\n  exposed-modules:\n    Servant\n    Servant.Server\n    Servant.Server.Experimental.Auth\n    Servant.Server.Generic\n    Servant.Server.Internal\n    Servant.Server.Internal.BasicAuth\n    Servant.Server.Internal.Context\n    Servant.Server.Internal.Delayed\n    Servant.Server.Internal.DelayedIO\n    Servant.Server.Internal.ErrorFormatter\n    Servant.Server.Internal.Handler\n    Servant.Server.Internal.RouteResult\n    Servant.Server.Internal.Router\n    Servant.Server.Internal.RoutingApplication\n    Servant.Server.Internal.ServerError\n    Servant.Server.StaticFiles\n    Servant.Server.UVerb\n\n  -- deprecated\n  exposed-modules:\n    Servant.Utils.StaticFiles\n\n  -- Bundled with GHC: Lower bound to not force re-installs\n  -- text and mtl are bundled starting with GHC-8.4\n  build-depends:\n      base                >= 4.9      && < 4.18\n    , bytestring          >= 0.10.8.1 && < 0.12\n    , constraints         >= 0.2      && < 0.14\n    , containers          >= 0.5.7.1  && < 0.7\n    , mtl                 >= 2.2.2    && < 2.3\n    , text                >= 1.2.3.0  && < 2.1\n    , transformers        >= 0.5.2.0  && < 0.6\n    , filepath            >= 1.4.1.1  && < 1.5\n\n  -- Servant dependencies\n  -- strict dependency as we re-export 'servant' things.\n  build-depends:\n      servant             >= 0.19     && < 0.20\n    , http-api-data       >= 0.4.1    && < 0.5.1\n\n  -- Other dependencies: Lower bound around what is in the latest Stackage LTS.\n  -- Here can be exceptions if we really need features from the newer versions.\n  build-depends:\n      base-compat         >= 0.10.5   && < 0.13\n    , base64-bytestring   >= 1.0.0.1  && < 1.3\n    , exceptions          >= 0.10.0   && < 0.11\n    , http-media          >= 0.7.1.3  && < 0.9\n    , http-types          >= 0.12.2   && < 0.13\n    , network-uri         >= 2.6.1.0  && < 2.8\n    , monad-control       >= 1.0.2.3  && < 1.1\n    , network             >= 2.8      && < 3.2\n    , sop-core            >= 0.4.0.0  && < 0.6\n    , string-conversions  >= 0.4.0.1  && < 0.5\n    , resourcet           >= 1.2.2    && < 1.3\n    , tagged              >= 0.8.6    && < 0.9\n    , transformers-base   >= 0.4.5.2  && < 0.5\n    , wai                 >= 3.2.1.2  && < 3.3\n    , wai-app-static      >= 3.1.6.2  && < 3.2\n    , word8               >= 0.1.3    && < 0.2\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n\n  ghc-options: -Wall -Wno-redundant-constraints\n\nexecutable greet\n  main-is: greet.hs\n  hs-source-dirs: example\n  ghc-options: -Wall\n  default-language: Haskell2010\n  build-depends:\n      base\n    , base-compat\n    , servant\n    , servant-server\n    , wai\n    , text\n\n  build-depends:\n      aeson               >= 1.4.1.0  && < 3\n    , warp                >= 3.2.25   && < 3.4\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  ghc-options: -Wall\n  default-language: Haskell2010\n  hs-source-dirs: test\n  main-is: Spec.hs\n  other-modules:\n      Servant.ArbitraryMonadServerSpec\n      Servant.Server.ErrorSpec\n      Servant.Server.Internal.ContextSpec\n      Servant.Server.Internal.RoutingApplicationSpec\n      Servant.Server.RouterSpec\n      Servant.Server.StaticFilesSpec\n      Servant.Server.StreamingSpec\n      Servant.Server.UsingContextSpec\n      Servant.Server.UsingContextSpec.TestCombinators\n      Servant.HoistSpec\n      Servant.ServerSpec\n\n  -- Dependencies inherited from the library. No need to specify bounds.\n  build-depends:\n      base\n    , base-compat\n    , base64-bytestring\n    , bytestring\n    , http-types\n    , mtl\n    , resourcet\n    , safe\n    , servant\n    , servant-server\n    , sop-core\n    , string-conversions\n    , text\n    , transformers\n    , transformers-compat\n    , wai\n\n  -- Additional dependencies\n  build-depends:\n      aeson                >= 1.4.1.0  && < 3\n    , directory            >= 1.3.0.0  && < 1.4\n    , hspec                >= 2.6.0    && < 2.10\n    , hspec-wai            >= 0.10.1   && < 0.12\n    , QuickCheck           >= 2.12.6.1 && < 2.15\n    , should-not-typecheck >= 2.1.0    && < 2.2\n    , temporary            >= 1.3      && < 1.4\n    , wai-extra            >= 3.0.24.3 && < 3.2\n\n  build-tool-depends:\n    hspec-discover:hspec-discover >= 2.6.0 && <2.10\n";
    }