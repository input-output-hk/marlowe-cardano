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
      identifier = { name = "servant"; version = "0.19.1"; };
      license = "BSD-3-Clause";
      copyright = "2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors";
      maintainer = "haskell-servant-maintainers@googlegroups.com";
      author = "Servant Contributors";
      homepage = "http://docs.servant.dev/";
      url = "";
      synopsis = "A family of combinators for defining webservices APIs";
      description = "A family of combinators for defining webservices APIs and serving them\n\nYou can learn about the basics in the <http://docs.servant.dev/en/stable/tutorial/index.html tutorial>.\n\n<https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md CHANGELOG>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
          (hsPkgs."singleton-bool" or (errorHandler.buildDepError "singleton-bool"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."string-conversions" or (errorHandler.buildDepError "string-conversions"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
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
      url = "http://hackage.haskell.org/package/servant-0.19.1.tar.gz";
      sha256 = "78bc48716f47bc182be5785cef22c9de20c933b49386323453f24a96d39066be";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\nname:                servant\nversion:             0.19.1\n\nsynopsis:            A family of combinators for defining webservices APIs\ncategory:            Servant, Web\ndescription:\n  A family of combinators for defining webservices APIs and serving them\n  .\n  You can learn about the basics in the <http://docs.servant.dev/en/stable/tutorial/index.html tutorial>.\n  .\n  <https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md CHANGELOG>\n\nhomepage:            http://docs.servant.dev/\nbug-reports:         http://github.com/haskell-servant/servant/issues\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Servant Contributors\nmaintainer:          haskell-servant-maintainers@googlegroups.com\ncopyright:           2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors\nbuild-type:          Simple\n\ntested-with: GHC ==8.6.5 || ==8.8.4 || ==8.10.2 || ==9.0.1\n           , GHCJS ==8.6.0.1\n\nextra-source-files:\n  CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: http://github.com/haskell-servant/servant.git\n\nlibrary\n  exposed-modules:\n    Servant.API\n    Servant.API.Alternative\n    Servant.API.BasicAuth\n    Servant.API.Capture\n    Servant.API.ContentTypes\n    Servant.API.Description\n    Servant.API.Empty\n    Servant.API.Experimental.Auth\n    Servant.API.Fragment\n    Servant.API.Generic\n    Servant.API.Header\n    Servant.API.HttpVersion\n    Servant.API.IsSecure\n    Servant.API.Modifiers\n    Servant.API.NamedRoutes\n    Servant.API.QueryParam\n    Servant.API.Raw\n    Servant.API.RemoteHost\n    Servant.API.ReqBody\n    Servant.API.ResponseHeaders\n    Servant.API.Status\n    Servant.API.Stream\n    Servant.API.Sub\n    Servant.API.TypeErrors\n    Servant.API.TypeLevel\n    Servant.API.UVerb\n    Servant.API.UVerb.Union\n    Servant.API.Vault\n    Servant.API.Verbs\n    Servant.API.WithNamedContext\n\n  -- Types\n  exposed-modules:\n    Servant.Types.SourceT\n\n  -- Test stuff\n  exposed-modules:\n    Servant.Test.ComprehensiveAPI\n\n  -- Safe links\n  exposed-modules:\n    Servant.Links\n\n  -- Bundled with GHC: Lower bound to not force re-installs\n  -- text and mtl are bundled starting with GHC-8.4\n  --\n  -- note: mtl lower bound is so low because of GHC-7.8\n  build-depends:\n      base                   >= 4.9      && < 4.18\n    , bytestring             >= 0.10.8.1 && < 0.12\n    , constraints            >= 0.2\n    , mtl                    >= 2.2.2    && < 2.3\n    , sop-core               >= 0.4.0.0  && < 0.6\n    , transformers           >= 0.5.2.0  && < 0.6\n    , text                   >= 1.2.3.0  && < 2.1\n\n\n  -- We depend (heavily) on the API of these packages:\n  -- i.e. re-export, or allow using without direct dependency\n  build-depends:\n      http-api-data          >= 0.4.1    && < 0.5.1\n    , singleton-bool         >= 0.1.4    && < 0.1.7\n\n  -- Other dependencies: Lower bound around what is in the latest Stackage LTS.\n  -- Here can be exceptions if we really need features from the newer versions.\n  build-depends:\n      base-compat            >= 0.10.5   && < 0.13\n    , aeson                  >= 1.4.1.0  && < 3\n    , attoparsec             >= 0.13.2.2 && < 0.15\n    , bifunctors             >= 5.5.3    && < 5.6\n    , case-insensitive       >= 1.2.0.11 && < 1.3\n    , deepseq                >= 1.4.2.0  && < 1.5\n    , http-media             >= 0.7.1.3  && < 0.9\n    , http-types             >= 0.12.2   && < 0.13\n    , mmorph                 >= 1.1.2    && < 1.3\n    , network-uri            >= 2.6.1.0  && < 2.7\n    , QuickCheck             >= 2.12.6.1 && < 2.15\n    , string-conversions     >= 0.4.0.1  && < 0.5\n    , tagged                 >= 0.8.6    && < 0.9\n    , vault                  >= 0.3.1.2  && < 0.4\n\n  hs-source-dirs: src\n  default-language: Haskell2010\n  other-extensions: AllowAmbiguousTypes\n                  , CPP\n                  , ConstraintKinds\n                  , DataKinds\n                  , DeriveDataTypeable\n                  , DeriveGeneric\n                  , ExplicitNamespaces\n                  , FlexibleContexts\n                  , FlexibleInstances\n                  , FunctionalDependencies\n                  , GADTs\n                  , KindSignatures\n                  , MultiParamTypeClasses\n                  , OverloadedStrings\n                  , PolyKinds\n                  , RankNTypes\n                  , ScopedTypeVariables\n                  , TupleSections\n                  , TypeFamilies\n                  , TypeOperators\n                  , UndecidableInstances\n\n  ghc-options: -Wall -Wno-redundant-constraints\n\ntest-suite spec\n  type: exitcode-stdio-1.0\n  ghc-options: -Wall\n  default-language: Haskell2010\n  hs-source-dirs: test\n  main-is: Spec.hs\n  other-modules:\n      Servant.API.ContentTypesSpec\n      Servant.API.ResponseHeadersSpec\n      Servant.API.StreamSpec\n      Servant.LinksSpec\n\n  -- Dependencies inherited from the library. No need to specify bounds.\n  build-depends:\n      base\n    , base-compat\n    , aeson\n    , bytestring\n    , http-media\n    , mtl\n    , servant\n    , string-conversions\n    , text\n    , transformers\n\n  -- Additional dependencies\n  build-depends:\n      hspec                >= 2.6.0    && < 2.10\n    , QuickCheck           >= 2.12.6.1 && < 2.15\n    , quickcheck-instances >= 0.3.19   && < 0.4\n\n  build-tool-depends:\n    hspec-discover:hspec-discover >= 2.6.0 && < 2.10\n";
    }