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
      specVersion = "1.18";
      identifier = { name = "servant-openapi3"; version = "2.0.1.5"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2015-2020, Servant contributors";
      maintainer = "nickolay.kudasov@gmail.com, kolmax94@gmail.com";
      author = "David Johnson, Nickolay Kudasov, Maxim Koltsov";
      homepage = "https://github.com/biocad/servant-openapi3";
      url = "";
      synopsis = "Generate a Swagger/OpenAPI/OAS 3.0 specification for your servant API.";
      description = "Swagger is a project used to describe and document RESTful APIs. The core of the\nproject is the [OpenAPI Specification (OAS)](https://swagger.io/docs/specification/about/).\nThis library implements v3.0 of the spec. Unlike Servant it is language-agnostic and thus is\nquite popular among developers in different languages. It has also existed for a longer time\nand has more helpful tooling.\n\nThis package provides means to generate a Swagger/OAS specification for a Servant API\nand also to partially test whether an API conforms with its specification.\n\nGenerated Swagger specification then can be used for many things such as\n\n* displaying interactive documentation using [Swagger UI](http://swagger.io/swagger-ui/);\n\n* generating clients and servers in many languages using [Swagger Codegen](http://swagger.io/swagger-codegen/);\n\n* and [many others](http://swagger.io/open-source-integrations/).";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."insert-ordered-containers" or (errorHandler.buildDepError "insert-ordered-containers"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."singleton-bool" or (errorHandler.buildDepError "singleton-bool"))
          (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = true;
          };
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-openapi3" or (errorHandler.buildDepError "servant-openapi3"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
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
      url = "http://hackage.haskell.org/package/servant-openapi3-2.0.1.5.tar.gz";
      sha256 = "5e39a8b5504dddeb8f56f07623366c7fab9df646202033c898be2d48d4c39e7d";
      });
    }) // {
    package-description-override = "name:                servant-openapi3\r\nversion:             2.0.1.5\r\nx-revision: 1\r\nsynopsis:            Generate a Swagger/OpenAPI/OAS 3.0 specification for your servant API.\r\ndescription:\r\n  Swagger is a project used to describe and document RESTful APIs. The core of the \r\n  project is the [OpenAPI Specification (OAS)](https://swagger.io/docs/specification/about/). \r\n  This library implements v3.0 of the spec. Unlike Servant it is language-agnostic and thus is \r\n  quite popular among developers in different languages. It has also existed for a longer time \r\n  and has more helpful tooling.\r\n  .\r\n  This package provides means to generate a Swagger/OAS specification for a Servant API\r\n  and also to partially test whether an API conforms with its specification.\r\n  .\r\n  Generated Swagger specification then can be used for many things such as\r\n  .\r\n  * displaying interactive documentation using [Swagger UI](http://swagger.io/swagger-ui/);\r\n  .\r\n  * generating clients and servers in many languages using [Swagger Codegen](http://swagger.io/swagger-codegen/);\r\n  .\r\n  * and [many others](http://swagger.io/open-source-integrations/).\r\nhomepage:            https://github.com/biocad/servant-openapi3\r\nbug-reports:         https://github.com/biocad/servant-openapi3/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nauthor:              David Johnson, Nickolay Kudasov, Maxim Koltsov\r\nmaintainer:          nickolay.kudasov@gmail.com, kolmax94@gmail.com\r\ncopyright:           (c) 2015-2020, Servant contributors\r\ncategory:            Web, Servant, Swagger\r\nbuild-type:          Custom\r\ncabal-version:       1.18\r\ntested-with:\r\n  GHC ==8.6.5\r\n   || ==8.8.4\r\n   || ==8.10.7\r\n   || ==9.0.2\r\n   || ==9.2.2\r\n   || ==9.4.2\r\n\r\nextra-source-files:\r\n    README.md\r\n  , CHANGELOG.md\r\n  , example/server/*.hs\r\n  , example/src/*.hs\r\n  , example/test/*.hs\r\n  , example/*.cabal\r\n  , example/swagger.json\r\n  , example/LICENSE\r\nextra-doc-files:\r\n    example/src/*.hs\r\n  , example/test/*.hs\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/biocad/servant-openapi3.git\r\n\r\ncustom-setup\r\n  setup-depends:\r\n    base >=4.9 && <4.18,\r\n    Cabal >= 1.24 && <3.7,\r\n    cabal-doctest >=1.0.6 && <1.1\r\n\r\nlibrary\r\n  ghc-options:         -Wall\r\n  exposed-modules:\r\n    Servant.OpenApi\r\n    Servant.OpenApi.Test\r\n    Servant.OpenApi.TypeLevel\r\n\r\n    -- Internal modules\r\n    Servant.OpenApi.Internal\r\n    Servant.OpenApi.Internal.Orphans\r\n    Servant.OpenApi.Internal.Test\r\n    Servant.OpenApi.Internal.TypeLevel\r\n    Servant.OpenApi.Internal.TypeLevel.API\r\n    Servant.OpenApi.Internal.TypeLevel.Every\r\n    Servant.OpenApi.Internal.TypeLevel.TMap\r\n  hs-source-dirs:      src\r\n  build-depends:       aeson                     >=1.4.2.0  && <1.6 || >=2.0.1.0 && <2.2\r\n                     , aeson-pretty              >=0.8.7    && <0.9\r\n                     , base                      >=4.9.1.0  && <4.18\r\n                     , base-compat               >=0.10.5   && <0.13\r\n                     , bytestring                >=0.10.8.1 && <0.12\r\n                     , http-media                >=0.7.1.3  && <0.9\r\n                     , insert-ordered-containers >=0.2.1.0  && <0.3\r\n                     , lens                      >=4.17     && <5.3\r\n                     , servant                   >=0.17     && <0.20\r\n                     , singleton-bool            >=0.1.4    && <0.2\r\n                     , openapi3                  >=3.0.0    && <3.3\r\n                     , text                      >=1.2.3.0  && <3\r\n                     , unordered-containers      >=0.2.9.0  && <0.3\r\n\r\n                     , hspec\r\n                     , QuickCheck\r\n  default-language:    Haskell2010\r\n\r\ntest-suite doctests\r\n  ghc-options:      -Wall\r\n  build-depends:\r\n    base,\r\n    directory >= 1.0,\r\n    doctest >= 0.11.1 && <0.21,\r\n    servant,\r\n    QuickCheck,\r\n    filepath\r\n  default-language: Haskell2010\r\n  hs-source-dirs:   test\r\n  main-is:          doctests.hs\r\n  type:             exitcode-stdio-1.0\r\n\r\ntest-suite spec\r\n  ghc-options:      -Wall\r\n  type:             exitcode-stdio-1.0\r\n  hs-source-dirs:   test\r\n  main-is:          Spec.hs\r\n  build-tool-depends: hspec-discover:hspec-discover >=2.6.0 && <2.11\r\n  build-depends:    base\r\n                  , base-compat\r\n                  , aeson\r\n                  , hspec >=2.6.0 && <2.11\r\n                  , QuickCheck\r\n                  , lens\r\n                  , lens-aeson >=1.0.2    && <1.3\r\n                  , servant\r\n                  , servant-openapi3\r\n                    -- openapi3 3.1.0 fixes some ordering-related issues, making tests stable\r\n                  , openapi3 >= 3.1.0\r\n                  , text\r\n                  , template-haskell\r\n                  , utf8-string >=1.0.1.1 && <1.1\r\n                  , time\r\n                  , vector\r\n  other-modules:\r\n    Servant.OpenApiSpec\r\n  default-language: Haskell2010\r\n";
    }