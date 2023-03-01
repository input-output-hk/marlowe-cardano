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
      identifier = { name = "servant-client"; version = "0.19"; };
      license = "BSD-3-Clause";
      copyright = "2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors";
      maintainer = "haskell-servant-maintainers@googlegroups.com";
      author = "Servant Contributors";
      homepage = "http://docs.servant.dev/";
      url = "";
      synopsis = "Automatic derivation of querying functions for servant";
      description = "This library lets you derive automatically Haskell functions that\nlet you query each endpoint of a <http://hackage.haskell.org/package/servant servant> webservice.\n\nSee <http://docs.servant.dev/en/stable/tutorial/Client.html the client section of the tutorial>.\n\n<https://github.com/haskell-servant/servant/blob/master/servant-client/CHANGELOG.md CHANGELOG>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."kan-extensions" or (errorHandler.buildDepError "kan-extensions"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."kan-extensions" or (errorHandler.buildDepError "kan-extensions"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."tdigest" or (errorHandler.buildDepError "tdigest"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = if compiler.isGhcjs && true then false else true;
          };
        "readme" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."markdown-unlit" or (errorHandler.buildDepError "markdown-unlit"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.markdown-unlit.components.exes.markdown-unlit or (pkgs.buildPackages.markdown-unlit or (errorHandler.buildToolDepError "markdown-unlit:markdown-unlit")))
            ];
          buildable = if compiler.isGhcjs && true then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/servant-client-0.19.tar.gz";
      sha256 = "ea96d72d7687425cbcfaac03223f2b8d21769bed9a23ad53000ed068b2beaaad";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\r\nname:                servant-client\r\nversion:             0.19\r\nx-revision: 4\r\n\r\nsynopsis:            Automatic derivation of querying functions for servant\r\ncategory:            Servant, Web\r\ndescription:\r\n  This library lets you derive automatically Haskell functions that\r\n  let you query each endpoint of a <http://hackage.haskell.org/package/servant servant> webservice.\r\n  .\r\n  See <http://docs.servant.dev/en/stable/tutorial/Client.html the client section of the tutorial>.\r\n  .\r\n  <https://github.com/haskell-servant/servant/blob/master/servant-client/CHANGELOG.md CHANGELOG>\r\n\r\nhomepage:            http://docs.servant.dev/\r\nbug-reports:         http://github.com/haskell-servant/servant/issues\r\nlicense:             BSD-3-Clause\r\nlicense-file:        LICENSE\r\nauthor:              Servant Contributors\r\nmaintainer:          haskell-servant-maintainers@googlegroups.com\r\ncopyright:           2014-2016 Zalora South East Asia Pte Ltd, 2016-2019 Servant Contributors\r\nbuild-type:          Simple\r\ntested-with: GHC ==8.6.5 || ==8.8.4 || ==8.10.2 || ==9.0.1\r\n           , GHCJS ==8.6.0.1\r\n\r\nextra-source-files:\r\n  CHANGELOG.md\r\n  README.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: http://github.com/haskell-servant/servant.git\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Servant.Client\r\n    Servant.Client.Streaming\r\n    Servant.Client.Internal.HttpClient\r\n    Servant.Client.Internal.HttpClient.Streaming\r\n\r\n  -- Bundled with GHC: Lower bound to not force re-installs\r\n  -- text and mtl are bundled starting with GHC-8.4\r\n  build-depends:\r\n      base                  >= 4.9      && < 4.18\r\n    , bytestring            >= 0.10.8.1 && < 0.12\r\n    , containers            >= 0.5.7.1  && < 0.7\r\n    , deepseq               >= 1.4.2.0  && < 1.5\r\n    , mtl                   >= 2.2.2    && < 2.3\r\n    , stm                   >= 2.4.5.1  && < 2.6\r\n    , text                  >= 1.2.3.0  && < 2.1\r\n    , time                  >= 1.6.0.1  && < 1.13\r\n    , transformers          >= 0.5.2.0  && < 0.6\r\n\r\n  if !impl(ghc >= 8.2)\r\n    build-depends:\r\n      bifunctors >= 5.5.3 && < 5.6\r\n\r\n  -- Servant dependencies.\r\n  -- Strict dependency on `servant-client-core` as we re-export things.\r\n  build-depends:\r\n      servant               >= 0.18 && < 0.20\r\n    , servant-client-core   >= 0.19 && < 0.19.1\r\n\r\n  -- Other dependencies: Lower bound around what is in the latest Stackage LTS.\r\n  -- Here can be exceptions if we really need features from the newer versions.\r\n  build-depends:\r\n      base-compat           >= 0.10.5   && < 0.13\r\n    , http-client           >= 0.5.13.1 && < 0.8\r\n    , http-media            >= 0.7.1.3  && < 0.9\r\n    , http-types            >= 0.12.2   && < 0.13\r\n    , exceptions            >= 0.10.0   && < 0.11\r\n    , kan-extensions        >= 5.2      && < 5.3\r\n    , monad-control         >= 1.0.2.3  && < 1.1\r\n    , semigroupoids         >= 5.3.1    && < 5.4\r\n    , transformers-base     >= 0.4.5.2  && < 0.5\r\n    , transformers-compat   >= 0.6.2    && < 0.8\r\n\r\n  hs-source-dirs: src\r\n  default-language: Haskell2010\r\n  ghc-options: -Wall -Wno-redundant-constraints\r\n\r\ntest-suite spec\r\n  type: exitcode-stdio-1.0\r\n  ghc-options: -Wall -rtsopts -threaded \"-with-rtsopts=-T -N2\"\r\n  default-language: Haskell2010\r\n  if impl(ghcjs)\r\n     buildable: False\r\n  hs-source-dirs: test\r\n  main-is: Spec.hs\r\n  other-modules:\r\n      Servant.BasicAuthSpec\r\n      Servant.BrokenSpec\r\n      Servant.ClientTestUtils\r\n      Servant.ConnectionErrorSpec\r\n      Servant.FailSpec\r\n      Servant.GenAuthSpec\r\n      Servant.GenericSpec\r\n      Servant.HoistClientSpec\r\n      Servant.StreamSpec\r\n      Servant.SuccessSpec\r\n      Servant.WrappedApiSpec\r\n\r\n  -- Dependencies inherited from the library. No need to specify bounds.\r\n  build-depends:\r\n      base\r\n    , aeson\r\n    , base-compat\r\n    , bytestring\r\n    , http-api-data\r\n    , http-client\r\n    , http-types\r\n    , mtl\r\n    , kan-extensions\r\n    , servant-client\r\n    , servant-client-core\r\n    , sop-core\r\n    , stm\r\n    , text\r\n    , transformers\r\n    , transformers-compat\r\n    , wai\r\n    , warp\r\n\r\n  -- Additional dependencies\r\n  build-depends:\r\n      entropy           >= 0.4.1.3  && < 0.5\r\n    , hspec             >= 2.6.0    && < 2.10\r\n    , HUnit             >= 1.6.0.0  && < 1.7\r\n    , network           >= 2.8.0.0  && < 3.2\r\n    , QuickCheck        >= 2.12.6.1 && < 2.15\r\n    , servant           == 0.19.*\r\n    , servant-server    == 0.19.*\r\n    , tdigest           >= 0.2     && < 0.3\r\n\r\n  build-tool-depends:\r\n    hspec-discover:hspec-discover >= 2.6.0 && < 2.10\r\n\r\ntest-suite readme\r\n  type:           exitcode-stdio-1.0\r\n  main-is:        README.lhs\r\n  build-depends:  base, servant, http-client, text, servant-client, markdown-unlit\r\n  build-tool-depends: markdown-unlit:markdown-unlit\r\n  ghc-options:    -pgmL markdown-unlit\r\n  default-language: Haskell2010\r\n  if impl(ghcjs)\r\n     buildable: False\r\n";
    }