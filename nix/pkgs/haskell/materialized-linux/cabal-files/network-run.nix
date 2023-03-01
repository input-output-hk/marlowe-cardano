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
      identifier = { name = "network-run"; version = "0.2.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "kazu@iij.ad.jp";
      author = "Kazu Yamamoto";
      homepage = "";
      url = "";
      synopsis = "Simple network runner library";
      description = "Simple functions to run network clients and servers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-run-0.2.4.tar.gz";
      sha256 = "f415c619eeb34a18493dfcd634049c7a1da1b02615e1387b0096c80126af6d70";
      });
    }) // {
    package-description-override = "name:                network-run\nversion:             0.2.4\nsynopsis:            Simple network runner library\ndescription:         Simple functions to run network clients and servers.\n-- bug-reports:\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Kazu Yamamoto\nmaintainer:          kazu@iij.ad.jp\ncategory:            Network\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\ncabal-version:       >=1.10\n\nlibrary\n  exposed-modules:     Network.Run.TCP\n                       Network.Run.UDP\n  other-modules:       Network.Run.Core\n  -- other-extensions:\n  build-depends:       base >= 4 && < 5\n                     , network >= 3.1.0\n                     , bytestring\n  -- hs-source-dirs:\n  default-language:    Haskell2010\n\nsource-repository head\n  type:                git\n  location:            https://github.com/kazu-yamamoto/network-run\n";
    }