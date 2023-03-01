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
      identifier = { name = "ordered-containers"; version = "0.2.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "me@dmwit.com";
      author = "Daniel Wagner";
      homepage = "";
      url = "";
      synopsis = "Set- and Map-like types that remember the order elements were inserted";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/ordered-containers-0.2.2.tar.gz";
      sha256 = "c75ac7330e70cd5d6ac0062b68033779cf15cd986d4ca20f838e016d466d22c9";
      });
    }) // {
    package-description-override = "name:                ordered-containers\nversion:             0.2.2\nsynopsis:            Set- and Map-like types that remember the order elements were inserted\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Daniel Wagner\nmaintainer:          me@dmwit.com\ncategory:            Data\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md\ncabal-version:       >=1.10\n\nsource-repository head\n  type:                git\n  location:            https://github.com/dmwit/ordered-containers\n\nlibrary\n  exposed-modules:     Data.Map.Ordered, Data.Map.Ordered.Strict, Data.Set.Ordered\n  other-modules:       Data.Map.Ordered.Internal, Data.Map.Util\n  build-depends:       base >=4.7 && <5, containers >=0.1 && <0.7\n  default-language:    Haskell98\n  ghc-options:         -fno-warn-tabs\n";
    }