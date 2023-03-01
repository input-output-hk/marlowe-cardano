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
      identifier = { name = "singleton-bool"; version = "0.1.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/singleton-bool#readme";
      url = "";
      synopsis = "Type level booleans";
      description = "Type level booleans.\n\n@singletons@ package provides similar functionality,\nbut it has tight dependency constraints.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.8") (hsPkgs."dec" or (errorHandler.buildDepError "dec"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/singleton-bool-0.1.5.tar.gz";
      sha256 = "405dd57dea92857c04f539c3394894c40c8103ea0c4f3f0fdbfbd8acccde899f";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               singleton-bool\nversion:            0.1.5\nx-revision:         3\nsynopsis:           Type level booleans\ndescription:\n  Type level booleans.\n  .\n  @singletons@ package provides similar functionality,\n  but it has tight dependency constraints.\n\ncategory:           Web\nhomepage:           https://github.com/phadej/singleton-bool#readme\nbug-reports:        https://github.com/phadej/singleton-bool/issues\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\nbuild-type:         Simple\ntested-with:\n  GHC ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.1\n\nextra-source-files:\n  CHANGELOG.md\n  README.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/singleton-bool\n\nlibrary\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  build-depends:    base >=4.6 && <4.16\n\n  if impl(ghc >=7.8)\n    build-depends: dec >=0.0.3 && <0.1\n\n  exposed-modules:  Data.Singletons.Bool\n  default-language: Haskell2010\n\n  if !impl(ghc >=7.8)\n    build-depends: tagged >=0.8.5 && <0.9\n";
    }