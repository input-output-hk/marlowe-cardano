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
      identifier = { name = "natural-transformation"; version = "0.4"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2015-2016 The University of Kansas";
      maintainer = "Andy Gill <andygill@ku.edu>";
      author = "Andy Gill";
      homepage = "https://github.com/ku-fpg/natural-transformation";
      url = "";
      synopsis = "A natural transformation package.";
      description = "A natural transformation transforms a container @f a@ into another\ncontainer @g a@. Natural transformations act as functor morphisms\nin category theory.\n\nThe naming of '~>', ':~>' and '$$' were taken,\nwith permission, from Edward Kmett's @indexed@ package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "natural-transformation-properties" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."natural-transformation" or (errorHandler.buildDepError "natural-transformation"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/natural-transformation-0.4.tar.gz";
      sha256 = "aac28e2c1147ed77c1ec0f0eb607a577fa26d0fd67474293ba860ec124efc8af";
      });
    }) // {
    package-description-override = "name:                natural-transformation\r\nversion:             0.4\r\nx-revision: 10\r\nsynopsis:            A natural transformation package.\r\ndescription:         A natural transformation transforms a container @f a@ into another\r\n                     container @g a@. Natural transformations act as functor morphisms\r\n                     in category theory.\r\n                     .\r\n                     The naming of '~>', ':~>' and '$$' were taken,\r\n                     with permission, from Edward Kmett's @indexed@ package.\r\nhomepage:            https://github.com/ku-fpg/natural-transformation\r\nbug-reports:         https://github.com/ku-fpg/natural-transformation/issues\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nstability:           Provisional\r\nauthor:              Andy Gill\r\nmaintainer:          Andy Gill <andygill@ku.edu>\r\ncopyright:           Copyright (c) 2015-2016 The University of Kansas\r\ncategory:            Control\r\nbuild-type:          Simple\r\nextra-source-files:  CHANGELOG.md, README.md\r\ntested-with:         GHC == 7.8.4, GHC == 7.10.3, GHC == 8.0.1\r\ncabal-version:       >= 1.10\r\n\r\nsource-repository head\r\n  type:                git\r\n  location:            https://github.com/ku-fpg/natural-transformation\r\n\r\nlibrary\r\n  exposed-modules:     Control.Natural\r\n                       Control.Natural.RULES\r\n                       Control.Object\r\n\r\n  build-depends:       base       >= 4.7  && < 5\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:     semigroups >= 0.16 && < 0.21\r\n\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n\r\ntest-suite natural-transformation-properties\r\n  type:                exitcode-stdio-1.0\r\n  main-is:             Properties.hs\r\n  build-depends:       base                   >= 4.7 && < 5\r\n                     , containers             >= 0.1 && < 0.7\r\n                     , natural-transformation == 0.4\r\n                     , quickcheck-instances   >= 0.1 && < 0.4\r\n                     , tasty                  >= 0.8 && < 1.5\r\n                     , tasty-quickcheck       >= 0.8 && < 0.11\r\n  hs-source-dirs:      tests\r\n  default-language:    Haskell2010\r\n  ghc-options:         -Wall\r\n";
    }