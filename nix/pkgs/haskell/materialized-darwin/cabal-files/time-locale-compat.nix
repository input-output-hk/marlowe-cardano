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
    flags = { old-locale = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "time-locale-compat"; version = "0.1.1.5"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2014-2018 Kei Hibino";
      maintainer = "ex8k.hibino@gmail.com";
      author = "Kei Hibino";
      homepage = "https://github.com/khibino/haskell-time-locale-compat";
      url = "";
      synopsis = "Compatibile module for time-format locale";
      description = "This package contains wrapped name module for time-format locale between old-locale and time-1.5.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if flags.old-locale
          then [
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ]
          else [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]);
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/time-locale-compat-0.1.1.5.tar.gz";
      sha256 = "07ff1566de7d851423a843b2de385442319348c621d4f779b3d365ce91ac502c";
      });
    }) // {
    package-description-override = "name:                time-locale-compat\nversion:             0.1.1.5\nsynopsis:            Compatibile module for time-format locale\ndescription:         This package contains wrapped name module for time-format locale between old-locale and time-1.5.\nhomepage:            https://github.com/khibino/haskell-time-locale-compat\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Kei Hibino\nmaintainer:          ex8k.hibino@gmail.com\ncopyright:           Copyright (c) 2014-2018 Kei Hibino\ncategory:            System\nbuild-type:          Simple\ncabal-version:       >=1.10\ntested-with:           GHC == 8.4.1, GHC == 8.4.2, GHC == 8.4.3\n                     , GHC == 8.2.1, GHC == 8.2.2\n                     , GHC == 8.0.1, GHC == 8.0.2\n                     , GHC == 7.10.1, GHC == 7.10.2, GHC == 7.10.3\n                     , GHC == 7.8.1, GHC == 7.8.2, GHC == 7.8.3, GHC == 7.8.4\n                     , GHC == 7.6.1, GHC == 7.6.2, GHC == 7.6.3\n                     , GHC == 7.4.1, GHC == 7.4.2\n\nflag old-locale\n  description: If true, use old-locale, otherwise use time 1.5 or newer.\n  default:     True\n\nlibrary\n  exposed-modules:     Data.Time.Locale.Compat\n\n  build-depends:         base <5\n\n  if flag(old-locale)\n    build-depends:       old-locale\n                       , time <1.5\n  else\n    build-depends:       time >=1.5\n\n  hs-source-dirs:      src\n\n  default-language:    Haskell2010\n\n\nsource-repository head\n  type:       git\n  location:   https://github.com/khibino/haskell-time-locale-compat\n\nsource-repository head\n  type:       mercurial\n  location:   https://bitbucket.org/khibino/haskell-time-locale-compat\n";
    }