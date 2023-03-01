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
      identifier = { name = "sort"; version = "1.0.0.0"; };
      license = "BSD-3-Clause";
      copyright = "Chris Dornan 2017";
      maintainer = "Chris Dornan <chris@chrisdornan.com>";
      author = "Chris Dornan";
      homepage = "https://github.com/cdornan/sort";
      url = "";
      synopsis = "A Haskell sorting toolkit";
      description = "A library of general-purpose sorting utilities.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/sort-1.0.0.0.tar.gz";
      sha256 = "cee3894879cb4b2150331eca96d5d27f51a6114bcb082d1d8dded55881f5770d";
      });
    }) // {
    package-description-override = "Name:                   sort\nVersion:                1.0.0.0\nSynopsis:               A Haskell sorting toolkit\nDescription:            A library of general-purpose sorting utilities.\nHomepage:               https://github.com/cdornan/sort\nAuthor:                 Chris Dornan\nLicense:                BSD3\nlicense-file:           LICENSE\nMaintainer:             Chris Dornan <chris@chrisdornan.com>\nCopyright:              Chris Dornan 2017\nCategory:               Text\nBuild-type:             Simple\nStability:              RFC\nbug-reports:            https://github.com/cdornan/sort/issues\n\nExtra-Source-Files:\n    README.markdown\n\nCabal-Version:          >= 1.10\n\nSource-Repository head\n    type:               git\n    location:           https://github.com/cdornan/sort.git\n\nLibrary\n    Hs-Source-Dirs:     src\n\n    Exposed-Modules:    Data.Sort\n\n    Default-Language:   Haskell2010\n\n    GHC-Options:\n      -Wall\n      -fwarn-tabs\n\n    Build-depends:\n        base                 >= 4.8 && < 5\n";
    }