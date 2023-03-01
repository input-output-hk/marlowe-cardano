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
      specVersion = "1.6";
      identifier = { name = "text-latin1"; version = "0.3.1"; };
      license = "BSD-3-Clause";
      copyright = "2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/text-latin1";
      url = "";
      synopsis = "Latin-1 (including ASCII) utility functions";
      description = "This package provides various functions over the ASCII ang Latin-1\nportions of the 'Char' and 'Word8' data types.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-checked" or (errorHandler.buildDepError "data-checked"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/text-latin1-0.3.1.tar.gz";
      sha256 = "6c7482ae0cfde06fe6ad8f0e6ea6b0d082d27a075370b5c018c31e53aad9abf3";
      });
    }) // {
    package-description-override = "Name: text-latin1\nVersion: 0.3.1\nCategory: Text\nStability: experimental\nSynopsis: Latin-1 (including ASCII) utility functions\nDescription:\n  This package provides various functions over the ASCII ang Latin-1\n  portions of the 'Char' and 'Word8' data types.\n\nHomepage: https://github.com/mvv/text-latin1\nBug-Reports: https://github.com/mvv/text-latin1/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nTested-With: GHC==7.0.4, GHC==7.2.2, GHC==7.4.2, GHC==7.6.3, GHC==7.8.4,\n             GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.1\n\nCabal-Version: >= 1.6.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/text-latin1.git\n\nLibrary\n  Build-Depends: base >= 4 && < 5\n               , data-checked >= 0.2\n               , bytestring\n               , text\n               , semigroups >= 0.18.4\n               , case-insensitive >= 1.0\n               , hashable >= 1.1\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Text.Ascii\n    Text.Latin1\n";
    }