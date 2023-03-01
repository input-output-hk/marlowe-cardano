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
      identifier = { name = "data-endian"; version = "0.1.1"; };
      license = "BSD-3-Clause";
      copyright = "2011-2016 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/data-endian";
      url = "";
      synopsis = "Endian-sensitive data";
      description = "This package provides helpers for converting endian-sensitive data.";
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
      url = "http://hackage.haskell.org/package/data-endian-0.1.1.tar.gz";
      sha256 = "8c1d4f30374f8331d31f4d7c6b39284331b6b9436e7b50f86547417bd05f2ac0";
      });
    }) // {
    package-description-override = "Name: data-endian\nVersion: 0.1.1\nCategory: Data\nStability: experimental\nSynopsis: Endian-sensitive data\nDescription:\n  This package provides helpers for converting endian-sensitive data.\n\nHomepage: https://github.com/mvv/data-endian\nBug-Reports: https://github.com/mvv/data-endian/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2011-2016 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nCabal-Version: >= 1.6.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/data-endian.git\n\nLibrary\n  Build-Depends: base >= 4 && < 5\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Data.Endian\n";
    }