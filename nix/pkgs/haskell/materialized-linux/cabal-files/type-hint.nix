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
      identifier = { name = "type-hint"; version = "0.1"; };
      license = "BSD-3-Clause";
      copyright = "2014 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/type-hint";
      url = "";
      synopsis = "Guide type inference with proxy values";
      description = "This package provides 'Proxy' values for various types from the @base@\nlibrary and functions to use these values as hints for type inference.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.7")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/type-hint-0.1.tar.gz";
      sha256 = "1161cdbf4b4b43c2953ee60438e948737604193e1bfe2c880ff178538faa99b9";
      });
    }) // {
    package-description-override = "Name: type-hint\nVersion: 0.1\nCategory: Phantom Types\nStability: experimental\nSynopsis: Guide type inference with proxy values\nDescription:\n  This package provides 'Proxy' values for various types from the @base@\n  library and functions to use these values as hints for type inference.\n\nHomepage: https://github.com/mvv/type-hint\nBug-Reports: https://github.com/mvv/type-hint/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2014 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nExtra-Source-Files:\n  README.md\n\nCabal-Version: >= 1.10.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/type-hint.git\n\nLibrary\n  Default-Language: Haskell2010\n  Build-Depends: base >= 4 && < 5\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Type.Hint\n  if !impl(ghc>=7.7)\n    Build-Depends: tagged >= 0.5\n";
    }