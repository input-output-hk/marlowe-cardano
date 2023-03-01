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
      identifier = { name = "data-checked"; version = "0.3"; };
      license = "BSD-3-Clause";
      copyright = "2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      maintainer = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      author = "Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>";
      homepage = "https://github.com/mvv/data-checked";
      url = "";
      synopsis = "Type-indexed runtime-checked properties ";
      description = "This package provides a (phantom) type-indexed newtype evidence-wrapper for\nvalues that are checked to satisfy the property associated with the type.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-checked-0.3.tar.gz";
      sha256 = "dc87d09c7c8587c9e6e372166e8de3b42c2cd804a493ff100c253e4d713c5676";
      });
    }) // {
    package-description-override = "Name: data-checked\nVersion: 0.3\nCategory: Data\nStability: experimental\nSynopsis: Type-indexed runtime-checked properties \nDescription:\n  This package provides a (phantom) type-indexed newtype evidence-wrapper for\n  values that are checked to satisfy the property associated with the type. \n\nHomepage: https://github.com/mvv/data-checked\nBug-Reports: https://github.com/mvv/data-checked/issues\n\nAuthor: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nMaintainer: Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nCopyright: 2013 Mikhail Vorozhtsov <mikhail.vorozhtsov@gmail.com>\nLicense: BSD3\nLicense-File: LICENSE\n\nCabal-Version: >= 1.6.0\nBuild-Type: Simple\n\nSource-Repository head\n  Type: git\n  Location: https://github.com/mvv/data-checked.git\n\nLibrary\n  Build-Depends: base >= 4 && < 5\n               , deepseq >= 1.2\n  Hs-Source-Dirs: src\n  GHC-Options: -Wall\n  Exposed-Modules:\n    Data.Checked\n    Data.Checked.Strict\n\n";
    }