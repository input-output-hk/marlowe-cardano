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
      specVersion = "1.4";
      identifier = { name = "multiplate"; version = "0.0.3"; };
      license = "MIT";
      copyright = "2010, Russell O'Connor";
      maintainer = "Russell O'Connor <roconnor@theorem.ca>";
      author = "Russell O'Connor";
      homepage = "http://haskell.org/haskellwiki/Multiplate";
      url = "";
      synopsis = "Lightweight generic library for mutually recursive data types.";
      description = "Multiplate is an alternative extension of the Uniplate/Compos core library\nto support mutally recursive\ndatatypes in a way that is as powerful as Compos, as easy to use as Biplate, and\nmore portable than both of them.\nMultiplate does not require GADTs and does not require multi-parameter type classes.\nIt only requires rank 3 polymorphism.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/multiplate-0.0.3.tar.gz";
      sha256 = "2c0016847dcedc8ba0054211256a3ef6c7f142e605668c7b64beebdf0eaf4ebf";
      });
    }) // {
    package-description-override = "Name:               multiplate\r\nVersion:            0.0.3\r\nx-revision: 1\r\nCabal-Version:      >= 1.4\r\nLicense:            MIT\r\nLicense-File:       LICENSE\r\nBuild-Type:         Simple\r\nCopyright:          2010, Russell O'Connor\r\nAuthor:             Russell O'Connor\r\nMaintainer:         Russell O'Connor <roconnor@theorem.ca>\r\nHomepage:           http://haskell.org/haskellwiki/Multiplate\r\nSynopsis:           Lightweight generic library for mutually recursive data types.\r\nCategory:           Generics, Lenses\r\nDescription:\r\n    Multiplate is an alternative extension of the Uniplate/Compos core library\r\n    to support mutally recursive\r\n    datatypes in a way that is as powerful as Compos, as easy to use as Biplate, and\r\n    more portable than both of them.\r\n\r\n    Multiplate does not require GADTs and does not require multi-parameter type classes.\r\n    It only requires rank 3 polymorphism.\r\nTested-with:        GHC == 6.12.3, GHC == 7.0.4, GHC == 7.2.2, GHC == 7.4.2, GHC == 7.6.3, GHC == 7.8.4, GHC == 7.10.2\r\ndata-files:         CHANGELOG\r\n\r\nLibrary\r\n    -- see https://github.com/haskell-infra/hackage-trustees/issues/156\r\n    Build-Depends:     base >= 3 && < 5, transformers >= 0.2 && < 0.6\r\n    Extensions: RankNTypes, ScopedTypeVariables\r\n    Exposed-modules:\r\n        Data.Generics.Multiplate\r\n\r\n";
    }