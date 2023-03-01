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
      specVersion = "1.12";
      identifier = { name = "vector-sized"; version = "1.5.0"; };
      license = "BSD-3-Clause";
      copyright = "2016 Joe Hermaszewski";
      maintainer = "whats.our.vector.victor@monoid.al";
      author = "Joe Hermaszewski";
      homepage = "https://github.com/expipiplus1/vector-sized#readme";
      url = "";
      synopsis = "Size tagged vectors";
      description = "Please see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."finite-typelits" or (errorHandler.buildDepError "finite-typelits"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-list-literals" or (errorHandler.buildDepError "indexed-list-literals"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-sized-1.5.0.tar.gz";
      sha256 = "54deae5e1d504821d63d5eedfcaea0d01b26af3b8123971ad7f1241326c3048e";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.4.\n--\n-- see: https://github.com/sol/hpack\n--\n-- hash: 8cc33c7ff1b35236dcaed143ba951337eb4f12e3c53c616fa15ec2a073c0942b\n\nname:           vector-sized\nversion:        1.5.0\nsynopsis:       Size tagged vectors\ndescription:    Please see README.md\ncategory:       Data\nhomepage:       https://github.com/expipiplus1/vector-sized#readme\nbug-reports:    https://github.com/expipiplus1/vector-sized/issues\nauthor:         Joe Hermaszewski\nmaintainer:     whats.our.vector.victor@monoid.al\ncopyright:      2016 Joe Hermaszewski\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    package.yaml\n    readme.md\n    changelog.md\n    default.nix\n\nsource-repository head\n  type: git\n  location: https://github.com/expipiplus1/vector-sized\n\nlibrary\n  exposed-modules:\n      Data.Vector.Generic.Mutable.Sized\n      Data.Vector.Generic.Mutable.Sized.Internal\n      Data.Vector.Generic.Sized\n      Data.Vector.Generic.Sized.Internal\n      Data.Vector.Mutable.Sized\n      Data.Vector.Primitive.Mutable.Sized\n      Data.Vector.Primitive.Sized\n      Data.Vector.Sized\n      Data.Vector.Storable.Mutable.Sized\n      Data.Vector.Storable.Sized\n      Data.Vector.Unboxed.Mutable.Sized\n      Data.Vector.Unboxed.Sized\n  other-modules:\n      Paths_vector_sized\n  hs-source-dirs:\n      src\n  build-depends:\n      adjunctions >=4.3 && <4.5\n    , base >=4.9 && <5\n    , binary >=0.8.3.0\n    , comonad >=4 && <6\n    , deepseq >=1.1 && <1.5\n    , distributive >=0.5 && <0.7\n    , finite-typelits >=0.1\n    , hashable >=1.2.4.0\n    , indexed-list-literals >=0.2.0.0\n    , primitive >=0.5 && <0.8\n    , vector >=0.11 && <0.13\n  default-language: Haskell2010\n";
    }