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
      identifier = { name = "contravariant-extras"; version = "0.3.5.3"; };
      license = "MIT";
      copyright = "(c) 2015, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/contravariant-extras";
      url = "";
      synopsis = "Extras for the \"contravariant\" package";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."template-haskell-compat-v0208" or (errorHandler.buildDepError "template-haskell-compat-v0208"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/contravariant-extras-0.3.5.3.tar.gz";
      sha256 = "6a85370aba6dbebfde46a0029039ef329fc1e118794a67a021ed9bf808b58b64";
      });
    }) // {
    package-description-override = "name: contravariant-extras\nversion: 0.3.5.3\ncategory: Control\nsynopsis: Extras for the \"contravariant\" package\nhomepage: https://github.com/nikita-volkov/contravariant-extras\nbug-reports: https://github.com/nikita-volkov/contravariant-extras/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2015, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nbuild-type: Simple\ncabal-version: >=1.10\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/contravariant-extras.git\n\nlibrary\n  hs-source-dirs: library\n  default-extensions: Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language: Haskell2010\n  exposed-modules:\n    Contravariant.Extras\n    Contravariant.Extras.Contrazip\n    Contravariant.Extras.ContrazipLifting\n    Contravariant.Extras.Op\n    Contravariant.Extras.Op.Contrazip\n  other-modules:\n    Contravariant.Extras.Prelude\n    Contravariant.Extras.TH\n  build-depends:\n    base >=4.10 && <5,\n    contravariant >=1.3 && <2,\n    template-haskell >=2.8 && <3,\n    template-haskell-compat-v0208 >=0.1.7 && <2\n";
    }