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
    flags = { werror = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "beam-migrate"; version = "0.5.1.2"; };
      license = "MIT";
      copyright = "Copyright (C) 2017-2018 Travis Athougies";
      maintainer = "travis@athougies.net";
      author = "Travis Athougies";
      homepage = "https://travis.athougies.net/projects/beam.html";
      url = "";
      synopsis = "SQL DDL support and migrations support library for Beam";
      description = "This package provides type classes to allow backends to implement\nSQL DDL support for beam. This allows you to use beam syntax to\nwrite type-safe schema generation code.\nThe package also provides features to introspect beam schemas,\nand support for automatic generation of migrations in SQL and\nHaskell formats.\nThis is mostly a low-level support library. Most often, this\nlibrary is used to write tooling to support DDL manipulation in\nyour project, or to enable migrations support in beam backends.\nFor a more turnkey solution for database migrations, consider\nthe <http://hackage.haskell.org/package/beam-migrate-cli beam-migrate>\ncommand line tool. This provides out-of-the-box support for migrations,\nschema change management, and version control, based on the features\nprovided in this library.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."pqueue" or (errorHandler.buildDepError "pqueue"))
          (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/beam-migrate-0.5.1.2.tar.gz";
      sha256 = "bb8f1116e8d7d340dcdd0b92249b2e804d43489e45a43f29fb857d6a7c5936c0";
      });
    }) // {
    package-description-override = "name:                beam-migrate\nversion:             0.5.1.2\nsynopsis:            SQL DDL support and migrations support library for Beam\ndescription:         This package provides type classes to allow backends to implement\n                     SQL DDL support for beam. This allows you to use beam syntax to\n                     write type-safe schema generation code.\n\n                     The package also provides features to introspect beam schemas,\n                     and support for automatic generation of migrations in SQL and\n                     Haskell formats.\n\n                     This is mostly a low-level support library. Most often, this\n                     library is used to write tooling to support DDL manipulation in\n                     your project, or to enable migrations support in beam backends.\n\n                     For a more turnkey solution for database migrations, consider\n                     the <http://hackage.haskell.org/package/beam-migrate-cli beam-migrate>\n                     command line tool. This provides out-of-the-box support for migrations,\n                     schema change management, and version control, based on the features\n                     provided in this library.\n\nhomepage:            https://travis.athougies.net/projects/beam.html\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Travis Athougies\nmaintainer:          travis@athougies.net\ncopyright:           Copyright (C) 2017-2018 Travis Athougies\ncategory:            Database\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md\ncabal-version:       1.18\n\nlibrary\n  exposed-modules:     Database.Beam.Migrate\n\n                       Database.Beam.Migrate.Types\n                       Database.Beam.Migrate.Checks\n                       Database.Beam.Migrate.Actions\n                       Database.Beam.Migrate.Backend\n                       Database.Beam.Migrate.Serialization\n\n                       Database.Beam.Migrate.SQL\n                       Database.Beam.Migrate.SQL.Builder\n                       Database.Beam.Migrate.SQL.Tables\n                       Database.Beam.Migrate.SQL.SQL92\n                       Database.Beam.Migrate.SQL.BeamExtensions\n\n                       Database.Beam.Migrate.Log\n\n                       Database.Beam.Migrate.Generics\n\n                       Database.Beam.Migrate.Simple\n\n                       Database.Beam.Haskell.Syntax\n\n  other-modules:       Database.Beam.Migrate.Generics.Types\n                       Database.Beam.Migrate.Generics.Tables\n                       Database.Beam.Migrate.SQL.Types\n                       Database.Beam.Migrate.Types.CheckedEntities\n                       Database.Beam.Migrate.Types.Predicates\n\n  build-depends:       base                 >=4.9     && <5.0,\n                       beam-core            >=0.9     && <0.10,\n                       text                 >=1.2     && <1.3,\n                       aeson                >=0.11    && <2.1,\n                       bytestring           >=0.10    && <0.12,\n                       free                 >=4.12    && <5.2,\n                       time                 >=1.6     && <1.12,\n                       mtl                  >=2.2     && <2.3,\n                       scientific           >=0.3     && <0.4,\n                       vector               >=0.11    && <0.13,\n                       containers           >=0.5     && <0.7,\n                       unordered-containers >=0.2     && <0.3,\n                       hashable             >=1.2     && <1.5,\n                       microlens            >=0.4     && <0.5,\n                       parallel             >=3.2     && <3.3,\n                       deepseq              >=1.4     && <1.5,\n                       ghc-prim             >=0.5     && <0.9,\n                       containers           >=0.5     && <0.7,\n                       haskell-src-exts     >=1.18    && <1.24,\n                       pretty               >=1.1     && <1.2,\n                       dependent-map        >=0.2     && <0.5,\n                       dependent-sum        >=0.4     && <0.8,\n                       pqueue               >=1.3     && <1.5,\n                       uuid-types           >=1.0     && <1.1\n  default-language:    Haskell2010\n  default-extensions:  KindSignatures, OverloadedStrings, TypeFamilies, FlexibleContexts,\n                       StandaloneDeriving, GADTs, DeriveFunctor, RankNTypes, ScopedTypeVariables,\n                       FlexibleInstances, TypeOperators, TypeApplications, MultiParamTypeClasses,\n                       DataKinds, DeriveGeneric\n  ghc-options:         -Wall\n  if flag(werror)\n    ghc-options:       -Werror\n\nflag werror\n  description: Enable -Werror during development\n  default:     False\n  manual:      True\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-beam/beam.git\n  subdir: beam-migrate\n";
    }