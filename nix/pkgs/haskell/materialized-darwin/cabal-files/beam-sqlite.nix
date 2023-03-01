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
      identifier = { name = "beam-sqlite"; version = "0.5.1.2"; };
      license = "MIT";
      copyright = "(C) 2017-2018 Travis Athougies";
      maintainer = "travis@athougies.net";
      author = "Travis Athougies";
      homepage = "https://haskell-beam.github.io/beam/user-guide/backends/beam-sqlite/";
      url = "";
      synopsis = "Beam driver for SQLite";
      description = "Beam driver for the <https://sqlite.org/ SQLite> embedded database.\nSee <https://haskell-beam.github.io/beam/user-guide/backends/beam-sqlite/ here>\nfor more information";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
          (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))) ++ (pkgs.lib).optional (system.isFreebsd || system.isNetbsd || system.isOpenbsd || system.isOsx || system.isLinux || system.isSolaris || system.isAndroid) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      tests = {
        "beam-sqlite-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
            (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
            (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/beam-sqlite-0.5.1.2.tar.gz";
      sha256 = "f8c30a65c9e574aa6a1bdd6aaf3aa0abca7d2c2be08f2843cab32a454a36ba34";
      });
    }) // {
    package-description-override = "name:                beam-sqlite\nversion:             0.5.1.2\nsynopsis:            Beam driver for SQLite\ndescription:         Beam driver for the <https://sqlite.org/ SQLite> embedded database.\n                     See <https://haskell-beam.github.io/beam/user-guide/backends/beam-sqlite/ here>\n                     for more information\nhomepage:            https://haskell-beam.github.io/beam/user-guide/backends/beam-sqlite/\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Travis Athougies\nmaintainer:          travis@athougies.net\ncopyright:           (C) 2017-2018 Travis Athougies\ncategory:            Database\nbuild-type:          Simple\nextra-source-files:  README.md\nextra-doc-files:     ChangeLog.md\nbug-reports:          https://github.com/haskell-beam/beam/issues\ncabal-version:       1.18\n\nlibrary\n  exposed-modules:    Database.Beam.Sqlite\n                      Database.Beam.Sqlite.Syntax\n                      Database.Beam.Sqlite.Connection\n                      Database.Beam.Sqlite.Migrate\n  other-modules:      Database.Beam.Sqlite.SqliteSpecific\n  build-depends:      base          >=4.7  && <5,\n\n                      beam-core     >=0.9  && <0.10,\n                      beam-migrate  >=0.5  && <0.6,\n\n                      sqlite-simple >=0.4  && <0.5,\n                      text          >=1.0  && <1.3,\n                      bytestring    >=0.10 && <0.12,\n                      hashable      >=1.2  && <1.5,\n                      time          >=1.6  && <1.12,\n                      dlist         >=0.8  && <1.1,\n                      mtl           >=2.1  && <2.3,\n                      free          >=4.12 && <5.2,\n                      scientific    >=0.3  && <0.4,\n                      monad-control        >=1.0  && <1.1,\n                      network-uri   >=2.6  && <2.7,\n                      aeson         >=0.11 && <2.1,\n                      attoparsec    >=0.13 && <0.15,\n                      transformers-base    >=0.4  && <0.5\n  default-language:   Haskell2010\n  default-extensions: ScopedTypeVariables, OverloadedStrings, MultiParamTypeClasses, RankNTypes, FlexibleInstances,\n                      DeriveDataTypeable, DeriveGeneric, StandaloneDeriving, TypeFamilies, GADTs, OverloadedStrings,\n                      CPP, TypeApplications, FlexibleContexts, ConstraintKinds, DerivingStrategies\n  ghc-options:        -Wall\n  if flag(werror)\n    ghc-options:       -Werror\n\n  if os(windows)\n    cpp-options:      -DWINDOWS\n    build-depends:    Win32         >=2.4 && <2.8\n  if os(freebsd) || os(netbsd) || os(openbsd) || os(darwin) || os(linux) || os(solaris) || os(android)\n    cpp-options:      -DUNIX\n    build-depends:    unix          >=2.0 && <2.8\n\ntest-suite beam-sqlite-tests\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  build-depends:\n    base,\n    beam-core,\n    beam-migrate,\n    beam-sqlite,\n    sqlite-simple,\n    tasty,\n    tasty-expected-failure,\n    tasty-hunit,\n    text,\n    time\n  other-modules:\n    Database.Beam.Sqlite.Test\n    Database.Beam.Sqlite.Test.Insert\n    Database.Beam.Sqlite.Test.Migrate\n    Database.Beam.Sqlite.Test.Select\n  default-language: Haskell2010\n  default-extensions:\n    DeriveAnyClass,\n    DeriveGeneric,\n    FlexibleContexts,\n    FlexibleInstances,\n    LambdaCase,\n    MultiParamTypeClasses,\n    OverloadedStrings,\n    RankNTypes,\n    ScopedTypeVariables,\n    StandaloneDeriving,\n    TypeApplications,\n    TypeFamilies\n\nflag werror\n  description: Enable -Werror during development\n  default:     False\n  manual:      True\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-beam/beam.git\n  subdir: beam-sqlite\n";
    }