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
    flags = {
      systemlib = false;
      fulltextsearch = true;
      urifilenames = true;
      haveusleep = true;
      json1 = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "direct-sqlite"; version = "2.3.27"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2012 - 2014 Irene Knapp,\n2014 - 2018 Janne Hellsten,\n2018 - 2020 Sergey Bushnyak,\n2022        Joshua Chia";
      maintainer = "Joshua Chia <joshchia@gmail.com>";
      author = "Irene Knapp <irene.knapp@icloud.com>";
      homepage = "https://github.com/IreneKnapp/direct-sqlite";
      url = "";
      synopsis = "Low-level binding to SQLite3.  Includes UTF8 and BLOB support.";
      description = "This package is not very different from the other SQLite3 bindings out\nthere, but it fixes a few deficiencies I was finding.  As compared to\nbindings-sqlite3, it is slightly higher-level, in that it supports\nmarshalling of data values to and from the database.  In particular,\nit supports strings encoded as UTF8, and BLOBs represented as\nByteStrings.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        libs = if flags.systemlib
          then [ (pkgs."sqlite3" or (errorHandler.sysDepError "sqlite3")) ]
          else (pkgs.lib).optional (!system.isWindows && !system.isAndroid) (pkgs."pthread" or (errorHandler.sysDepError "pthread"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."direct-sqlite" or (errorHandler.buildDepError "direct-sqlite"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/direct-sqlite-2.3.27.tar.gz";
      sha256 = "0319dd780712859fcacf0b8f1401088f76af3b651c7ec366e0231c15e73bd82e";
      });
    }) // {
    package-description-override = "name:               direct-sqlite\nversion:            2.3.27\nsynopsis:           Low-level binding to SQLite3.  Includes UTF8 and BLOB support.\ndescription:        This package is not very different from the other SQLite3 bindings out\n                    there, but it fixes a few deficiencies I was finding.  As compared to\n                    bindings-sqlite3, it is slightly higher-level, in that it supports\n                    marshalling of data values to and from the database.  In particular,\n                    it supports strings encoded as UTF8, and BLOBs represented as\n                    ByteStrings.\nlicense:            BSD3\nlicense-file:       LICENSE\ncopyright:          Copyright (c) 2012 - 2014 Irene Knapp,\n                    2014 - 2018 Janne Hellsten,\n                    2018 - 2020 Sergey Bushnyak,\n                    2022        Joshua Chia\nauthor:             Irene Knapp <irene.knapp@icloud.com>\nmaintainer:         Joshua Chia <joshchia@gmail.com>\ncategory:           Database\nhomepage:           https://github.com/IreneKnapp/direct-sqlite\nbug-reports:        https://github.com/IreneKnapp/direct-sqlite/issues/new\nbuild-type:         Simple\nextra-source-files: cbits/sqlite3.c\n                    cbits/sqlite3.h\n                    cbits/sqlite3ext.h\n                    changelog\ncabal-version:      >= 1.10\n\nsource-repository head\n  type:     git\n  location: git://github.com/IreneKnapp/direct-sqlite.git\n\nflag systemlib\n  default:     False\n  description: Use the system-wide sqlite library\n\nflag fulltextsearch\n  default:     True\n  description: Enable full-text search when using the bundled sqlite library\n\nflag urifilenames\n  default:     True\n  description: Enable URI filenames when using the bundled sqlite library\n\nflag haveusleep\n  default:     True\n  description: Enable use of os function usleep.\n\nflag json1\n  default:     True\n  description: Enable json1 extension.\n\nlibrary\n  exposed-modules:  Database.SQLite3\n                    Database.SQLite3.Bindings\n                    Database.SQLite3.Bindings.Types\n                    Database.SQLite3.Direct\n  build-depends:    base       >= 4.1 && < 5\n                  , bytestring >= 0.9.2.1\n                  , text       >= 0.11\n  default-language: Haskell2010\n  include-dirs:     .\n  ghc-options:      -Wall -fwarn-tabs\n\n  if impl(ghc < 8.0)\n    build-depends: semigroups >= 0.18 && < 0.20\n\n  if flag(systemlib)\n    extra-libraries: sqlite3\n    cpp-options:     -Ddirect_sqlite_systemlib\n  else\n    c-sources:        cbits/sqlite3.c\n    install-includes: sqlite3.h, sqlite3ext.h\n    include-dirs:     cbits\n\n    if !os(windows) && !os(android)\n      extra-libraries: pthread\n\n    if flag(fulltextsearch)\n      cc-options: -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS3_PARENTHESIS\n                  -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS5\n\n    if flag(urifilenames)\n      cc-options: -DSQLITE_USE_URI\n\n    if flag(haveusleep)\n      cc-options: -DHAVE_USLEEP\n\n    if flag(json1)\n      cc-options: -DSQLITE_ENABLE_JSON1\n\ntest-suite test\n  type:               exitcode-stdio-1.0\n  main-is:            Main.hs\n  other-modules:      StrictEq\n  hs-source-dirs:     test\n  build-depends:      base\n                    , HUnit\n                    , base16-bytestring\n                    , bytestring\n                    , direct-sqlite\n                    , directory\n                    , temporary\n                    , text\n  default-language:   Haskell2010\n  default-extensions: Rank2Types\n                      ScopedTypeVariables\n                      NamedFieldPuns\n                      RecordWildCards\n                      OverloadedStrings\n                      DeriveDataTypeable\n  ghc-options:        -Wall -threaded -fno-warn-name-shadowing -fno-warn-unused-do-bind\n";
    }