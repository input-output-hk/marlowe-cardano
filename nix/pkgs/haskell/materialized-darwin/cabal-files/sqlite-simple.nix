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
      identifier = { name = "sqlite-simple"; version = "0.4.18.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011 MailRank, Inc.,\n(c) 2011-2012 Leon P Smith,\n(c) 2012-2014 Janne Hellsten";
      maintainer = "Janne Hellsten <jjhellst@gmail.com>";
      author = "Bryan O'Sullivan, Leon P Smith, Janne Hellsten";
      homepage = "http://github.com/nurpax/sqlite-simple";
      url = "";
      synopsis = "Mid-Level SQLite client library";
      description = "Mid-level SQLite client library, based on postgresql-simple.\n\nMain documentation (with examples): <docs/Database-SQLite-Simple.html Database.SQLite.Simple>\n\nYou can view the project page at <http://github.com/nurpax/sqlite-simple>\nfor more information.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."blaze-textual" or (errorHandler.buildDepError "blaze-textual"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."direct-sqlite" or (errorHandler.buildDepError "direct-sqlite"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."Only" or (errorHandler.buildDepError "Only"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."direct-sqlite" or (errorHandler.buildDepError "direct-sqlite"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/sqlite-simple-0.4.18.2.tar.gz";
      sha256 = "dc15b1a7dd5428f218d40cd8b3e51d4b559d5d86c52639ba3f0f811813735d91";
      });
    }) // {
    package-description-override = "Name:                sqlite-simple\nVersion:             0.4.18.2\nSynopsis:            Mid-Level SQLite client library\nDescription:\n    Mid-level SQLite client library, based on postgresql-simple.\n    .\n    Main documentation (with examples): <docs/Database-SQLite-Simple.html Database.SQLite.Simple>\n    .\n    You can view the project page at <http://github.com/nurpax/sqlite-simple>\n    for more information.\n\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Bryan O'Sullivan, Leon P Smith, Janne Hellsten\nMaintainer:          Janne Hellsten <jjhellst@gmail.com>\nCopyright:           (c) 2011 MailRank, Inc.,\n                     (c) 2011-2012 Leon P Smith,\n                     (c) 2012-2014 Janne Hellsten\nHomepage:            http://github.com/nurpax/sqlite-simple\nbug-reports:         http://github.com/nurpax/sqlite-simple/issues\nStability:           stable\nCategory:            Database\nBuild-type:          Simple\n\nCabal-version:       >= 1.10\n\nextra-source-files:  README.markdown\n                     changelog\n\nLibrary\n  Default-language:  Haskell2010\n  Exposed-modules:\n     Database.SQLite.Simple\n     Database.SQLite.Simple.Ok\n     Database.SQLite.Simple.FromField\n     Database.SQLite.Simple.FromRow\n     Database.SQLite.Simple.Internal\n     Database.SQLite.Simple.QQ\n     Database.SQLite.Simple.ToField\n     Database.SQLite.Simple.ToRow\n     Database.SQLite.Simple.Types\n     Database.SQLite.Simple.Function\n     Database.SQLite.Simple.Time\n     Database.SQLite.Simple.Time.Implementation\n\n  Build-depends:\n    attoparsec >= 0.10.3,\n    base < 5,\n    blaze-builder,\n    blaze-textual,\n    bytestring >= 0.9,\n    containers,\n    direct-sqlite >= 2.3.13 && < 2.4,\n    template-haskell,\n    text >= 0.11,\n    time,\n    transformers,\n    Only >= 0.1 && < 0.1.1\n    \n  if impl(ghc < 8.0)\n    Build-depends: semigroups >= 0.18 && < 0.20\n\n  default-extensions:\n      DoAndIfThenElse\n    , OverloadedStrings\n    , BangPatterns\n    , ViewPatterns\n    , TypeOperators\n\n  ghc-options: -Wall -fno-warn-name-shadowing\n\nsource-repository head\n  type:     git\n  location: http://github.com/nurpax/sqlite-simple\n\n\ntest-suite test\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n\n  hs-source-dirs: test\n  main-is:        Main.hs\n  other-modules:  Common\n                , Debug\n                , DirectSqlite\n                , Errors\n                , Fold\n                , Function\n                , ParamConv\n                , QQ\n                , Simple\n                , Statement\n                , TestImports\n                , UserInstances\n                , Utf8Strings\n\n  ghc-options: -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind\n\n  default-extensions:\n      NamedFieldPuns\n    , OverloadedStrings\n    , Rank2Types\n    , RecordWildCards\n\n  build-depends: base\n               , base16-bytestring\n               , bytestring >= 0.9\n               , HUnit\n               , sqlite-simple\n               , direct-sqlite\n               , text\n               , time\n";
    }