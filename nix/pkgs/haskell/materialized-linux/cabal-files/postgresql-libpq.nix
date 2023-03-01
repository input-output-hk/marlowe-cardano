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
    flags = { use-pkg-config = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "postgresql-libpq"; version = "0.9.4.3"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2010 Grant Monroe\n(c) 2011 Leon P Smith";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Grant Monroe, Leon P Smith, Joey Adams";
      homepage = "https://github.com/haskellari/postgresql-libpq";
      url = "";
      synopsis = "low-level binding to libpq";
      description = "This is a binding to libpq: the C application\nprogrammer's interface to PostgreSQL. libpq is a\nset of library functions that allow client\nprograms to pass queries to the PostgreSQL\nbackend server and to receive the results of\nthese queries.";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        ];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        libs = (pkgs.lib).optionals (!flags.use-pkg-config) (if system.isWindows
          then [ (pkgs."libpq" or (errorHandler.sysDepError "libpq")) ]
          else [
            (pkgs."pq" or (errorHandler.sysDepError "pq"))
            ] ++ (pkgs.lib).optionals (system.isOpenbsd) [
            (pkgs."crypto" or (errorHandler.sysDepError "crypto"))
            (pkgs."ssl" or (errorHandler.sysDepError "ssl"))
            ]);
        pkgconfig = (pkgs.lib).optional (flags.use-pkg-config) (pkgconfPkgs."libpq" or (errorHandler.pkgConfDepError "libpq"));
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      tests = {
        "smoke" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."postgresql-libpq" or (errorHandler.buildDepError "postgresql-libpq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/postgresql-libpq-0.9.4.3.tar.gz";
      sha256 = "e3e246dcd55352fce514969a72a6fe37771102034fb8e662fdc7ae780b83d6bd";
      });
    }) // {
    package-description-override = "name:               postgresql-libpq\nversion:            0.9.4.3\nx-revision:         3\nsynopsis:           low-level binding to libpq\ndescription:\n  This is a binding to libpq: the C application\n  programmer's interface to PostgreSQL. libpq is a\n  set of library functions that allow client\n  programs to pass queries to the PostgreSQL\n  backend server and to receive the results of\n  these queries.\n\nhomepage:           https://github.com/haskellari/postgresql-libpq\nbug-reports:        https://github.com/haskellari/postgresql-libpq/issues\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Grant Monroe, Leon P Smith, Joey Adams\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\ncopyright:\n  (c) 2010 Grant Monroe\n  (c) 2011 Leon P Smith\n\ncategory:           Database\nbuild-type:         Custom\nextra-source-files: cbits/noticehandlers.h\ncabal-version:      >=1.10\ntested-with:\n  GHC ==7.0.4\n   || ==7.2.2\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.2\n\nextra-source-files: CHANGELOG.md\n\ncustom-setup\n  setup-depends:\n      base   >=4.3  && <5\n    , Cabal  >=1.10 && <3.9\n\n-- If true,  use pkg-config,  otherwise use the pg_config based build\n-- configuration\nflag use-pkg-config\n  default: False\n  manual:  True\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  c-sources:        cbits/noticehandlers.c\n  include-dirs:     cbits\n  exposed-modules:\n    Database.PostgreSQL.LibPQ\n    Database.PostgreSQL.LibPQ.Internal\n\n  other-modules:    Database.PostgreSQL.LibPQ.Compat\n  build-depends:\n      base        >=4.3     && <4.18\n    , bytestring  >=0.9.1.0 && <0.12\n\n  if !os(windows)\n    build-depends: unix >=2.4.2.0 && <2.8\n\n  if os(windows)\n    build-depends: Win32 >=2.2.0.2 && <2.14\n\n  ghc-options:      -Wall\n\n  if flag(use-pkg-config)\n    pkgconfig-depends: libpq ==9.3 || >9.3\n\n  else\n    if os(windows)\n      -- Due to https://sourceware.org/bugzilla/show_bug.cgi?id=22948,\n      -- if we specify pq instead of libpq, then ld might link against\n      -- libpq.dll directly, which can lead to segfaults. As a temporary hack,\n      -- we force ld to link against the libpq.lib import library directly\n      -- by specifying libpq here.\n      extra-libraries: libpq\n\n    else\n      extra-libraries: pq\n\n      if os(openbsd)\n        extra-libraries:\n          crypto\n          ssl\n\n  -- Other-modules:\n  build-tools:      hsc2hs -any\n\ntest-suite smoke\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          Smoke.hs\n  hs-source-dirs:   test\n  build-depends:\n      base\n    , bytestring\n    , postgresql-libpq\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/postgresql-libpq\n";
    }