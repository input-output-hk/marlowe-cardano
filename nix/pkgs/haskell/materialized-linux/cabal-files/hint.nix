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
      identifier = { name = "hint"; version = "0.9.0.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "\"Samuel Gélineau\" <gelisam@gmail.com>";
      author = "The Hint Authors";
      homepage = "https://github.com/haskell-hint/hint";
      url = "";
      synopsis = "A Haskell interpreter built on top of the GHC API";
      description = "This library defines an Interpreter monad. It allows to load Haskell\nmodules, browse them, type-check and evaluate strings with Haskell\nexpressions and even coerce them into values. The library is thread-safe\nand type-safe (even the coercion of expressions to values).\nIt is, essentially, a huge subset of the GHC API wrapped in a simpler\nAPI.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
          (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hint" or (errorHandler.buildDepError "hint"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hint-0.9.0.6.tar.gz";
      sha256 = "7ed65eee263372fdb3f96e16214d521529b0e524ee1946783786e51051fff2c8";
      });
    }) // {
    package-description-override = "name:         hint\nversion:      0.9.0.6\ndescription:\n        This library defines an Interpreter monad. It allows to load Haskell\n        modules, browse them, type-check and evaluate strings with Haskell\n        expressions and even coerce them into values. The library is thread-safe\n        and type-safe (even the coercion of expressions to values).\n\n        It is, essentially, a huge subset of the GHC API wrapped in a simpler\n        API.\n\nsynopsis:     A Haskell interpreter built on top of the GHC API\ncategory:     Language, Compilers/Interpreters\nlicense:      BSD3\nlicense-file: LICENSE\nauthor:       The Hint Authors\nmaintainer:   \"Samuel Gélineau\" <gelisam@gmail.com>\nhomepage:     https://github.com/haskell-hint/hint\n\ncabal-version: >= 1.10\nbuild-type:    Simple\n\nextra-source-files: README.md\n                    AUTHORS\n                    CHANGELOG.md\n                    examples/example.hs\n                    examples/SomeModule.hs\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-hint/hint\n\ntest-suite unit-tests\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: unit-tests\n  main-is:        run-unit-tests.hs\n  default-language: Haskell2010\n\n  build-depends:  base == 4.*,\n                  bytestring,\n                  hint,\n                  HUnit,\n                  directory,\n                  filepath,\n                  exceptions >= 0.10.0,\n                  stm,\n                  text,\n                  typed-process,\n\n                  -- packages used by setImports calls\n                  containers\n\n  if impl(ghc >= 8.10) {\n      cpp-options: -DTHREAD_SAFE_LINKER\n  }\n\n  if !os(windows) {\n      build-depends: unix >= 2.2.0.0\n  }\n\n  default-extensions:  CPP\n\nlibrary\n  default-language: Haskell2010\n  build-depends: base == 4.*,\n                 containers,\n                 ghc >= 8.4 && < 9.3,\n                 ghc-paths,\n                 ghc-boot,\n                 transformers,\n                 filepath,\n                 exceptions == 0.10.*,\n                 random,\n                 directory,\n                 temporary\n\n  if impl(ghc >= 8.10) {\n      cpp-options: -DTHREAD_SAFE_LINKER\n  }\n\n  if !os(windows) {\n      build-depends: unix >= 2.2.0.0\n  }\n\n  exposed-modules: Language.Haskell.Interpreter\n                   Language.Haskell.Interpreter.Extension\n                   Language.Haskell.Interpreter.Unsafe\n                   Hint.Internal\n  other-modules:   Hint.GHC\n                   Hint.Base\n                   Hint.InterpreterT\n                   Hint.CompatPlatform\n                   Hint.Configuration\n                   Hint.Extension\n                   Hint.Context\n                   Hint.Conversions\n                   Hint.Eval\n                   Hint.Parsers\n                   Hint.Reflection\n                   Hint.Typecheck\n                   Hint.Util\n                   Hint.Annotations\n                   Control.Monad.Ghc\n\n  hs-source-dirs: src\n\n  ghc-options: -Wall\n  default-extensions:  CPP\n                       GeneralizedNewtypeDeriving\n                       DeriveDataTypeable\n                       MagicHash\n                       FunctionalDependencies\n                       Rank2Types\n                       ScopedTypeVariables\n                       ExistentialQuantification\n                       LambdaCase\n";
    }