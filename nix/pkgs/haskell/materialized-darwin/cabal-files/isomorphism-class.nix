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
      specVersion = "3.0";
      identifier = { name = "isomorphism-class"; version = "0.1.0.7"; };
      license = "MIT";
      copyright = "(c) 2022 Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/isomorphism-class";
      url = "";
      synopsis = "Isomorphism typeclass solving the conversion problem";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."isomorphism-class" or (errorHandler.buildDepError "isomorphism-class"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rebase" or (errorHandler.buildDepError "rebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/isomorphism-class-0.1.0.7.tar.gz";
      sha256 = "fed5754cc5c4b8ae5d3c1a12791d1dc00bd475559d6d362f7957d42225cfcf4e";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: isomorphism-class\nversion: 0.1.0.7\nsynopsis: Isomorphism typeclass solving the conversion problem\nhomepage: https://github.com/nikita-volkov/isomorphism-class\nbug-reports: https://github.com/nikita-volkov/isomorphism-class/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2022 Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/isomorphism-class.git\n\ncommon language-settings\n  default-language: Haskell2010\n  default-extensions:\n    FlexibleContexts\n    FlexibleInstances\n    MagicHash\n    MultiParamTypeClasses\n    NoImplicitPrelude\n    ScopedTypeVariables\n    TypeApplications\n    UndecidableSuperClasses\n\ncommon library-dependencies\n  build-depends:\n    base >=4.12 && <5,\n    bytestring >=0.10 && <0.12,\n    containers >=0.6 && <0.7,\n    hashable >=1 && <2,\n    primitive >=0.7.3 && <0.8,\n    text >=1.2 && <3,\n    unordered-containers >=0.2 && <0.3,\n    vector >=0.12 && <0.14,\n\nlibrary\n  import: language-settings, library-dependencies\n  hs-source-dirs: library\n  exposed-modules:\n    IsomorphismClass\n  other-modules:\n    IsomorphismClass.Prelude\n    IsomorphismClass.TextCompat.Array\n\ntest-suite test\n  import: language-settings, library-dependencies\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is: Main.hs\n  other-modules:\n    Test.ExtraInstances\n  build-depends:\n    bytestring >=0.11,\n    isomorphism-class,\n    QuickCheck >=2.13 && <3,\n    quickcheck-instances ==0.3.28,\n    rebase >=1.15 && <2,\n    tasty >=1.2.3 && <2,\n    tasty-hunit >=0.10.0.2 && <0.11,\n    tasty-quickcheck >=0.10.1 && <0.11,\n";
    }