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
    flags = { examples = false; };
    package = {
      specVersion = "1.20";
      identifier = { name = "servant-pagination"; version = "2.5.0"; };
      license = "LGPL-3.0-only";
      copyright = "(c) 2018-2020 Chordify";
      maintainer = "Chordify <haskelldevelopers@chordify.net>\nMatthias Benkort <matthias.benkort@gmail.com>";
      author = "Chordify";
      homepage = "https://github.com/chordify/haskell-servant-pagination";
      url = "";
      synopsis = "Type-safe pagination for Servant APIs";
      description = "This module offers opinionated helpers to declare a type-safe and a\nflexible pagination mecanism for Servant APIs. This design, inspired by\nHeroku's API, provides a small framework to communicate about a possible\npagination feature of an endpoint, enabling a client to consume the API in\ndifferent fashions (pagination with offset / limit, endless scroll using\nlast referenced resources, ascending and descending ordering, etc.)";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."uri-encode" or (errorHandler.buildDepError "uri-encode"))
          ];
        buildable = true;
        };
      exes = {
        "servant-pagination-simple" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-pagination" or (errorHandler.buildDepError "servant-pagination"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "servant-pagination-complex" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-pagination" or (errorHandler.buildDepError "servant-pagination"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = if !flags.examples then false else true;
          };
        };
      tests = {
        "servant-pagination-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."servant-pagination" or (errorHandler.buildDepError "servant-pagination"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/servant-pagination-2.5.0.tar.gz";
      sha256 = "c39bae1a4ae93b7f00f4ffeea096d46f9aba2ac12541ae636340545a4b3e41c5";
      });
    }) // {
    package-description-override = "name:\n    servant-pagination\nsynopsis:\n    Type-safe pagination for Servant APIs\ndescription:\n    This module offers opinionated helpers to declare a type-safe and a\n    flexible pagination mecanism for Servant APIs. This design, inspired by\n    Heroku's API, provides a small framework to communicate about a possible\n    pagination feature of an endpoint, enabling a client to consume the API in\n    different fashions (pagination with offset / limit, endless scroll using\n    last referenced resources, ascending and descending ordering, etc.)\nversion:\n    2.5.0\nhomepage:\n    https://github.com/chordify/haskell-servant-pagination\nbug-reports:\n    https://github.com/chordify/haskell-servant-pagination/issues\nlicense:\n    LGPL-3\nlicense-file:\n    LICENSE\nauthor:\n    Chordify\nmaintainer:\n    Chordify <haskelldevelopers@chordify.net>\n    Matthias Benkort <matthias.benkort@gmail.com>\ncopyright:\n    (c) 2018-2020 Chordify\ncategory:\n    Web\nbuild-type:\n    Simple\ncabal-version:\n    1.20\nextra-source-files:\n    README.md\n    CHANGELOG.md\n    stack.yaml\n    Setup.hs\n    .stylish-haskell.yaml\n\nsource-repository head\n  type:\n      git\n  location:\n      git://github.com/chordify/haskell-servant-pagination.git\n\nflag examples\n  description:\n      build examples executables\n  default:\n      False\n  manual:\n      True\n\n\nlibrary\n  default-language:\n    Haskell2010\n  ghc-options:\n    -Wall\n  default-extensions:\n      BangPatterns\n    , ConstraintKinds\n    , DataKinds\n    , DefaultSignatures\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveGeneric\n    , ExistentialQuantification\n    , FlexibleContexts\n    , FlexibleInstances\n    , GADTs\n    , KindSignatures\n    , MultiParamTypeClasses\n    , OverloadedStrings\n    , ParallelListComp\n    , ScopedTypeVariables\n    , TupleSections\n    , TypeFamilies\n    , TypeOperators\n    , UndecidableInstances\n\n  build-depends:\n      base >= 4 && < 5\n    , text >= 1.2 && < 2.1\n    , servant >= 0.11 && < 0.20\n    , servant-server >= 0.11 && < 0.20\n    , safe >= 0.3 && < 1\n    , uri-encode >= 1.5 && < 1.6\n\n  hs-source-dirs:\n    src\n  exposed-modules:\n    Servant.Pagination\n\n\nexecutable servant-pagination-simple\n  if !flag(examples)\n    buildable: False\n\n  default-language:\n      Haskell2010\n  ghc-options:\n      -Wall\n      -threaded\n      -rtsopts\n      -with-rtsopts=-N\n  default-extensions:\n      BangPatterns\n    , ConstraintKinds\n    , DataKinds\n    , DefaultSignatures\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveGeneric\n    , ExistentialQuantification\n    , FlexibleContexts\n    , FlexibleInstances\n    , GADTs\n    , KindSignatures\n    , MultiParamTypeClasses\n    , OverloadedStrings\n    , ParallelListComp\n    , ScopedTypeVariables\n    , TupleSections\n    , TypeFamilies\n    , TypeOperators\n\n  build-depends:\n      base >= 4 && < 5\n    , aeson >= 1.2 && < 2\n    , servant >= 0.11 && < 0.19\n    , servant-pagination\n    , servant-server >= 0.11 && < 0.19\n    , warp >= 3.2 && < 4\n\n  hs-source-dirs:\n      examples\n  main-is:\n      Simple.hs\n  other-modules:\n      Color\n\n\nexecutable servant-pagination-complex\n  if !flag(examples)\n    buildable: False\n\n  default-language:\n      Haskell2010\n  ghc-options:\n      -Wall\n      -threaded\n      -rtsopts\n      -with-rtsopts=-N\n  default-extensions:\n      BangPatterns\n    , ConstraintKinds\n    , DataKinds\n    , DefaultSignatures\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveGeneric\n    , ExistentialQuantification\n    , FlexibleContexts\n    , FlexibleInstances\n    , GADTs\n    , KindSignatures\n    , MultiParamTypeClasses\n    , OverloadedStrings\n    , ParallelListComp\n    , ScopedTypeVariables\n    , TupleSections\n    , TypeFamilies\n    , TypeOperators\n\n  build-depends:\n      base >= 4 && < 5\n    , aeson >= 1.2 && < 2\n    , servant >= 0.11 && < 0.19\n    , servant-pagination\n    , servant-server >= 0.11 && < 0.19\n    , warp >= 3.2 && < 4\n\n  hs-source-dirs:\n      examples\n  main-is:\n      Complex.hs\n  other-modules:\n      Color\n\n\ntest-suite servant-pagination-test\n  type:\n      exitcode-stdio-1.0\n  default-language:\n      Haskell2010\n  ghc-options:\n      -Wall\n  default-extensions:\n      BangPatterns\n    , ConstraintKinds\n    , DataKinds\n    , DefaultSignatures\n    , DeriveDataTypeable\n    , DeriveFunctor\n    , DeriveGeneric\n    , ExistentialQuantification\n    , FlexibleContexts\n    , FlexibleInstances\n    , GADTs\n    , KindSignatures\n    , MultiParamTypeClasses\n    , OverloadedStrings\n    , ParallelListComp\n    , ScopedTypeVariables\n    , TupleSections\n    , TypeFamilies\n    , TypeOperators\n\n  build-depends:\n      base\n    , hspec\n    , QuickCheck\n    , servant-pagination\n    , servant-server\n    , text\n\n  hs-source-dirs:\n      test\n  main-is:\n      Spec.hs\n  other-modules:\n      Servant.PaginationSpec\n";
    }