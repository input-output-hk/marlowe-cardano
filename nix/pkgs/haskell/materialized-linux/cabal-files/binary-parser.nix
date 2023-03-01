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
      identifier = { name = "binary-parser"; version = "0.5.7.2"; };
      license = "MIT";
      copyright = "(c) 2015, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/binary-parser";
      url = "";
      synopsis = "A highly-efficient but limited parser API specialised for bytestrings";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."binary-parser" or (errorHandler.buildDepError "binary-parser"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
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
      url = "http://hackage.haskell.org/package/binary-parser-0.5.7.2.tar.gz";
      sha256 = "870775819775d648301960d632eaf68082e756b5b9f3e271fd54c7ab5dd927dc";
      });
    }) // {
    package-description-override = "name:\n  binary-parser\nversion:\n  0.5.7.2\nsynopsis:\n  A highly-efficient but limited parser API specialised for bytestrings\ncategory:\n  Parser, Binary\nhomepage:\n  https://github.com/nikita-volkov/binary-parser\nbug-reports:\n  https://github.com/nikita-volkov/binary-parser/issues\nauthor:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright:\n  (c) 2015, Nikita Volkov\nlicense:\n  MIT\nlicense-file:\n  LICENSE\nbuild-type:\n  Simple\ncabal-version:\n  >=1.10\n\nsource-repository head\n  type:\n    git\n  location:\n    git://github.com/nikita-volkov/binary-parser.git\n\nlibrary\n  hs-source-dirs:\n    library\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DerivingVia, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  other-modules:\n    BinaryParser.Prelude\n  exposed-modules:\n    BinaryParser\n  build-depends:\n    -- data:\n    bytestring >= 0.10 && < 0.12,\n    text >= 1 && < 3,\n    -- general:\n    mtl == 2.*,\n    transformers >= 0.4 && < 0.7,\n    base >= 4.12 && < 5\n\ntest-suite tests\n  type:\n    exitcode-stdio-1.0\n  hs-source-dirs:\n    tests\n  main-is:\n    Main.hs\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, DerivingVia, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  build-depends:\n    binary-parser,\n    QuickCheck >=2.10 && <3,\n    quickcheck-instances >=0.3.22 && <0.4,\n    rerebase <2,\n    tasty >=1.2.3 && <2,\n    tasty-hunit >=0.10 && <0.11,\n    tasty-quickcheck >=0.10 && <0.11\n";
    }