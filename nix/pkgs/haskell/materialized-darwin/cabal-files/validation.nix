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
      identifier = { name = "validation"; version = "1.1.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2010-2013 Tony Morris, Nick Partridge\nCopyright (C) 2014,2015 NICTA Limited\nCopyright (c) 2016-2019, Commonwealth Scientific and Industrial Research Organisation (CSIRO) ABN 41 687 119 230.";
      maintainer = "Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>, Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>";
      author = "Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>";
      homepage = "https://github.com/qfpl/validation";
      url = "";
      synopsis = "A data-type like Either but with an accumulating Applicative";
      description = "<<https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png>>\n\nA data-type like Either but with differing properties and type-class\ninstances.\n\nLibrary support is provided for this different representation, include\n`lens`-related functions for converting between each and abstracting over their\nsimilarities.\n\n* `Validation`\n\nThe `Validation` data type is isomorphic to `Either`, but has an instance\nof `Applicative` that accumulates on the error side. That is to say, if two\n(or more) errors are encountered, they are appended using a `Semigroup`\noperation.\n\nAs a consequence of this `Applicative` instance, there is no corresponding\n`Bind` or `Monad` instance. `Validation` is an example of, \"An applicative\nfunctor that is not a monad.\"";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."assoc" or (errorHandler.buildDepError "assoc"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          ];
        buildable = true;
        };
      tests = {
        "hedgehog" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            ];
          buildable = true;
          };
        "hunit" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/validation-1.1.2.tar.gz";
      sha256 = "1dcd52a577d06fbeb66a5acdeb125a438fc9aa4f07ef854cd93f4123a7f81096";
      });
    }) // {
    package-description-override = "name:               validation\nversion:            1.1.2\nlicense:            BSD3\nlicense-file:       LICENCE\nauthor:             Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>\nmaintainer:         Tony Morris <ʇǝu˙sıɹɹoɯʇ@ןןǝʞsɐɥ> <dibblego>, Nick Partridge <nkpart>, Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>\ncopyright:          Copyright (C) 2010-2013 Tony Morris, Nick Partridge\n                    Copyright (C) 2014,2015 NICTA Limited\n                    Copyright (c) 2016-2019, Commonwealth Scientific and Industrial Research Organisation (CSIRO) ABN 41 687 119 230.\nsynopsis:           A data-type like Either but with an accumulating Applicative\ncategory:           Data\ndescription:\n  <<https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png>>\n  .\n  A data-type like Either but with differing properties and type-class\n  instances.\n  .\n  Library support is provided for this different representation, include\n  `lens`-related functions for converting between each and abstracting over their\n  similarities.\n  .\n  * `Validation`\n  .\n  The `Validation` data type is isomorphic to `Either`, but has an instance\n  of `Applicative` that accumulates on the error side. That is to say, if two\n  (or more) errors are encountered, they are appended using a `Semigroup`\n  operation.\n  .\n  As a consequence of this `Applicative` instance, there is no corresponding\n  `Bind` or `Monad` instance. `Validation` is an example of, \"An applicative\n  functor that is not a monad.\"\n\nhomepage:           https://github.com/qfpl/validation\nbug-reports:        https://github.com/qfpl/validation/issues\ncabal-version:      >= 1.10\nbuild-type:         Simple\nextra-source-files: changelog\ntested-with:        GHC==9.0.1, GHC==8.10.4, GHC==8.8.4, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3\n\nsource-repository   head\n  type:             git\n  location:         git@github.com:qfpl/validation.git\n\nlibrary\n  default-language:\n                    Haskell2010\n\n  build-depends:\n                      base          >= 4.8  && < 5\n                    , assoc         >= 1    && < 1.1\n                    , deepseq       >= 1.4  && < 1.5\n                    , semigroups    >= 0.16 && < 1\n                    , semigroupoids >= 5    && < 6\n                    , bifunctors    >= 5.5  && < 6\n                    , lens          >= 4    && < 6\n\n  ghc-options:\n                    -Wall\n\n  hs-source-dirs:\n                    src\n\n  exposed-modules:\n                    Data.Validation\n\ntest-suite hedgehog\n  type:\n                    exitcode-stdio-1.0\n\n  main-is:\n                    hedgehog_tests.hs\n\n  default-language:\n                    Haskell2010\n\n  build-depends:\n                      base       >= 4.8  && < 5\n                    , hedgehog   >= 0.5  && < 1.1\n                    , semigroups >= 0.16 && < 1\n                    , validation\n\n  ghc-options:\n                    -Wall\n                    -threaded\n\n  hs-source-dirs:\n                    test\n\ntest-suite hunit\n  type:\n                    exitcode-stdio-1.0\n\n  main-is:\n                    hunit_tests.hs\n\n  default-language:\n                    Haskell2010\n\n  build-depends:\n                      base       >= 4.8  && < 5\n                    , HUnit      >= 1.5  && < 1.7\n                    , lens       >= 4    && < 5\n                    , semigroups >= 0.16 && < 1\n                    , validation\n\n  ghc-options:\n                    -Wall\n                    -threaded\n\n  hs-source-dirs:\n                    test\n";
    }