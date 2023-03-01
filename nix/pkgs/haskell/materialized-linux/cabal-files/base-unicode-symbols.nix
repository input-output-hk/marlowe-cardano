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
    flags = { old-base = false; base-4-8 = true; };
    package = {
      specVersion = "1.6";
      identifier = { name = "base-unicode-symbols"; version = "0.2.4.2"; };
      license = "BSD-3-Clause";
      copyright = "2009–2019 Roel van Dijk <roel@lambdacube.nl>";
      maintainer = "Roel van Dijk <roel@lambdacube.nl>";
      author = "Roel van Dijk <roel@lambdacube.nl>";
      homepage = "http://haskell.org/haskellwiki/Unicode-symbols";
      url = "";
      synopsis = "Unicode alternatives for common functions and operators";
      description = "This package defines new symbols for a number of functions,\noperators and types in the base package.\n\nAll symbols are documented with their actual definition and\ninformation regarding their Unicode code point. They should be\ncompletely interchangeable with their definitions.\n\nFor further Unicode goodness you can enable the @UnicodeSyntax@\nlanguage extension [1]. This extension enables Unicode characters\nto be used to stand for certain ASCII character sequences,\ni.e. &#x2192; instead of @->@, &#x2200; instead of @forall@ and many\nothers.\n\nOriginal idea by P&#xE9;ter Divi&#xE1;nszky.\n\n\\[1] <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unicode-syntax>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/base-unicode-symbols-0.2.4.2.tar.gz";
      sha256 = "4364d6c403616e9ec0c240c4cb450c66af43ea8483d73c315e96f4ba3cb97062";
      });
    }) // {
    package-description-override = "name:          base-unicode-symbols\nversion:       0.2.4.2\ncabal-version: >=1.6\nbuild-type:    Simple\nstability:     provisional\nauthor:        Roel van Dijk <roel@lambdacube.nl>\nmaintainer:    Roel van Dijk <roel@lambdacube.nl>\ncopyright:     2009–2019 Roel van Dijk <roel@lambdacube.nl>\nlicense:       BSD3\nlicense-file:  LICENSE\ncategory:\nhomepage:      http://haskell.org/haskellwiki/Unicode-symbols\nbug-reports:   https://github.com/roelvandijk/base-unicode-symbols/issues\nsynopsis:      Unicode alternatives for common functions and operators\ndescription:\n  This package defines new symbols for a number of functions,\n  operators and types in the base package.\n  .\n  All symbols are documented with their actual definition and\n  information regarding their Unicode code point. They should be\n  completely interchangeable with their definitions.\n  .\n  For further Unicode goodness you can enable the @UnicodeSyntax@\n  language extension [1]. This extension enables Unicode characters\n  to be used to stand for certain ASCII character sequences,\n  i.e. &#x2192; instead of @->@, &#x2200; instead of @forall@ and many\n  others.\n  .\n  Original idea by P&#xE9;ter Divi&#xE1;nszky.\n  .\n  \\[1] <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unicode-syntax>\n\nextra-source-files: LICENSE, README.markdown\n\nsource-repository head\n  type:     git\n  location: git://github.com/roelvandijk/base-unicode-symbols.git\n\nflag old-base\n  description: Support for base < 3.0.3.1\n  default: False\n\nflag base-4-8\n  description: Support features of base >= 4.8.0.0\n  default: True\n\nlibrary\n  hs-source-dirs: src\n  ghc-options: -Wall\n\n  if flag(old-base)\n    build-depends: base >= 3.0 && < 3.0.3.1\n  else\n    exposed-modules: Control.Category.Unicode\n    if flag(base-4-8)\n      build-depends: base >= 4.8.0.0 && < 5\n      exposed-modules: Numeric.Natural.Unicode\n    else\n      build-depends: base >= 3.0.3.1 && < 5\n  exposed-modules: Control.Applicative.Unicode\n                 , Control.Arrow.Unicode\n                 , Control.Monad.Unicode\n                 , Data.Bool.Unicode\n                 , Data.Eq.Unicode\n                 , Data.Foldable.Unicode\n                 , Data.Function.Unicode\n                 , Data.List.Unicode\n                 , Data.Monoid.Unicode\n                 , Data.Ord.Unicode\n                 , Data.String.Unicode\n                 , Prelude.Unicode\n";
    }