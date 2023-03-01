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
      identifier = { name = "haskell-src-exts"; version = "1.23.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Dan Burton <danburton.email@gmail.com>";
      author = "Niklas Broberg";
      homepage = "https://github.com/haskell-suite/haskell-src-exts";
      url = "";
      synopsis = "Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer";
      description = "Haskell-Source with Extensions (HSE, haskell-src-exts)\nis a standalone parser for Haskell. In addition to\nstandard Haskell, all extensions implemented in GHC are supported.\n\nApart from these standard extensions,\nit also handles regular patterns as per the HaRP extension\nas well as HSX-style embedded XML syntax.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy:happy")))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
            (hsPkgs."smallcheck" or (errorHandler.buildDepError "smallcheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/haskell-src-exts-1.23.1.tar.gz";
      sha256 = "67853047169fff7d3e5d87acef214ee185a6ab8c6a104ed9c59e389574cf6c05";
      });
    }) // {
    package-description-override = "Name:                   haskell-src-exts\nVersion:                1.23.1\nLicense:                BSD3\nLicense-File:           LICENSE\nBuild-Type:             Simple\nAuthor:                 Niklas Broberg\nMaintainer:             Dan Burton <danburton.email@gmail.com>\nCategory:               Language\nSynopsis:               Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer\nDescription:            Haskell-Source with Extensions (HSE, haskell-src-exts)\n                        is a standalone parser for Haskell. In addition to\n                        standard Haskell, all extensions implemented in GHC are supported.\n                        .\n                        Apart from these standard extensions,\n                        it also handles regular patterns as per the HaRP extension\n                        as well as HSX-style embedded XML syntax.\nHomepage:               https://github.com/haskell-suite/haskell-src-exts\nBug-Reports:            https://github.com/haskell-suite/haskell-src-exts/issues\nStability:              Stable\nCabal-Version:          >= 1.10\nTested-With:\n                          GHC == 7.8.2\n                        , GHC == 7.10.3\n                        , GHC == 8.0.2\n                        , GHC == 8.2.2\n                        , GHC == 8.4.4\n                        , GHC == 8.6.5\n                        , GHC == 8.8.3\n                        , GHC == 8.10.1\n\nExtra-Source-Files:\n                        README.md\n                        CHANGELOG\n                        RELEASENOTES-1.17.0\n                        tests/examples/*.hs\n                        tests/examples/*.lhs\n                        tests/examples/*.hs.parser.golden\n                        tests/examples/*.lhs.parser.golden\n                        tests/examples/*.hs.exactprinter.golden\n                        tests/examples/*.lhs.exactprinter.golden\n                        tests/examples/*.hs.prettyprinter.golden\n                        tests/examples/*.lhs.prettyprinter.golden\n                        tests/examples/*.hs.prettyparser.golden\n                        tests/examples/*.lhs.prettyparser.golden\n                        tests/Runner.hs\n                        tests/Extensions.hs\n\nLibrary\n  Default-language:     Haskell98\n  Build-Tools:          happy >= 1.19\n  Build-Depends:        array >= 0.1, pretty >= 1.0,\n                        base >= 4.5 && < 5,\n                        -- this is needed to access GHC.Generics on GHC 7.4\n                        ghc-prim\n  -- this is needed to access Data.Semigroup and Control.Monad.Fail on GHCs\n  -- before 8.0\n  if !impl(ghc >= 8.0)\n    Build-Depends:\n                        semigroups >= 0.18.3,\n                        fail == 4.9.*\n\n  Exposed-modules:      Language.Haskell.Exts,\n                        Language.Haskell.Exts.Lexer,\n                        Language.Haskell.Exts.Pretty,\n                        Language.Haskell.Exts.Extension,\n                        Language.Haskell.Exts.Build,\n                        Language.Haskell.Exts.SrcLoc,\n\n                        Language.Haskell.Exts.Syntax,\n                        Language.Haskell.Exts.Fixity,\n                        Language.Haskell.Exts.ExactPrint,\n                        Language.Haskell.Exts.Parser,\n                        Language.Haskell.Exts.Comments\n\n  Other-modules:        Language.Haskell.Exts.ExtScheme,\n                        Language.Haskell.Exts.ParseMonad,\n                        Language.Haskell.Exts.ParseSyntax,\n                        Language.Haskell.Exts.InternalLexer,\n                        Language.Haskell.Exts.ParseUtils,\n                        Language.Haskell.Exts.InternalParser\n                        Language.Preprocessor.Unlit\n  Hs-source-dirs:       src\n  Ghc-options:          -Wall\n\nSource-Repository head\n        Type:           git\n        Location:       https://github.com/haskell-suite/haskell-src-exts.git\n\nTest-Suite test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      tests\n  main-is:             Runner.hs\n  other-modules:       Extensions\n  GHC-Options:         -threaded -Wall\n  Default-language:    Haskell2010\n  Build-depends:       base,\n                       mtl,\n                       containers,\n                       haskell-src-exts,\n                       smallcheck >= 1.0,\n                       tasty >= 0.3,\n                       tasty-smallcheck,\n                       tasty-golden >= 2.2.2,\n                       filepath,\n                       directory,\n                       pretty-show >= 1.6.16\n";
    }