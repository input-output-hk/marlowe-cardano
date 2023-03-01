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
    flags = { developer = false; native = true; integer-simple = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "blaze-textual"; version = "0.2.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2011 MailRank, Inc.";
      maintainer = "Bryan O'Sullivan <bos@serpentine.com>\n, Andrey Prokopenko <persiantiger@yandex.ru>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "http://github.com/bos/blaze-textual";
      url = "";
      synopsis = "Fast rendering of common datatypes";
      description = "A library for efficiently rendering Haskell datatypes to\nbytestrings.\n\n/Note/: if you use GHCi or Template Haskell, please see the\n@README@ file for important details about building this package,\nand other packages that depend on it:\n<https://github.com/bos/blaze-textual#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ] ++ (pkgs.lib).optional (!flags.native) (hsPkgs."double-conversion" or (errorHandler.buildDepError "double-conversion"))) ++ (if flags.integer-simple
          then [
            (hsPkgs."integer-simple" or (errorHandler.buildDepError "integer-simple"))
            ]
          else [
            (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
            ]);
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
            (hsPkgs."blaze-textual" or (errorHandler.buildDepError "blaze-textual"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."double-conversion" or (errorHandler.buildDepError "double-conversion"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/blaze-textual-0.2.2.1.tar.gz";
      sha256 = "7a9199740189f435b762d98e65f5d7c0c7a1467e36c11210a8d65e76a5e5567e";
      });
    }) // {
    package-description-override = "name:            blaze-textual\nversion:         0.2.2.1\nlicense:         BSD3\nlicense-file:    LICENSE\ncategory:        Text\ncopyright:       Copyright 2011 MailRank, Inc.\nauthor:          Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:      Bryan O'Sullivan <bos@serpentine.com>\n               , Andrey Prokopenko <persiantiger@yandex.ru>\nstability:       experimental\nsynopsis:        Fast rendering of common datatypes\ncabal-version:   1.12\nhomepage:        http://github.com/bos/blaze-textual\nbug-reports:     http://github.com/bos/blaze-textual/issues\nbuild-type:      Simple\ndescription:\n    A library for efficiently rendering Haskell datatypes to\n    bytestrings.\n    .\n    /Note/: if you use GHCi or Template Haskell, please see the\n    @README@ file for important details about building this package,\n    and other packages that depend on it:\n    <https://github.com/bos/blaze-textual#readme>\n\nextra-source-files:\n    README.markdown\n    changelog.md\n    tests/*.hs\n\nflag developer\n  description: operate in developer mode\n  default: False\n  manual: True\n\nflag native\n  description: use slow native code for double conversion\n  default: True\n  manual: True\n\nflag integer-simple\n  description: use integer-simple instead of integer-gmp\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:\n    Blaze.Text\n    Blaze.Text.Double\n    Blaze.Text.Int\n\n  if flag(native)\n    other-modules: Blaze.Text.Double.Native\n\n  build-depends:\n    base == 4.*,\n    blaze-builder >= 0.2.1.4,\n    bytestring,\n    ghc-prim,\n    old-locale,\n    text >= 0.11.0.2,\n    time,\n    vector\n\n  if !flag(native)\n    build-depends:\n      double-conversion >= 0.2.0.1\n\n  if flag(developer)\n    ghc-options: -Werror\n    ghc-prof-options: -auto-all\n\n  if flag(native)\n    cpp-options: -DNATIVE\n\n  ghc-options:      -Wall\n\n  if flag(integer-simple)\n    cpp-options: -DINTEGER_SIMPLE\n    build-depends: integer-simple\n  else\n    cpp-options: -DINTEGER_GMP\n    build-depends: integer-gmp >= 0.2\n\n  default-language: Haskell2010\n\ntest-suite tests\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is:        QC.hs\n  ghc-options:    -Wall -threaded -rtsopts\n  build-depends:\n    QuickCheck >= 2.4.0.1,\n    base,\n    blaze-builder,\n    blaze-textual,\n    bytestring,\n    double-conversion,\n    test-framework >= 0.3.3,\n    test-framework-quickcheck2 >= 0.2.9\n\n  default-language: Haskell2010\n\nsource-repository head\n  type:     git\n  location: http://github.com/bos/blaze-textual\n\nsource-repository head\n  type:     mercurial\n  location: http://bitbucket.org/bos/blaze-textual\n";
    }