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
      identifier = { name = "fmt"; version = "0.6.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "yom@artyom.me";
      author = "Artyom Kazak <yom@artyom.me>,\nDmitry Kovanikov <kovanikov@gmail.com>";
      homepage = "http://github.com/aelve/fmt";
      url = "";
      synopsis = "A new formatting library";
      description = "A new formatting library that tries to be simple to understand while still\nbeing powerful and providing more convenience features than other libraries\n(like functions for pretty-printing maps and lists, or a function for\nprinting arbitrary datatypes using generics).\n\nA comparison with other libraries:\n\n* @printf@ (from @Text.Printf@) takes a formatting string and uses some\ntype tricks to accept the rest of the arguments polyvariadically. It's\nvery concise, but there are some drawbacks – it can't produce @Text@\n(you'd have to @T.pack@ it every time) and it doesn't warn you at\ncompile-time if you pass wrong arguments or not enough of them.\n\n* <https://hackage.haskell.org/package/text-format text-format> takes a\nformatting string with curly braces denoting places where arguments\nwould be substituted (the arguments themselves are provided via a\ntuple). If you want to apply formatting to some of the arguments, you\nhave to use one of the provided formatters. Like @printf@, it can fail at\nruntime, but at least the formatters are first-class (and you can add new\nones).\n\n* <https://hackage.haskell.org/package/formatting formatting> takes a\nformatting template consisting of pieces of strings interleaved with\nformatters; this ensures that arguments always match their placeholders.\n@formatting@ provides lots of formatters and generally seems to be the\nmost popular formatting library here. Unfortunately, at least in my\nexperience writing new formatters can be awkward and people sometimes\nhave troubles understanding how @formatting@ works.\n\n* <https://hackage.haskell.org/package/fmt fmt> (i.e. this library)\nprovides formatters that are ordinary functions, and a bunch of operators\nfor concatenating formatted strings; those operators also do automatic\nconversion. There are some convenience formatters which aren't present in\n@formatting@ (like ones for formatting maps, lists, converting to base64,\netc). Some find the operator syntax annoying, while others like it.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-locale-compat" or (errorHandler.buildDepError "time-locale-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover.components.exes.doctest-discover or (pkgs.buildPackages.doctest-discover or (errorHandler.buildToolDepError "doctest-discover:doctest-discover")))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "8.0"
            then false
            else true;
          };
        };
      benchmarks = {
        "benches" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."interpolate" or (errorHandler.buildDepError "interpolate"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/fmt-0.6.1.2.tar.gz";
      sha256 = "78ed7bddb25c0bc4355ca2be2be3c4d8af796bce7e76e20f04b6aebbcbab8ab9";
      });
    }) // {
    package-description-override = "name:                fmt\nversion:             0.6.1.2\nsynopsis:            A new formatting library\ndescription:\n  A new formatting library that tries to be simple to understand while still\n  being powerful and providing more convenience features than other libraries\n  (like functions for pretty-printing maps and lists, or a function for\n  printing arbitrary datatypes using generics).\n  .\n  A comparison with other libraries:\n  .\n  * @printf@ (from @Text.Printf@) takes a formatting string and uses some\n    type tricks to accept the rest of the arguments polyvariadically. It's\n    very concise, but there are some drawbacks – it can't produce @Text@\n    (you'd have to @T.pack@ it every time) and it doesn't warn you at\n    compile-time if you pass wrong arguments or not enough of them.\n  .\n  * <https://hackage.haskell.org/package/text-format text-format> takes a\n    formatting string with curly braces denoting places where arguments\n    would be substituted (the arguments themselves are provided via a\n    tuple). If you want to apply formatting to some of the arguments, you\n    have to use one of the provided formatters. Like @printf@, it can fail at\n    runtime, but at least the formatters are first-class (and you can add new\n    ones).\n  .\n  * <https://hackage.haskell.org/package/formatting formatting> takes a\n    formatting template consisting of pieces of strings interleaved with\n    formatters; this ensures that arguments always match their placeholders.\n    @formatting@ provides lots of formatters and generally seems to be the\n    most popular formatting library here. Unfortunately, at least in my\n    experience writing new formatters can be awkward and people sometimes\n    have troubles understanding how @formatting@ works.\n  .\n  * <https://hackage.haskell.org/package/fmt fmt> (i.e. this library)\n    provides formatters that are ordinary functions, and a bunch of operators\n    for concatenating formatted strings; those operators also do automatic\n    conversion. There are some convenience formatters which aren't present in\n    @formatting@ (like ones for formatting maps, lists, converting to base64,\n    etc). Some find the operator syntax annoying, while others like it.\nhomepage:            http://github.com/aelve/fmt\nbug-reports:         http://github.com/aelve/fmt/issues\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Artyom Kazak <yom@artyom.me>,\n                     Dmitry Kovanikov <kovanikov@gmail.com>\nmaintainer:          yom@artyom.me\n-- copyright:\ncategory:            Text\ntested-with:\n  GHC == 7.10.3\n  GHC == 8.0.2\n  GHC == 8.2.2\n  GHC == 8.4.4\n  GHC == 8.6.4\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n                     tests/doctest-config.json\ncabal-version:       >=1.10\n\nsource-repository head\n  type:                git\n  location:            git://github.com/aelve/fmt.git\n\nlibrary\n  exposed-modules:     Fmt\n                       Fmt.Time\n                       Fmt.Internal\n                       Fmt.Internal.Core\n                       Fmt.Internal.Formatters\n                       Fmt.Internal.Template\n                       Fmt.Internal.Tuple\n                       Fmt.Internal.Numeric\n                       Fmt.Internal.Generic\n\n  build-depends:       base >=4.6 && <5,\n                       base64-bytestring,\n                       bytestring,\n                       call-stack,\n                       containers,\n                       formatting >= 6.3.4,\n                       microlens >= 0.3,\n                       text,\n                       time,\n                       time-locale-compat\n\n  ghc-options:         -Wall -fno-warn-unused-do-bind\n  if impl(ghc > 8.0)\n    ghc-options:       -Wno-redundant-constraints\n  hs-source-dirs:      lib\n  default-language:    Haskell2010\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups == 0.18.*\n\ntest-suite tests\n  main-is:             Main.hs\n  type:                exitcode-stdio-1.0\n  build-depends:       base >=4.6 && <5\n                     , bytestring\n                     , call-stack\n                     , containers\n                     , fmt\n                     , hspec >= 2.2 && < 2.8\n                     , neat-interpolation\n                     , text\n                     , vector\n\n  ghc-options:         -Wall -fno-warn-unused-do-bind\n\n  hs-source-dirs:      tests\n  default-language:    Haskell2010\n\ntest-suite doctests\n  default-language:    Haskell2010\n  type:                exitcode-stdio-1.0\n  ghc-options:         -threaded\n  hs-source-dirs:      tests\n  main-is:             doctests.hs\n  build-depends:       base >=4.6 && <5\n                     , doctest\n                     , QuickCheck\n\n  build-tool-depends:  doctest-discover:doctest-discover\n  if impl(ghc < 8.0)\n    buildable: False\n\nbenchmark benches\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      bench\n  main-is:             Main.hs\n  build-depends:       base >=4.6 && <5\n                     , bytestring\n                     , containers\n                     , criterion\n                     , deepseq\n                     , fmt\n                     , formatting\n                     , interpolate\n                     , text\n                     , vector\n  ghc-options:         -Wall\n  default-language:    Haskell2010\n";
    }