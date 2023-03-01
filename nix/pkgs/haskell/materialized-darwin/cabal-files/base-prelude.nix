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
      identifier = { name = "base-prelude"; version = "1.6.1"; };
      license = "MIT";
      copyright = "(c) 2014, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/base-prelude";
      url = "";
      synopsis = "Featureful preludes formed solely from the \"base\" package";
      description = "A library which aims to reexport all the non-conflicting and\nmost general definitions from the \\\"base\\\" package.\nThis includes APIs for applicatives, arrows, monoids, foldables, traversables,\nexceptions, generics, ST, MVars and STM.\n\nThis package will never have any dependencies other than \\\"base\\\".\n\nBesides a rich prelude it provides limited ones like \"BasePrelude.DataTypes\",\nwhich only exports the data-types defined across the \\\"base\\\" package,\nand \"BasePrelude.Operators\", which only exports the common operators.\n\n/Versioning policy/\n\nThe versioning policy of this package deviates from PVP in the sense\nthat its exports in part are transitively determined by the version of \\\"base\\\".\nTherefore it's recommended for the users of \\\"base-prelude\\\" to specify\nthe bounds of \\\"base\\\" as well.";
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
      url = "http://hackage.haskell.org/package/base-prelude-1.6.1.tar.gz";
      sha256 = "729034d1d83d8eacdf84399f97c4f9b99bb710555ddc1e0b9f196f5dd0347d65";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: base-prelude\nversion: 1.6.1\nsynopsis: Featureful preludes formed solely from the \"base\" package\ndescription:\n  A library which aims to reexport all the non-conflicting and\n  most general definitions from the \\\"base\\\" package.\n  This includes APIs for applicatives, arrows, monoids, foldables, traversables,\n  exceptions, generics, ST, MVars and STM.\n\n  This package will never have any dependencies other than \\\"base\\\".\n\n  Besides a rich prelude it provides limited ones like \"BasePrelude.DataTypes\",\n  which only exports the data-types defined across the \\\"base\\\" package,\n  and \"BasePrelude.Operators\", which only exports the common operators.\n\n  /Versioning policy/\n\n  The versioning policy of this package deviates from PVP in the sense\n  that its exports in part are transitively determined by the version of \\\"base\\\".\n  Therefore it's recommended for the users of \\\"base-prelude\\\" to specify\n  the bounds of \\\"base\\\" as well.\ncategory: Prelude\nhomepage: https://github.com/nikita-volkov/base-prelude\nbug-reports: https://github.com/nikita-volkov/base-prelude/issues\nauthor: Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer: Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright: (c) 2014, Nikita Volkov\nlicense: MIT\nlicense-file: LICENSE\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: git://github.com/nikita-volkov/base-prelude.git\n\nlibrary\n  hs-source-dirs: library\n  default-language: Haskell2010\n  exposed-modules:\n    BasePrelude\n    BasePrelude.DataTypes\n    BasePrelude.Operators\n  build-depends:\n    base >=4.12 && <5\n";
    }