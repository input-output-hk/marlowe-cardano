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
      identifier = { name = "one-line-aeson-text"; version = "0.1.0.4"; };
      license = "Apache-2.0";
      copyright = "2021 Mission Valley Software LLC";
      maintainer = "Chris Martin, Julie Moronuki";
      author = "Chris Martin";
      homepage = "https://github.com/typeclasses/one-line-aeson-text";
      url = "";
      synopsis = "Pretty-printing short Aeson values as text.";
      description = "A very simply pretty-printer for Aeson values that\nproduces single-line output, which you may want for\ndisplaying short JSON data in log files.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "test-one-line-aeson-text" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."one-line-aeson-text" or (errorHandler.buildDepError "one-line-aeson-text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/one-line-aeson-text-0.1.0.4.tar.gz";
      sha256 = "21bf02f5fb3b6c687c02385a64c7b928b912414a9cd39dc0f9dd6847d8b50d42";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\n\nname: one-line-aeson-text\nversion: 0.1.0.4\ncategory: Text, JSON\n\nsynopsis: Pretty-printing short Aeson values as text.\n\ndescription: A very simply pretty-printer for Aeson values that\n             produces single-line output, which you may want for\n             displaying short JSON data in log files.\n\nhomepage:    https://github.com/typeclasses/one-line-aeson-text\nbug-reports: https://github.com/typeclasses/one-line-aeson-text/issues\n\nauthor:     Chris Martin\nmaintainer: Chris Martin, Julie Moronuki\n\ncopyright: 2021 Mission Valley Software LLC\nlicense: Apache-2.0\nlicense-file: license.txt\n\nbuild-type: Simple\n\nextra-source-files:\n    README.md\n\nsource-repository head\n    type: git\n    location: https://github.com/typeclasses/one-line-aeson-text\n\ncommon base\n    default-language: Haskell2010\n    build-depends:\n        aeson ^>= 2.0\n      , base ^>= 4.13 || ^>= 4.14 || ^>= 4.15 || ^>= 4.16\n      , text ^>= 1.2.4\n\nlibrary\n    import: base\n    hs-source-dirs: src\n    exposed-modules:\n        Data.Aeson.OneLine\n\ntest-suite test-one-line-aeson-text\n    import: base\n    hs-source-dirs: test\n    type: exitcode-stdio-1.0\n    main-is: Main.hs\n    build-depends: one-line-aeson-text\n";
    }