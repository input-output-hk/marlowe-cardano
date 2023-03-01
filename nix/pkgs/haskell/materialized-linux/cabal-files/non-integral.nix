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
      specVersion = "2.2";
      identifier = { name = "non-integral"; version = "0.1.1.2"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Implementation decision for non-integer calculations";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "non-integral-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/non-integral-0.1.1.2.tar.gz";
      sha256 = "fded226cd30e74287913a398e54850ef9a13211a4a4d2064a5ef4835aaf6085b";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                non-integral\nversion:             0.1.1.2\nlicense:             Apache-2.0\nauthor:              IOHK Formal Methods Team\nmaintainer:          formal.methods@iohk.io\ndescription:         Implementation decision for non-integer calculations\nbuild-type:          Simple\n\nextra-source-files:\n  README.md\n  ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/input-output-hk/cardano-ledger.git\n  subdir:   libs/non-integral\n\ncommon base\n  build-depends:      base >= 4.12 && < 4.17\n\ncommon project-config\n  default-language:   Haskell2010\n\n  ghc-options:        -Wall\n                      -Wcompat\n                      -Wincomplete-record-updates\n                      -Wincomplete-uni-patterns\n                      -Wredundant-constraints\n                      -Wunused-packages\n\nlibrary\n  import:             base, project-config\n  exposed-modules:    Cardano.Ledger.NonIntegral\n  hs-source-dirs:     src\n\ntest-suite non-integral-test\n  import:             base, project-config\n  type:               exitcode-stdio-1.0\n  main-is:            Tests.hs\n  other-modules:      Tests.Cardano.Ledger.NonIntegral\n  hs-source-dirs:     test\n  ghc-options:        -O2\n                      -threaded\n                      -rtsopts\n                      -with-rtsopts=-N\n\n  build-depends:      non-integral\n                    , QuickCheck\n";
    }