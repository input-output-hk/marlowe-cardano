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
    flags = { system-libbf = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "libBF"; version = "0.6.5.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "iavor.diatchki@gmail.com";
      author = "Iavor Diatchki";
      homepage = "";
      url = "";
      synopsis = "A binding to the libBF library.";
      description = "LibBF is a C library for working with arbitray precision\nIEEE 754 floating point numbers.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ];
        libs = (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "9.4")) ((pkgs.lib).optionals (system.isWindows) (if system.isX86_64
          then [
            (pkgs."gcc_s_seh-1" or (errorHandler.sysDepError "gcc_s_seh-1"))
            ]
          else [
            (pkgs."gcc_s_dw2-1" or (errorHandler.sysDepError "gcc_s_dw2-1"))
            ])) ++ (pkgs.lib).optional (flags.system-libbf) (pkgs."bf" or (errorHandler.sysDepError "bf"));
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      exes = {
        "bf-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."libBF" or (errorHandler.buildDepError "libBF"))
            ];
          buildable = true;
          };
        };
      tests = {
        "libBF-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."libBF" or (errorHandler.buildDepError "libBF"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/libBF-0.6.5.1.tar.gz";
      sha256 = "36e6bd8bd2b407540bd277d29172530b435e782b5d5101edcacd34fa7fbbb8d0";
      });
    }) // {
    package-description-override = "cabal-version:       2.2\n\nname:                libBF\nversion:             0.6.5.1\nsynopsis:            A binding to the libBF library.\ndescription:         LibBF is a C library for working with arbitray precision\n                     IEEE 754 floating point numbers.\nbug-reports:         https://github.com/GaloisInc/libBF-hs/issues\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Iavor Diatchki\nmaintainer:          iavor.diatchki@gmail.com\n-- copyright:\ncategory:            Data\nextra-source-files:  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/GaloisInc/libBF-hs.git\n\nflag system-libbf\n  default:     False\n  description: Use system libbf instead\n\nlibrary\n  exposed-modules:\n    LibBF,\n    LibBF.Opts,\n    LibBF.Mutable\n\n  build-depends:\n    base >=4.12.0.0 && < 5,\n    deepseq,\n    hashable >= 1.3\n\n  build-tool-depends:\n    hsc2hs:hsc2hs\n\n  hs-source-dirs:      src\n\n  -- Prior to GHC 9.4, Windows GHC bindists bundled a GCC-based C toolchain,\n  -- which requires linking against the GCC library to handle some of the\n  -- compiler intrinsics that appear in the optimized code. Moreover,\n  -- dynamically linking against the GCC library proves fragile, so we force\n  -- static linking by providing the full name of the static GCC library\n  -- archive.\n  --\n  -- On GHC 9.4+, Windows GHC bindists ship a Clang-based C toolchain. Clang\n  -- optimizations don't appear to require linking against anything in\n  -- particular, so it just works out of the box.\n  if !impl(ghc >= 9.4)\n    if os(windows)\n      if arch(x86_64)\n        extra-libraries: gcc_s_seh-1\n      else\n        extra-libraries: gcc_s_dw2-1\n\n  if flag(system-libbf)\n    extra-libraries: bf\n    c-sources:\n      cbits/libbf-hs.c\n  else\n    include-dirs:\n      libbf-2020-01-19\n\n    includes:\n      libbf-2020-01-19/libbf.h\n\n    install-includes:\n      libbf-2020-01-19/libbf.h\n      libbf-2020-01-19/cutils.h\n\n    c-sources:\n      libbf-2020-01-19/cutils.c\n      libbf-2020-01-19/libbf.c\n      cbits/libbf-hs.c\n\n  ghc-options:         -Wall\n  default-language:    Haskell2010\n\nexecutable bf-test\n  main-is:            RunUnitTests.hs\n  hs-source-dirs:     tests\n  build-depends:      base, libBF\n  default-language:   Haskell2010\n\n\ntest-suite libBF-tests\n  type:               exitcode-stdio-1.0\n  hs-source-dirs:     tests\n  main-is:            RunUnitTests.hs\n  default-language:   Haskell2010\n  build-depends:      base, libBF\n\n";
    }