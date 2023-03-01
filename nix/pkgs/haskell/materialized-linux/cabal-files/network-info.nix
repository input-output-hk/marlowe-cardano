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
      identifier = { name = "network-info"; version = "0.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Jacob Stanley";
      homepage = "http://github.com/jacobstanley/network-info";
      url = "";
      synopsis = "Access the local computer's basic network configuration";
      description = "This library provides simple read-only access to the\nlocal computer's networking configuration. It is\ncurrently capable of getting a list of all the network\ninterfaces and their respective IPv4, IPv6 and MAC\naddresses.\n\nnetwork-info has been tested and is known to work on\nUbuntu, FreeBSD, NetBSD, Mac OS and Windows.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."iphlpapi" or (errorHandler.sysDepError "iphlpapi")) ++ (pkgs.lib).optionals (system.isSolaris) [
          (pkgs."socket" or (errorHandler.sysDepError "socket"))
          (pkgs."nsl" or (errorHandler.sysDepError "nsl"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-info-0.2.1.tar.gz";
      sha256 = "9b2d88312fc8280351d5003200cc07decbb865f85d3aa7b1094b238bd6a8b404";
      });
    }) // {
    package-description-override = "name:           network-info\nversion:        0.2.1\nsynopsis:       Access the local computer's basic network configuration\n\ndescription:    This library provides simple read-only access to the\n                local computer's networking configuration. It is\n                currently capable of getting a list of all the network\n                interfaces and their respective IPv4, IPv6 and MAC\n                addresses.\n                .\n                network-info has been tested and is known to work on\n                Ubuntu, FreeBSD, NetBSD, Mac OS and Windows.\n\nhomepage:       http://github.com/jacobstanley/network-info\nlicense:        BSD3\nlicense-file:   LICENSE\nauthor:         Jacob Stanley\nmaintainer:     Jacob Stanley <jacob@stanley.io>\ncategory:       Network\nbuild-type:     Simple\ncabal-version:  >= 1.10\n\ntested-with:\n  GHC == 8.8.4,\n  GHC == 8.10.7,\n  GHC == 9.0.1,\n  GHC == 9.2.1\n\nextra-source-files:\n  cbits/common.h,\n  cbits/common.inc,\n  cbits/network.h,\n  test/src/Main.hs,\n  test/network-info-test.cabal,\n  test/run-tests.bat,\n  test/run-tests.sh,\n  README.mkd,\n  CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: git://github.com/jacobstanley/network-info.git\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs: src\n  include-dirs: cbits\n  cc-options: -Wall -std=c99\n\n  exposed-modules: Network.Info\n\n  build-depends:\n    base == 4.*\n\n  if os(windows)\n    c-sources: cbits/network-windows.c\n    extra-libraries: iphlpapi\n  else\n    c-sources: cbits/network-unix.c\n  if os(solaris)\n    extra-libraries: socket, nsl\n";
    }