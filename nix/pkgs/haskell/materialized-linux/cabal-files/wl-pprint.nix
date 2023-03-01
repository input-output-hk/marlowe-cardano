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
      specVersion = "1.6";
      identifier = { name = "wl-pprint"; version = "1.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Noam Lewis <jones.noamle@gmail.com>";
      author = "Daan Leijen";
      homepage = "";
      url = "";
      synopsis = "The Wadler/Leijen Pretty Printer";
      description = "This is a pretty printing library based on Wadler's paper \"A Prettier\nPrinter\".  See the haddocks for full info.  This version allows the\nlibrary user to declare overlapping instances of the 'Pretty' class.";
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
      url = "http://hackage.haskell.org/package/wl-pprint-1.2.1.tar.gz";
      sha256 = "0c7c8323ff9ef60e7183fcd76ff7176b78872873b19606ff410eeddc2ef2c74e";
      });
    }) // {
    package-description-override = "Name:                wl-pprint\nVersion:             1.2.1\nCabal-Version:       >=1.6\nSynopsis:            The Wadler/Leijen Pretty Printer\nCategory:            Text\nDescription:\n This is a pretty printing library based on Wadler's paper \"A Prettier\n Printer\".  See the haddocks for full info.  This version allows the\n library user to declare overlapping instances of the 'Pretty' class.\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Daan Leijen\nMaintainer:          Noam Lewis <jones.noamle@gmail.com>\nBuild-Type:          Simple\n\nLibrary\n  Build-Depends:       base < 5\n  Exposed-Modules:     Text.PrettyPrint.Leijen\n\nsource-repository head\n  type: git\n  location: git@github.com:sinelaw/wl-pprint.git\n";
    }