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
      specVersion = "1.8";
      identifier = { name = "hfsevents"; version = "0.1.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "stegeman@gmail.com";
      author = "Luite Stegeman";
      homepage = "http://github.com/luite/hfsevents";
      url = "";
      synopsis = "File/folder watching for OS X";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        libs = [ (pkgs."pthread" or (errorHandler.sysDepError "pthread")) ];
        frameworks = [ (pkgs."Cocoa" or (errorHandler.sysDepError "Cocoa")) ];
        buildable = if system.isOsx then true else false;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/hfsevents-0.1.6.tar.gz";
      sha256 = "74c3f3f3a5e55fff320c352a2d481069ff915860a0ab970864c6a0e6b65d3f05";
      });
    }) // {
    package-description-override = "name:                hfsevents\nversion:             0.1.6\nsynopsis:            File/folder watching for OS X\nhomepage:            http://github.com/luite/hfsevents\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Luite Stegeman\nmaintainer:          stegeman@gmail.com\ncategory:            System\nbuild-type:          Simple\nextra-source-files:  cbits/c_fsevents.h, test/test.hs, test/trace.hs\ncabal-version:       >=1.8\n\nsource-repository head\n  type:     git\n  location: https://github.com/luite/hfsevents.git\n\nlibrary\n  exposed-modules: System.OSX.FSEvents\n  if os(darwin)\n    buildable: True\n  else\n    buildable: False\n  frameworks: Cocoa\n  C-sources: cbits/c_fsevents.m\n  include-dirs: cbits\n  extra-libraries: pthread\n  build-depends:\n    base >= 4 && < 5,\n    bytestring,\n    cereal >= 0.3 && < 0.6,\n    unix,\n    mtl,\n    text\n\n";
    }