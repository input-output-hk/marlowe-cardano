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
      identifier = { name = "errors"; version = "2.3.0"; };
      license = "BSD-3-Clause";
      copyright = "2012, 2013 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Simplified error-handling";
      description = "The one-stop shop for all your error-handling needs!  Just import\n\"Control.Error\".\n\nThis library encourages an error-handling style that directly uses the type\nsystem, rather than out-of-band exceptions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ [ (hsPkgs."safe" or (errorHandler.buildDepError "safe")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/errors-2.3.0.tar.gz";
      sha256 = "6772e5689f07e82077ffe3339bc672934d83d83a97a7d4f1349de1302cb71f75";
      });
    }) // {
    package-description-override = "Name: errors\r\nVersion: 2.3.0\r\nx-revision: 4\r\nCabal-Version: >=1.8.0.2\r\nBuild-Type: Simple\r\nTested-With: GHC == 7.8.4, GHC == 7.10.2, GHC == 8.0.1\r\nLicense: BSD3\r\nLicense-File: LICENSE\r\nCopyright: 2012, 2013 Gabriella Gonzalez\r\nAuthor: Gabriella Gonzalez\r\nMaintainer: GenuineGabriella@gmail.com\r\nBug-Reports: https://github.com/Gabriella439/Haskell-Errors-Library/issues\r\nSynopsis: Simplified error-handling\r\nDescription:\r\n    The one-stop shop for all your error-handling needs!  Just import\r\n    \"Control.Error\".\r\n    .\r\n    This library encourages an error-handling style that directly uses the type\r\n    system, rather than out-of-band exceptions.\r\nCategory: Control, Error Handling\r\nextra-source-files: CHANGELOG.md\r\nSource-Repository head\r\n    Type: git\r\n    Location: https://github.com/Gabriella439/Haskell-Errors-Library\r\n\r\nLibrary\r\n    Build-Depends:\r\n        base                >= 4.7   && < 5   ,\r\n        exceptions          >= 0.6   && < 0.11,\r\n        text                            < 2.1 ,\r\n        transformers        >= 0.2   && < 0.7 ,\r\n        transformers-compat >= 0.4   && < 0.8\r\n    if impl(ghc <= 7.6.3)\r\n        Build-Depends:\r\n            safe            >= 0.3.3 && < 0.3.10\r\n    else\r\n        Build-Depends:\r\n            safe            >= 0.3.3 && < 0.4\r\n    Exposed-Modules:\r\n        Control.Error,\r\n        Control.Error.Safe,\r\n        Control.Error.Script,\r\n        Control.Error.Util,\r\n        Data.EitherR\r\n";
    }