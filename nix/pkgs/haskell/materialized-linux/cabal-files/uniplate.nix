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
      specVersion = "1.18";
      identifier = { name = "uniplate"; version = "1.6.13"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2006-2020";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/uniplate#readme";
      url = "";
      synopsis = "Help writing simple, concise and fast generic operations.";
      description = "Uniplate is library for writing simple and concise generic operations.\nUniplate has similar goals to the original Scrap Your Boilerplate work,\nbut is substantially simpler and faster.\n\nTo get started with Uniplate you should import one of the three following\nmodules:\n\n* \"Data.Generics.Uniplate.Data\" - to quickly start writing generic functions.\nMost users should start by importing this module.\n\n* \"Data.Generics.Uniplate.Direct\" - a replacement for \"Data.Generics.Uniplate.Data\"\nwith substantially higher performance (around 5 times), but requires writing\ninstance declarations.\n\n* \"Data.Generics.Uniplate.Operations\" - definitions of all the operations defined\nby Uniplate. Both the above two modules re-export this module.\n\nIn addition, some users may want to make use of the following modules:\n\n* \"Data.Generics.Uniplate.Zipper\" - a zipper built on top of Uniplate instances.\n\n* \"Data.Generics.SYB\" - users transitioning from the Scrap Your Boilerplate library.\n\n* \"Data.Generics.Compos\" - users transitioning from the Compos library.\n\n* \"Data.Generics.Uniplate.DataOnly\" - users making use of both @Data@ and @Direct@\nto avoid getting instance conflicts.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/uniplate-1.6.13.tar.gz";
      sha256 = "e777c94628445556a71f135a42cf72d2cfbaccba5849cc42fbfec8b2182e3ad2";
      });
    }) // {
    package-description-override = "cabal-version:      >= 1.18\r\nbuild-type:         Simple\r\nname:               uniplate\r\nversion:            1.6.13\r\nx-revision: 1\r\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\r\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\r\ncopyright:          Neil Mitchell 2006-2020\r\nhomepage:           https://github.com/ndmitchell/uniplate#readme\r\nbug-reports:        https://github.com/ndmitchell/uniplate/issues\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\nsynopsis:           Help writing simple, concise and fast generic operations.\r\ncategory:           Generics\r\ndescription:\r\n    Uniplate is library for writing simple and concise generic operations.\r\n    Uniplate has similar goals to the original Scrap Your Boilerplate work,\r\n    but is substantially simpler and faster.\r\n    .\r\n    To get started with Uniplate you should import one of the three following\r\n    modules:\r\n    .\r\n    * \"Data.Generics.Uniplate.Data\" - to quickly start writing generic functions.\r\n    Most users should start by importing this module.\r\n    .\r\n    * \"Data.Generics.Uniplate.Direct\" - a replacement for \"Data.Generics.Uniplate.Data\"\r\n    with substantially higher performance (around 5 times), but requires writing\r\n    instance declarations.\r\n    .\r\n    * \"Data.Generics.Uniplate.Operations\" - definitions of all the operations defined\r\n    by Uniplate. Both the above two modules re-export this module.\r\n    .\r\n    In addition, some users may want to make use of the following modules:\r\n    .\r\n    * \"Data.Generics.Uniplate.Zipper\" - a zipper built on top of Uniplate instances.\r\n    .\r\n    * \"Data.Generics.SYB\" - users transitioning from the Scrap Your Boilerplate library.\r\n    .\r\n    * \"Data.Generics.Compos\" - users transitioning from the Compos library.\r\n    .\r\n    * \"Data.Generics.Uniplate.DataOnly\" - users making use of both @Data@ and @Direct@\r\n    to avoid getting instance conflicts.\r\n\r\nextra-source-files:\r\n    Data/Generics/Uniplate/Internal/DataInc.hs\r\n    Data/Generics/Uniplate/Internal/OperationsInc.hs\r\nextra-doc-files:\r\n    README.md\r\n    CHANGES.txt\r\ntested-with: GHC==8.10, GHC==8.8, GHC==8.6, GHC==8.4, GHC==8.2, GHC==8.0\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: https://github.com/ndmitchell/uniplate.git\r\n\r\nlibrary\r\n    default-language: Haskell2010\r\n    build-depends:\r\n        base >=4.10 && <5, containers, syb, ghc-prim,\r\n        hashable >= 1.1.2.3,\r\n        unordered-containers >= 0.2.1\r\n\r\n    exposed-modules:\r\n        Data.Generics.Str\r\n        Data.Generics.Compos\r\n        Data.Generics.SYB\r\n        Data.Generics.Uniplate.Data\r\n        Data.Generics.Uniplate.Data.Instances\r\n        Data.Generics.Uniplate.DataOnly\r\n        Data.Generics.Uniplate.Direct\r\n        Data.Generics.Uniplate.Operations\r\n        Data.Generics.Uniplate.Typeable\r\n        Data.Generics.Uniplate.Zipper\r\n\r\n        -- DEPRECATED\r\n        Data.Generics.Uniplate\r\n        Data.Generics.UniplateOn\r\n        Data.Generics.UniplateStr\r\n        Data.Generics.UniplateStrOn\r\n        Data.Generics.Biplate\r\n        Data.Generics.PlateDirect\r\n        Data.Generics.PlateTypeable\r\n        Data.Generics.PlateData\r\n\r\n    other-modules:\r\n        Data.Generics.Uniplate.Internal.Data\r\n        Data.Generics.Uniplate.Internal.DataOnlyOperations\r\n        Data.Generics.Uniplate.Internal.Utils\r\n";
    }