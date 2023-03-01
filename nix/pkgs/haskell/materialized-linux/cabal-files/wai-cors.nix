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
      specVersion = "1.22";
      identifier = { name = "wai-cors"; version = "0.2.7"; };
      license = "MIT";
      copyright = "(c) 2015-2019 Lars Kuhtz <lakuhtz@gmail.com>,\n(c) 2014 AlephCloud Systems, Inc.";
      maintainer = "Lars Kuhtz <lakuhtz@gmail.com>";
      author = "Lars Kuhtz <lakuhtz@gmail.com>";
      homepage = "https://github.com/larskuhtz/wai-cors";
      url = "";
      synopsis = "CORS for WAI";
      description = "This package provides an implemenation of\nCross-Origin resource sharing (CORS) for\n<http://hackage.haskell.org/package/wai Wai>\nthat aims to be compliant with <http://www.w3.org/TR/cors>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-unicode-symbols" or (errorHandler.buildDepError "base-unicode-symbols"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          ];
        buildable = true;
        };
      tests = {
        "phantomjs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-unicode-symbols" or (errorHandler.buildDepError "base-unicode-symbols"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
            (hsPkgs."wai-websockets" or (errorHandler.buildDepError "wai-websockets"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            ];
          buildable = true;
          };
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-unicode-symbols" or (errorHandler.buildDepError "base-unicode-symbols"))
            (hsPkgs."base-unicode-symbols" or (errorHandler.buildDepError "base-unicode-symbols"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."wai-websockets" or (errorHandler.buildDepError "wai-websockets"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wai-cors-0.2.7.tar.gz";
      sha256 = "2597beb56ebd7148f9755ae2661c065a6c532e0a286717061861b149a51cfb81";
      });
    }) // {
    package-description-override = "-- ------------------------------------------------------ --\n-- Copyright © 2015-2019 Lars Kuhtz <lakuhtz@gmail.com>\n-- Copyright © 2014 AlephCloud Systems, Inc.\n-- ------------------------------------------------------ --\n\nName: wai-cors\nVersion: 0.2.7\nSynopsis: CORS for WAI\n\nDescription:\n    This package provides an implemenation of\n    Cross-Origin resource sharing (CORS) for\n    <http://hackage.haskell.org/package/wai Wai>\n    that aims to be compliant with <http://www.w3.org/TR/cors>.\n\nHomepage: https://github.com/larskuhtz/wai-cors\nBug-reports: https://github.com/larskuhtz/wai-cors/issues\nLicense: MIT\nLicense-file: LICENSE\nAuthor: Lars Kuhtz <lakuhtz@gmail.com>\nMaintainer: Lars Kuhtz <lakuhtz@gmail.com>\nCopyright:\n    (c) 2015-2019 Lars Kuhtz <lakuhtz@gmail.com>,\n    (c) 2014 AlephCloud Systems, Inc.\nCategory: HTTP, Network, Web, Wai\nBuild-type: Simple\nCabal-version: 1.22\ntested-with:\n    GHC == 7.10.3\n    GHC == 8.0.2\n    GHC == 8.2.2\n    GHC == 8.4.4\n    GHC == 8.6.5\n\ndata-files:\n    README.md\n    CHANGELOG.md\n    test/index.html\n    test/phantomjs.js\n    examples/Scotty.hs\n    examples/Wai.hs\n    examples/ServantWai.hs\n\nsource-repository head\n    type: git\n    location: https://github.com/larskuhtz/wai-cors\n    branch: master\n\nsource-repository this\n    type: git\n    location: https://github.com/larskuhtz/wai-cors\n    tag: 0.2.7\n\nLibrary\n    default-language: Haskell2010\n    hs-source-dirs: src\n\n    exposed-modules:\n        Network.Wai.Middleware.Cors\n\n    build-depends:\n        attoparsec >= 0.10.4.0,\n        base >= 4.8 && <5.0,\n        base-unicode-symbols >= 0.2.2.3,\n        bytestring >= 0.10.0.2,\n        case-insensitive >= 1.0.0.1,\n        http-types >= 0.8.0,\n        mtl >= 2.2,\n        transformers >= 0.4,\n        wai >= 3.0\n\n    ghc-options: -Wall\n\nTest-Suite phantomjs\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    main-is: PhantomJS.hs\n    hs-source-dirs: test\n\n    other-modules:\n        Server\n\n    build-depends:\n        base >= 4.8 && <5.0,\n        base-unicode-symbols >= 0.2,\n        directory >= 1.2,\n        filepath >= 1.4,\n        http-types >= 0.8,\n        network >= 2.6,\n        process >= 1.2,\n        text >= 1.2,\n        wai >= 3.0,\n        wai-cors,\n        wai-websockets >= 3.0,\n        warp >= 3.0,\n        websockets >= 0.9\n\n    ghc-options: -threaded -Wall -with-rtsopts=-N\n\nTest-Suite unit-tests\n    type: exitcode-stdio-1.0\n    default-language: Haskell2010\n    main-is: UnitTests.hs\n    hs-source-dirs: test\n\n    build-depends:\n        base >= 4.8 && <5.0,\n        base-unicode-symbols >= 0.2,\n        base-unicode-symbols >= 0.2,\n        http-types >= 0.8,\n        tasty >= 0.11,\n        tasty-hunit >= 0.9,\n        wai >= 3.0,\n        wai-cors,\n        wai-extra >= 3.0,\n        wai-websockets >= 3.0.1,\n        warp >= 3.0,\n        websockets >= 0.10\n\n    ghc-options: -threaded -Wall -with-rtsopts=-N\n\n";
    }