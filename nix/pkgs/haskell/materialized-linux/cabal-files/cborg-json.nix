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
      identifier = { name = "cborg-json"; version = "0.2.5.0"; };
      license = "BSD-3-Clause";
      copyright = "2015-2017 Duncan Coutts,\n2015-2017 Well-Typed LLP,\n2015 IRIS Connect Ltd";
      maintainer = "ben@smart-cactus.org";
      author = "Duncan Coutts";
      homepage = "https://github.com/well-typed/cborg";
      url = "";
      synopsis = "A library for encoding JSON as CBOR";
      description = "This package implements the bijection between JSON and\nCBOR defined in the CBOR specification, RFC 7049.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          ];
        buildable = true;
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."cborg-json" or (errorHandler.buildDepError "cborg-json"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cborg-json-0.2.5.0.tar.gz";
      sha256 = "58c373453f06fd8558e062c4fbe6dd862cb0ae28ac3c02ba3b607573bd077cd4";
      });
    }) // {
    package-description-override = "name:                cborg-json\r\nversion:             0.2.5.0\r\nx-revision: 1\r\nsynopsis:            A library for encoding JSON as CBOR\r\ndescription:         This package implements the bijection between JSON and\r\n                     CBOR defined in the CBOR specification, RFC 7049.\r\nhomepage:            https://github.com/well-typed/cborg\r\nlicense:             BSD3\r\nlicense-file:        LICENSE.txt\r\nauthor:              Duncan Coutts\r\nmaintainer:          ben@smart-cactus.org\r\nbug-reports:         https://github.com/well-typed/cborg/issues\r\ncopyright:           2015-2017 Duncan Coutts,\r\n                     2015-2017 Well-Typed LLP,\r\n                     2015 IRIS Connect Ltd\r\ncategory:            Codec\r\nbuild-type:          Simple\r\nextra-source-files:  ChangeLog.md\r\ncabal-version:       >=1.10\r\ntested-with:\r\n  GHC == 8.4.4,\r\n  GHC == 8.6.5,\r\n  GHC == 8.8.3,\r\n  GHC == 8.10.7,\r\n  GHC == 9.0.1,\r\n  GHC == 9.2.2,\r\n  GHC == 9.4.2\r\n\r\nlibrary\r\n  exposed-modules:     Codec.CBOR.JSON\r\n  ghc-options:         -Wall\r\n  build-depends:\r\n    base                 >=4.11 && < 4.18,\r\n    aeson                >=0.7  && <2.2,\r\n    aeson-pretty         >=0.8  && <0.9,\r\n    unordered-containers >=0.2  && <0.3,\r\n    scientific           >=0.3  && <0.4,\r\n    text                 >=1.1  && <2.1,\r\n    vector               >=0.10 && <0.14,\r\n\r\n    cborg ==0.2.*\r\n\r\n  hs-source-dirs:      src\r\n  default-language:    Haskell2010\r\n\r\n  if impl(ghc >= 8.0)\r\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\r\n\r\n\r\n\r\nbenchmark bench\r\n  type:              exitcode-stdio-1.0\r\n  hs-source-dirs:    bench\r\n  main-is:           Main.hs\r\n\r\n  default-language:  Haskell2010\r\n  ghc-options:\r\n    -Wall -rtsopts -fno-cse -fno-ignore-asserts -fno-warn-orphans -O2\r\n\r\n  other-modules:\r\n\r\n  build-depends:\r\n    base       >= 4.11    && < 4.18,\r\n    bytestring >= 0.10.4  && < 0.12,\r\n    criterion  >= 1.0     && < 1.6,\r\n    deepseq    >= 1.0     && < 1.5,\r\n    zlib       >= 0.5     && < 0.7,\r\n    directory,\r\n    process,\r\n    aeson,\r\n\r\n    cborg,\r\n    cborg-json\r\n";
    }