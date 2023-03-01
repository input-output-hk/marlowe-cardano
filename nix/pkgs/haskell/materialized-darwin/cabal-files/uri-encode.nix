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
    flags = { tools = false; network-uri = true; };
    package = {
      specVersion = "1.18";
      identifier = { name = "uri-encode"; version = "1.5.0.7"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "code@silk.co";
      author = "Silk";
      homepage = "";
      url = "";
      synopsis = "Unicode aware uri-encoding";
      description = "This package allows you to uri encode and uri decode\nStrings, Texts and ByteString values.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          ] ++ (if flags.network-uri
          then [
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ]);
        buildable = true;
        };
      exes = {
        "uri-encode" = {
          depends = (pkgs.lib).optionals (flags.tools) ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ] ++ (if flags.network-uri
            then [
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              ]
            else [
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ]));
          buildable = if flags.tools then true else false;
          };
        "uri-decode" = {
          depends = (pkgs.lib).optionals (flags.tools) ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ] ++ (if flags.network-uri
            then [
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              ]
            else [
              (hsPkgs."network" or (errorHandler.buildDepError "network"))
              ]));
          buildable = if flags.tools then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/uri-encode-1.5.0.7.tar.gz";
      sha256 = "c79c624257833841a22890e4d2b0ab07e4be88e0f99474d328223815c0814252";
      });
    }) // {
    package-description-override = "name:                uri-encode\r\nversion:             1.5.0.7\r\nx-revision: 2\r\nsynopsis:            Unicode aware uri-encoding\r\ndescription:         This package allows you to uri encode and uri decode\r\n                     Strings, Texts and ByteString values.\r\ncabal-version:       1.18\r\ncategory:            Network, Web\r\nauthor:              Silk\r\nmaintainer:          code@silk.co\r\nlicense:             BSD3\r\nlicense-file:        LICENSE\r\nbuild-type:          Simple\r\n\r\nextra-doc-files:\r\n  CHANGELOG.md\r\n  LICENSE\r\n  README.md\r\n\r\nflag tools\r\n  description:       Build executables\r\n  default:           False\r\n  manual:            True\r\n\r\nflag network-uri\r\n  description:       Get Network.URI from the network-uri package\r\n  default:           True\r\n\r\nsource-repository head\r\n  type:              git\r\n  location:          https://github.com/silkapp/uri-encode.git\r\n\r\nlibrary\r\n  ghc-options:       -Wall\r\n  hs-source-dirs:    src\r\n  exposed-modules:   Network.URI.Encode\r\n  build-depends:\r\n      base == 4.*\r\n    , bytestring >= 0.9 && < 0.12\r\n    , text >= 0.7 && < 2.1\r\n    , utf8-string >= 0.3 && < 1.1\r\n  if flag(network-uri)\r\n    build-depends: network-uri >= 2.6\r\n  else\r\n    build-depends: network (>= 2.2 && < 2.4.1.0) || (> 2.4.1.0 && < 2.6)\r\n  default-language: Haskell2010\r\n\r\nexecutable uri-encode\r\n  main-is:           URIEncode.hs\r\n  ghc-options:       -Wall\r\n  hs-source-dirs:    src\r\n  if flag(tools)\r\n    buildable:       True\r\n    build-depends:\r\n        base == 4.*\r\n      , bytestring >= 0.9 && < 0.12\r\n      , text >= 0.7 && < 1.3\r\n      , utf8-string >= 0.3 && < 1.1\r\n    if flag(network-uri)\r\n      build-depends: network-uri >= 2.6\r\n    else\r\n      build-depends: network (>= 2.2 && < 2.4.1.0) || (> 2.4.1.0 && < 2.6)\r\n  else\r\n    buildable:      False\r\n  default-language: Haskell2010\r\n\r\nexecutable uri-decode\r\n  main-is:           URIDecode.hs\r\n  ghc-options:       -Wall\r\n  hs-source-dirs:    src\r\n  if flag(tools)\r\n    buildable:       True\r\n    build-depends:\r\n        base == 4.*\r\n      , bytestring >= 0.9 && < 0.12\r\n      , text >= 0.7 && < 1.3\r\n      , utf8-string >= 0.3 && < 1.1\r\n    if flag(network-uri)\r\n      build-depends: network-uri >= 2.6\r\n    else\r\n      build-depends: network (>= 2.2 && < 2.4.1.0) || (> 2.4.1.0 && < 2.6)\r\n  else\r\n    buildable:       False\r\n  default-language: Haskell2010\r\n";
    }