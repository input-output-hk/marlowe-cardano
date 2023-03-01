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
      specVersion = "3.0";
      identifier = { name = "plutus-ledger-api"; version = "1.0.0.1"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones, Jann Mueller";
      homepage = "";
      url = "";
      synopsis = "Interface to the Plutus ledger for the Cardano ledger.";
      description = "Interface to the Plutus scripting support for the Cardano ledger.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "plutus-ledger-api-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/plutus-ledger-api-1.0.0.1.tar.gz";
      sha256 = "e806c23c8b923a31af6676a5acb3db4648dcc12ceba9d60a7ca686d79afedace";
      });
    }) // {
    package-description-override = "cabal-version: 3.0\nname:          plutus-ledger-api\nversion:       1.0.0.1\nlicense:       Apache-2.0\nlicense-file:  LICENSE NOTICE\nmaintainer:    michael.peyton-jones@iohk.io\nauthor:        Michael Peyton Jones, Jann Mueller\nsynopsis:      Interface to the Plutus ledger for the Cardano ledger.\ndescription:\n    Interface to the Plutus scripting support for the Cardano ledger.\n\ncategory:      Language\nbuild-type:    Simple\n\nsource-repository head\n    type:     git\n    location: https://github.com/input-output-hk/plutus\n\nlibrary\n    exposed-modules:\n        Plutus.ApiCommon\n        Plutus.V1.Ledger.Address\n        Plutus.V1.Ledger.Api\n        Plutus.V1.Ledger.Bytes\n        Plutus.V1.Ledger.Contexts\n        Plutus.V1.Ledger.Credential\n        Plutus.V1.Ledger.Crypto\n        Plutus.V1.Ledger.DCert\n        Plutus.V1.Ledger.EvaluationContext\n        Plutus.V1.Ledger.Examples\n        Plutus.V1.Ledger.Interval\n        Plutus.V1.Ledger.ProtocolVersions\n        Plutus.V1.Ledger.Scripts\n        Plutus.V1.Ledger.Time\n        Plutus.V1.Ledger.Tx\n        Plutus.V1.Ledger.Value\n        Plutus.V2.Ledger.Api\n        Plutus.V2.Ledger.Contexts\n        Plutus.V2.Ledger.EvaluationContext\n        Plutus.V2.Ledger.Tx\n\n    hs-source-dirs:     src\n    other-modules:\n        Codec.CBOR.Extras\n        Data.Either.Extras\n        Prettyprinter.Extras\n\n    default-language:   Haskell2010\n    default-extensions:\n        DeriveFoldable DeriveFunctor DeriveGeneric DeriveLift\n        DeriveTraversable DerivingStrategies ExplicitForAll\n        FlexibleContexts GeneralizedNewtypeDeriving ImportQualifiedPost\n        MultiParamTypeClasses ScopedTypeVariables StandaloneDeriving\n\n    ghc-options:\n        -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n        -Wincomplete-record-updates -Wredundant-constraints -Widentities\n        -Wunused-packages -Wmissing-deriving-strategies -fobject-code\n        -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n\n    build-depends:\n        barbies,\n        base >=4.9 && <5,\n        base16-bytestring >=1,\n        bytestring,\n        cborg,\n        containers,\n        deepseq,\n        flat,\n        lens,\n        mtl,\n        nothunks,\n        plutus-core ^>=1.0,\n        plutus-tx ^>=1.0,\n        prettyprinter,\n        serialise,\n        tagged,\n        template-haskell,\n        text,\n        transformers\n\ntest-suite plutus-ledger-api-test\n    type:               exitcode-stdio-1.0\n    main-is:            Spec.hs\n    hs-source-dirs:     test\n    other-modules:\n        Spec.Builtins\n        Spec.Eval\n        Spec.Interval\n        Spec.NoThunks\n\n    default-language:   Haskell2010\n    default-extensions:\n        DeriveFoldable DeriveFunctor DeriveGeneric DeriveLift\n        DeriveTraversable DerivingStrategies ExplicitForAll\n        FlexibleContexts GeneralizedNewtypeDeriving ImportQualifiedPost\n        MultiParamTypeClasses ScopedTypeVariables StandaloneDeriving\n\n    ghc-options:\n        -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n        -Wincomplete-record-updates -Wredundant-constraints -Widentities\n        -Wunused-packages -Wmissing-deriving-strategies -fobject-code\n        -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n\n    build-depends:\n        base >=4.9 && <5,\n        bytestring,\n        containers,\n        extra,\n        hedgehog,\n        mtl,\n        nothunks,\n        plutus-core ^>=1.0,\n        plutus-ledger-api ^>=1.0,\n        serialise,\n        tasty,\n        tasty-hedgehog,\n        tasty-hunit,\n        tasty-quickcheck\n";
    }