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
    flags = { development = false; external-libsodium-vrf = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-crypto-praos"; version = "2.0.0.0.1"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Crypto primitives from libsodium";
      description = "VRF (and KES, tba) primitives from libsodium.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-crypto-praos-2.0.0.0.1.tar.gz";
      sha256 = "cd4951b54a9481175d631a0dffff804476e14272ef0ac3071201196a759e6bf8";
      });
    }) // {
    package-description-override = "cabal-version:      2.2\nname:               cardano-crypto-praos\nversion:            2.0.0.0.1\nlicense:            Apache-2.0\nlicense-file:       LICENSE NOTICE\ncopyright:          2019-2021 IOHK\nmaintainer:         operations@iohk.io\nauthor:             IOHK\nsynopsis:           Crypto primitives from libsodium\ndescription:        VRF (and KES, tba) primitives from libsodium.\ncategory:           Currency\nbuild-type:         Simple\nextra-source-files:\n    README.md\n    cbits/crypto_vrf.h\n    cbits/vrf03/crypto_vrf_ietfdraft03.h\n    cbits/vrf03/vrf_ietfdraft03.h\n    cbits/private/common.h\n    cbits/private/quirks.h\n    cbits/private/ed25519_ref10.h\n    cbits/private/ed25519_ref10_fe_25_5.h\n    cbits/private/ed25519_ref10_fe_51.h\n    cbits/private/fe_25_5/constants.h\n    cbits/private/fe_25_5/base.h\n    cbits/private/fe_25_5/base2.h\n    cbits/private/fe_25_5/fe.h\n    cbits/private/fe_51/constants.h\n    cbits/private/fe_51/base.h\n    cbits/private/fe_51/base2.h\n    cbits/private/fe_51/fe.h\n\nflag development\n    description: Disable `-Werror`\n    default:     False\n    manual:      True\n\nflag external-libsodium-vrf\n    description:\n        Rely on a special libsodium fork containing the VRF code.\n        Otherwise expect a normal unaltered system libsodium, and\n        bundle the VRF code.\n\n    manual:      True\n\nlibrary\n    exposed-modules:\n        Cardano.Crypto.VRF.Praos\n        Cardano.Crypto.RandomBytes\n\n    pkgconfig-depends: libsodium\n    hs-source-dirs:    src\n    default-language:  Haskell2010\n    ghc-options:\n        -Wall -Wcompat -Wincomplete-record-updates\n        -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints\n        -Wunused-packages\n\n    build-depends:\n        base >=4.14 && <4.15,\n        base,\n        bytestring,\n        cardano-binary,\n        cardano-crypto-class,\n        cardano-prelude,\n        nothunks\n\n    if !flag(development)\n        ghc-options: -Werror\n\n    if !flag(external-libsodium-vrf)\n        c-sources:\n            cbits/crypto_vrf.c\n            cbits/vrf03/convert.c\n            cbits/vrf03/keypair.c\n            cbits/vrf03/prove.c\n            cbits/vrf03/verify.c\n            cbits/vrf03/vrf.c\n            cbits/private/ed25519_ref10.c\n";
    }