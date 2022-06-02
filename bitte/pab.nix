# Needed variables:
#   PAB_STATE_DIR
#   NOMAD_ADDR_wbe
#   NOMAD_ADDR_node
#   NOMAD_ADDR_index
#   NOMAD_PORT_pab
#   NOMAD_ALLOC_DIR # With node socket in $NOMAD_ALLOC_DIR/node.sock

{ writeShellScriptBin
, writeText
, pabExe
, staticPkg
, network
, cacert
, coreutils
, lib
, gnused
, utillinux
, wait-for-socket
, sleep-until-restart-slot
}:
let
  inherit (network) slotZeroTime slotLengthMillis;

  constantFee = 10; # Constant fee per transaction in lovelace
  scriptsFeeFactor = 1.0; # Factor by which to multiply the size-dependent scripts fee in lovelace

  pabYamlIn = writeText "pab.yaml.in" (builtins.toJSON {
    contractStoreConfig = {
      tag = "UseFSStore";
      contents = "@PAB_STATE_DIR@/contract-store";
    };

    pabWebserverConfig = {
      baseUrl = "http://localhost:@NOMAD_PORT_pab@";
      staticDir = "${staticPkg}";
      permissiveCorsPolicy = false;
    };

    walletServerConfig = {
      tag = "LocalWalletConfig";
      walletSettings = {
        baseUrl = "@NOMAD_ADDR_wbe@";
      };
    };

    nodeServerConfig = {
      pscSocketPath = "@NOMAD_ALLOC_DIR@/node.sock";
      pscBaseUrl = "@NOMAD_ADDR_node@";
      pscKeptBlocks = 2160;
      pscNetworkId = "${toString network.magic}";
      pscSlotConfig = {
        scSlotZeroTime = slotZeroTime;
        scSlotLength = slotLengthMillis;
      };
      pscFeeConfig = {
        fcConstantFee = {
          getLovelace = constantFee;
        };
        fcScriptsFeeFactor = scriptsFeeFactor;
      };
      pscInitialTxWallets = [ ];
      pscNodeMode = "AlonzoNode";
      pscProtocolParametersJsonPath = ../marlowe-dashboard-client/private-testnet.protocol;
    };

    chainIndexConfig = {
      ciBaseUrl = "@NOMAD_ADDR_index@";
      ciWatchedAddresses = [ ];
    };

    requestProcessingConfig = {
      requestProcessingInterval = 1;
    };

    signingProcessConfig = {
      spBaseUrl = "http://fixme";
      spWallet = {
        getWallet = 1;
      };
    };

    metadataServerConfig = {
      mdBaseUrl = "http://fixme";
    };

    developmentOptions = {
      pabRollbackHistory = 100;
      pabResumeFrom = network.pabResumeFrom;
    };
  });

  dbFile = "$PAB_STATE_DIR/pab.db";

  fsStore = "$PAB_STATE_DIR/contract-store";

  # Note: The db is dropped as a workaround for a problem with
  # eventful which crashes PAB. Currently data persistence is not
  # relevant, but the problem *will* occur again when the DB removal
  # is removed unless the underlying problem is identified/fixed.
  pab-init-cmd = writeShellScriptBin "pab-init-cmd" ''
    set -eEuo pipefail

    echo "[pab-init-cmd]: Dropping PAB database file '${dbFile}'" >&2
    rm -rf "${dbFile}"

    # Uncomment when using SQLite
    # echo "[pab-init-cmd]: Creating new DB '${dbFile}'" >&2
    # ${pabExe} --config=pab.yaml migrate

    mkdir -p ${fsStore}
  '';
in
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH=${lib.makeBinPath [ coreutils gnused utillinux wait-for-socket sleep-until-restart-slot ]}

  export SYSTEM_CERTIFICATE_PATH=${cacert}/etc/ssl/certs/ca-bundle.crt

  # Always start fresh
  rm -fR "$PAB_STATE_DIR"
  mkdir -p "$PAB_STATE_DIR"

  sed -e "s|@PAB_STATE_DIR@|$PAB_STATE_DIR|g" \
      -e "s|@NOMAD_PORT_pab@|$NOMAD_PORT_pab|g" \
      -e "s|@NOMAD_ADDR_wbe@|$NOMAD_ADDR_wbe|g" \
      -e "s|@NOMAD_ADDR_node@|$NOMAD_ADDR_node|g" \
      -e "s|@NOMAD_ALLOC_DIR@|$NOMAD_ALLOC_DIR|g" \
      -e "s|@NOMAD_ADDR_index@|$NOMAD_ADDR_index|g" \
      ${pabYamlIn} > pab.yaml

  wait-for-socket "$NOMAD_ALLOC_DIR/node.sock"

  ${pab-init-cmd}/bin/pab-init-cmd

  ${pabExe} --config=pab.yaml webserver --passphrase fixme-allow-pass-per-wallet &
  sleep-until-restart-slot&
  wait -n
  exit 1
''
