# Needed variables:
#   PAB_STATE_DIR
#   NOMAD_ADDR_wbe
#   NOMAD_ADDR_node
#   NOMAD_ADDR_index
#   NOMAD_PORT_pab
#   NOMAD_ALLOC_DIR # With node socket in $NOMAD_ALLOC_DIR/node.sock

{ writeShellScriptBin, writeText, pabExe, staticPkg, cacert, coreutils, lib, gnused, utillinux, wait-for-socket }:
let
  slotZeroTime = 1638215277000; # POSIX time of slot zero is milliseconds. See note [Datetime to slot] in Marlowe.Slot
  slotLengthMillis = 1000;

  constantFee = 10; # Constant fee per transaction in lovelace
  scriptsFeeFactor = 1.0; # Factor by which to multiply the size-dependent scripts fee in lovelace

  pabYamlIn = writeText "pab.yaml.in" (builtins.toJSON {
    dbConfig = {
      dbConfigFile = "@PAB_STATE_DIR@/pab.db";
      dbConfigPoolSize = 20;
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
      pscRandomTxInterval = 20000000;
      pscNetworkId = "1564";
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
      pabRollbackHistory = null;
      pabResumeFrom = {
        tag = "PointAtGenesis";
      };
    };
  });

  dbFile = "$PAB_STATE_DIR/pab.db";

  # Note: The db is dropped as a workaround for a problem with
  # eventful which crashes PAB. Currently data persistence is not
  # relevant, but the problem *will* occur again when the DB removal
  # is removed unless the underlying problem is identified/fixed.
  pab-init-cmd = writeShellScriptBin "pab-init-cmd" ''
    set -eEuo pipefail

    echo "[pab-init-cmd]: Dropping PAB database file '${dbFile}'" >&2
    rm -rf "${dbFile}"

    echo "[pab-init-cmd]: Creating new DB '${dbFile}'" >&2
    ${pabExe} --config=pab.yaml migrate
  '';
in
writeShellScriptBin "entrypoint" ''
  set -eEuo pipefail

  export PATH=${lib.makeBinPath [ coreutils gnused utillinux wait-for-socket ]}

  export SYSTEM_CERTIFICATE_PATH=${cacert}/etc/ssl/certs/ca-bundle.crt

  sed -e "s|@PAB_STATE_DIR@|$PAB_STATE_DIR|g" \
      -e "s|@NOMAD_PORT_pab@|$NOMAD_PORT_pab|g" \
      -e "s|@NOMAD_ADDR_wbe@|$NOMAD_ADDR_wbe|g" \
      -e "s|@NOMAD_ADDR_node@|$NOMAD_ADDR_node|g" \
      -e "s|@NOMAD_ALLOC_DIR@|$NOMAD_ALLOC_DIR|g" \
      -e "s|@NOMAD_ADDR_index@|$NOMAD_ADDR_index|g" \
      ${pabYamlIn} > pab.yaml

  wait-for-socket "$NOMAD_ALLOC_DIR/node.sock"

  ${pab-init-cmd}/bin/pab-init-cmd

  # Ugly ugly hack to kill the PAB at midnight UTC
  ${pabExe} --config=pab.yaml webserver &
  sleep $(($(date -f - +%s- <<< $'tomorrow 00:00\nnow')0))&
  wait -n
  exit 1
''
