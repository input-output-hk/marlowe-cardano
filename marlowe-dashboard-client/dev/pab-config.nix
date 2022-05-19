# TODO unify with bitte/pab.nix
{ contractStoreConfig ? { tag = "UseInMemoryStore"; }
, baseUrl
, walletUrl
, chainIndexUrl
, socket-path
, network
, protocol-parameters
}: {
  inherit contractStoreConfig;
  # contractStoreConfig = {
  #   tag = "UseFSStore";
  #   contents = fsStorePath
  # };
  # contractStoreConfig = {
  #   tag = "UseSqliteStore";
  #   contents = {
  #     sqliteConfigFile = dbConfigFile;
  #     sqliteConfigPoolSize = 20;
  #   };
  # };
  pabWebserverConfig = {
    inherit baseUrl;
    permissiveCorsPolicy = false;
  };
  walletServerConfig = {
    tag = "LocalWalletConfig";
    walletSettings = {
      baseUrl = walletUrl;
    };
  };
  nodeServerConfig = {
    pscSocketPath = socket-path;
    pscBaseUrl = "node";
    pscKeptBlocks = 2160;
    pscNetworkId = "${toString network.magic}";
    pscSlotConfig = {
      scSlotZeroTime = network.slotZeroTime;
      scSlotLength = network.slotLengthMillis;
    };
    pscFeeConfig = {
      fcConstantFee = {
        getLovelace = 10;
      };
      fcScriptsFeeFactor = 1.0;
    };
    pscInitialTxWallets = [ ];
    pscNodeMode = "AlonzoNode";
    pscProtocolParametersJsonPath = protocol-parameters;
  };
  chainIndexConfig = {
    ciBaseUrl = chainIndexUrl;
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
    pabResumeFrom = {
      tag = "PointAtGenesis";
    };
  };
}
