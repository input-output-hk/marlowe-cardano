{ dbConfigFile
, baseUrl
, walletUrl
, chainIndexUrl
, socket-path
, network
, protocol-parameters
}: {
  dbConfig = {
    inherit dbConfigFile;
    dbConfigPoolSize = 20;
  };
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
    pabRollbackHistory = null;
    pabResumeFrom = {
      tag = "PointAtGenesis";
    };
  };
}
