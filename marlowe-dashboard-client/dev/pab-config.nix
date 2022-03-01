{ dbConfigFile
, baseUrl
, walletUrl
, socket-path
, network-id
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
    pscNetworkId = network-id;
    pscSlotConfig = {
      scSlotZeroTime = 1644929640000;
      scSlotLength = 1000;
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
    ciBaseUrl = "http://chain-index:9083";
    ciWatchedAddresses = [ ];
  };
  requestProcessingConfig = {
    requestProcessingInterval = 1;
  };
  signingProcessConfig = {
    spBaseUrl = "http://0.0.0.0:9084";
    spWallet = {
      getWallet = 1;
    };
  };
  metadataServerConfig = {
    mdBaseUrl = "http://0.0.0.0:9085";
  };
  developmentOptions = {
    pabRollbackHistory = null;
    pabResumeFrom = {
      tag = "PointAtGenesis";
    };
  };
}
