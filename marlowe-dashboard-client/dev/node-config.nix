{
  AlonzoGenesisFile = "./alonzo-genesis.json";
  ByronGenesisFile = "./byron-genesis.json";
  ShelleyGenesisFile = "./shelly-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
  ByronGenesisHash = "2f1e13c4116da44b22e555ca6249ee284788308bdca3ee635dc1b7fff1ca8cc1";
  LastKnownBlockVersion-Alt = 0;
  LastKnownBlockVersion-Major = 5;
  LastKnownBlockVersion-Minor = 1;
  MaxConcurrencyDeadline = 4;
  MaxKnownMajorProtocolVersion = 5;
  PBftSignatureThreshold = 1.1;
  Protocol = "Cardano";
  RequiresNetworkMagic = "RequiresMagic";
  ShelleyGenesisHash = "56e7a4a5813a270cd4249ff1515ecb15ce444afda33c62c2e862452077d01ea5";
  TestAllegraHardForkAtEpoch = 2;
  TestAlonzoHardForkAtEpoch = 4;
  TestEnableDevelopmentHardForkEras = false;
  TestEnableDevelopmentNetworkProtocols = false;
  TestMaryHardForkAtEpoch = 3;
  TestShelleyHardForkAtEpoch = 1;
  TraceAcceptPolicy = true;
  TraceBlockFetchClient = false;
  TraceBlockFetchDecisions = false;
  TraceBlockFetchProtocol = false;
  TraceBlockFetchProtocolSerialised = false;
  TraceBlockFetchServer = false;
  TraceChainDb = true;
  TraceChainSyncBlockServer = false;
  TraceChainSyncClient = false;
  TraceChainSyncHeaderServer = false;
  TraceChainSyncProtocol = false;
  TraceConnectionManager = true;
  TraceDNSResolver = false;
  TraceDNSSubscription = false;
  TraceDiffusionInitialization = true;
  TraceErrorPolicy = true;
  TraceForge = true;
  TraceHandshake = false;
  TraceInboundGovernor = true;
  TraceIpSubscription = false;
  TraceLedgerPeers = true;
  TraceLocalChainSyncProtocol = false;
  TraceLocalErrorPolicy = true;
  TraceLocalHandshake = false;
  TraceLocalRootPeers = true;
  TraceLocalTxSubmissionProtocol = false;
  TraceLocalTxSubmissionServer = false;
  TraceMempool = true;
  TraceMux = false;
  TracePeerSelection = false;
  TracePeerSelectionActions = false;
  TracePublicRootPeers = false;
  TraceServer = true;
  TraceTxInbound = false;
  TraceTxOutbound = false;
  TraceTxSubmissionProtocol = false;
  TracingVerbosity = "NormalVerbosity";
  TurnOnLogMetrics = true;
  TurnOnLogging = true;
  defaultBackends = [ "KatipBK" ];
  defaultScribes = [
    [ "StdoutSK" "stdout" ]
  ];
  hasEKG = 12788;
  hasPrometheus = [ "127.0.0.1" 12798 ];
  minSeverity = "Info";
  options = {
    mapBackends = {
      "cardano.node.metrics" = [ "EKGViewBK" ];
      "cardano.node.resources" = [ "EKGViewBK" ];
    };
    mapSubtrace = {
      "cardano.node.metrics" = { subtrace = "Neutral"; };
    };
  };
  rotation = {
    rpKeepFilesNum = 10;
    rpLogLimitBytes = 5000000;
    rpMaxAgeHours = 24;
  };
  setupBackends = [ "KatipBK" ];
  setupScribes = [
    {
      scFormat = "ScText";
      scKind = "StdoutSK";
      scName = "stdout";
      scRotation = null;
    }
  ];
}
