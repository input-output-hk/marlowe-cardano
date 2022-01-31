{
  AlonzoGenesisFile = "./alonzo-genesis.json";
  ByronGenesisFile = "./byron-genesis.json";
  ShelleyGenesisFile = "./shelly-genesis.json";
  AlonzoGenesisHash = "1cd3e49429bdb386852fba733515ae35112415ed7bf93799688da371068d4e0a";
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
  ByronGenesisHash = "0cb403802a38d5ca4609517e4837c81b42f4760015d450fd33193e8c8b76c579";
  LastKnownBlockVersion-Alt = 0;
  LastKnownBlockVersion-Major = 5;
  LastKnownBlockVersion-Minor = 1;
  MaxConcurrencyDeadline = 4;
  MaxKnownMajorProtocolVersion = 5;
  PBftSignatureThreshold = 1.1;
  Protocol = "Cardano";
  RequiresNetworkMagic = "RequiresMagic";
  ShelleyGenesisHash = "1e1a73e3a833add2dc7139628a2cc79d41c651e911fd53dfa71e1906d2406802";
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
