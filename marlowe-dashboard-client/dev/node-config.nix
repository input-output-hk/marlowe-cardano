config: config // {
  AlonzoGenesisFile = "alonzo-genesis.json";
  ByronGenesisFile = "byron-genesis.json";
  ShelleyGenesisFile = "shelley-genesis.json";
  TraceDNSResolver = false;
  TraceDNSSubscription = false;
  TraceIpSubscription = false;
  TraceMempool = true;
  TracePeerSelection = false;
  TracePeerSelectionActions = false;
  TracePublicRootPeers = false;
  defaultScribes = [
    [ "StdoutSK" "stdout" ]
  ];
  minSeverity = "Info";
  setupScribes = [
    {
      scFormat = "ScText";
      scKind = "StdoutSK";
      scName = "stdout";
      scRotation = null;
    }
  ];
}
