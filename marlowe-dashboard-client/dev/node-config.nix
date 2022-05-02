{ AlonzoGenesisFile ? config.AlonzoGenesisFile
, ByronGenesisFile ? config.ByronGenesisFile
, ShelleyGenesisFile ? config.ShelleyGenesisFile
, config
}: config // {
  inherit AlonzoGenesisFile ByronGenesisFile ShelleyGenesisFile;
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
