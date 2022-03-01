let
  bitte-config = builtins.fromJSON (builtins.readFile ../../bitte/node/config/config.json);
  overrides = {
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
  };
in
bitte-config // overrides
