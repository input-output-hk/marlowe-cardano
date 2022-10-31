# Marlowe Discovery Service

The `marlowe-discovery` executable provides services for discoverying the on-chain presence of Marlowe contracts.

```console
marlowe-discovery : a contract discovery service for the Marlowe Runtime.

Usage: marlowe-discovery [--chain-seek-port-number PORT_NUMBER] 
                         [--chain-seek-query-port-number PORT_NUMBER] 
                         [--query-port PORT_NUMBER] [--sync-port PORT_NUMBER] 
                         [--chain-seek-host HOST_NAME] [-h|--host HOST_NAME]

  Contract discovery service for Marlowe Runtime

Available options:
  -h,--help                Show this help text
  --chain-seek-port-number PORT_NUMBER
                           The port number of the chain seek server.
                           (default: 3715)
  --chain-seek-query-port-number PORT_NUMBER
                           The port number of the chain sync query server.
                           (default: 3716)
  --query-port PORT_NUMBER The port number to run the query server on.
                           (default: 3721)
  --sync-port PORT_NUMBER  The port number to run the sync server on.
                           (default: 3722)
  --chain-seek-host HOST_NAME
                           The host name of the chain seek server.
                           (default: "127.0.0.1")
  -h,--host HOST_NAME      The host name to run the discovery server on.
                           (default: "127.0.0.1")
```