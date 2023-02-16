# Marlowe History Service

The `marlowe-history` executable provides services for querying the on-chain history of Marlowe contracts.

```console
marlowe-history : a contract history service for the Marlowe Runtime.

Usage: marlowe-history [--chain-sync-port PORT_NUMBER] 
                       [--chain-sync-query-port PORT_NUMBER] 
                       [--command-port PORT_NUMBER] [--query-port PORT_NUMBER] 
                       [--sync-port PORT_NUMBER] [--chain-sync-host HOST_NAME] 
                       [-h|--host HOST_NAME]

  Contract history service for Marlowe Runtime

Available options:
  -h,--help                Show this help text
  --chain-sync-port PORT_NUMBER
                           The port number of the chain sync server.
                           (default: 3715)
  --chain-sync-query-port PORT_NUMBER
                           The port number of the chain sync query server.
                           (default: 3716)
  --command-port PORT_NUMBER
                           The port number to run the job server on.
                           (default: 3717)
  --query-port PORT_NUMBER The port number to run the query server on.
                           (default: 3718)
  --sync-port PORT_NUMBER  The port number to run the sync server on.
                           (default: 3719)
  --chain-sync-host HOST_NAME
                           The host name of the chain sync server.
                           (default: "127.0.0.1")
  -h,--host HOST_NAME      The host name to run the history server on.
                           (default: "127.0.0.1")
```