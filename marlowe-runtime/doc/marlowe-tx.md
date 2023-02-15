# Marlowe Transaction Service

The `marlowe-tx` executable provides services related to building and submitting transactions for Marlowe contracts.

```console
marlowe-tx : the transaction creation server of the Marlowe Runtime

Usage: marlowe-tx [--chain-sync-port PORT_NUMBER] 
                  [--chain-sync-query-port PORT_NUMBER] 
                  [--chain-sync-command-port PORT_NUMBER] 
                  [--chain-sync-host HOST_NAME] [--command-port PORT_NUMBER] 
                  [-h|--host HOST_NAME]

  Marlowe runtime transaction creation server

Available options:
  -h,--help                Show this help text
  --chain-sync-port PORT_NUMBER
                           The port number of the chain sync server.
                           (default: 3715)
  --chain-sync-query-port PORT_NUMBER
                           The port number of the chain sync query server.
                           (default: 3716)
  --chain-sync-command-port PORT_NUMBER
                           The port number of the chain sync job server.
                           (default: 3720)
  --chain-sync-host HOST_NAME
                           The host name of the chain sync server.
                           (default: "127.0.0.1")
  --command-port PORT_NUMBER
                           The port number to run the job server on.
                           (default: 3723)
  -h,--host HOST_NAME      The host name to run the tx server on.
                           (default: "127.0.0.1")
```
