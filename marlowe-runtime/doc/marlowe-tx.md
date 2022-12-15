# Marlowe Transaction Service

The `marlowe-tx` executable provides services related to building and submitting transactions for Marlowe contracts.

```console
marlowe-tx : the transaction creation server of the Marlowe Runtime

Usage: marlowe-tx [--chain-seek-port-number PORT_NUMBER] 
                  [--chain-seek-query-port-number PORT_NUMBER] 
                  [--chain-seek-command-port-number PORT_NUMBER] 
                  [--chain-seek-host HOST_NAME] [--command-port PORT_NUMBER] 
                  [-h|--host HOST_NAME]

  Marlowe runtime transaction creation server

Available options:
  -h,--help                Show this help text
  --chain-seek-port-number PORT_NUMBER
                           The port number of the chain seek server.
                           (default: 3715)
  --chain-seek-query-port-number PORT_NUMBER
                           The port number of the chain sync query server.
                           (default: 3716)
  --chain-seek-command-port-number PORT_NUMBER
                           The port number of the chain sync job server.
                           (default: 3720)
  --chain-seek-host HOST_NAME
                           The host name of the chain seek server.
                           (default: "127.0.0.1")
  --command-port PORT_NUMBER
                           The port number to run the job server on.
                           (default: 3723)
  -h,--host HOST_NAME      The host name to run the tx server on.
                           (default: "127.0.0.1")
```
