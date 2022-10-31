# Marlowe Transaction Service

The `marlowe-tx` executable provides services related to building and submitting transactions for Marlowe contracts.

```console
marlowe-tx : the transaction creation server of the Marlowe Runtime

Usage: marlowe-tx [--chain-seek-port-number PORT_NUMBER] 
                  [--chain-seek-query-port-number PORT_NUMBER] 
                  [--chain-seek-command-port-number PORT_NUMBER] 
                  [--chain-seek-host HOST_NAME] [--command-port PORT_NUMBER] 
                  [-h|--host HOST_NAME] [--history-sync-port PORT_NUMBER] 
                  [--history-host HOST_NAME] [--log-level LOG_LEVEL | --silent]

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
  --history-sync-port PORT_NUMBER
                           The port number of the history sync server.
                           (default: 3719)
  --history-host HOST_NAME The host name of the history server.
                           (default: "127.0.0.1")
  --log-level LOG_LEVEL    Log everything up including the given level:
                           [DEBUG|INFO|WARNING|ERROR]
  --silent                 Suppress all logs.
```