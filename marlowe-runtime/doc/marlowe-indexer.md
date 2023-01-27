# Marlowe Indexer Daemon

The `marlowe-indexer` executable follows `chainseekd` and writes Marlowe contract transactions to a database.

```console
marlowe-indexer : a contract indexing service for the Marlowe Runtime.

Usage: marlowe-indexer [--chain-seek-port-number PORT_NUMBER] 
                       [--chain-seek-query-port-number PORT_NUMBER] 
                       [--chain-seek-host HOST_NAME]
                       (-d|--database-uri DATABASE_URI) 
                       [--log-config-file FILE_PATH]

  Contract indexing service for Marlowe Runtime

Available options:
  -h,--help                Show this help text
  --chain-seek-port-number PORT_NUMBER
                           The port number of the chain seek server.
                           (default: 3715)
  --chain-seek-query-port-number PORT_NUMBER
                           The port number of the chain sync query server.
                           (default: 3716)
  --chain-seek-host HOST_NAME
                           The host name of the chain seek server.
                           (default: "127.0.0.1")
  -d,--database-uri DATABASE_URI
                           URI of the database where the contract information is
                           saved.
  --log-config-file FILE_PATH
                           The logging configuration JSON file.

```
