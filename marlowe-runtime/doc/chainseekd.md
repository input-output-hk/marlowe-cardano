# Chain Seek Daemon

The `chainseekd` executable provides services for querying the blockchain for information that may relate to Marlowe contracts.

```console
chainseekd : a chain seek server for the Marlowe Runtime.

Usage: chainseekd [--version] (-s|--socket-path SOCKET_FILE) 
                  [-m|--testnet-magic INTEGER] (-d|--database-uri DATABASE_URI)
                  --genesis-config-file-hash CONFIG_HASH
                  --genesis-config-file CONFIG_FILE [-h|--host HOST_NAME] 
                  [--port-number PORT_NUMBER] [--query-port-number PORT_NUMBER] 
                  [--job-port-number PORT_NUMBER] [--block-cost COST_UNITS] 
                  [--tx-cost COST_UNITS] [--max-cost COST_UNITS]

  Chain seek server for Marlowe Runtime.

Available options:
  -h,--help                Show this help text
  --version                Show version.
  -s,--socket-path SOCKET_FILE
                           Location of the cardano-node socket file. Defaults to
                           the CARDANO_NODE_SOCKET_PATH environment variable.
  -m,--testnet-magic INTEGER
                           Testnet network ID magic. Defaults to the
                           CARDANO_TESTNET_MAGIC environment variable.
  -d,--database-uri DATABASE_URI
                           URI of the database where the chain information is
                           saved.
  --genesis-config-file-hash CONFIG_HASH
                           Hash of the Byron Genesis Config JSON file.
  --genesis-config-file CONFIG_FILE
                           Path to the Byron Genesis Config JSON File.
  -h,--host HOST_NAME      The hostname to serve the chain seek protocol on.
                           (default: "127.0.0.1")
  --port-number PORT_NUMBER
                           The port number to serve the chain seek protocol on.
                           (default: 3715)
  --query-port-number PORT_NUMBER
                           The port number to serve the query protocol on.
                           (default: 3716)
  --job-port-number PORT_NUMBER
                           The port number to serve the job protocol on.
                           (default: 3720)
  --block-cost COST_UNITS  The number of cost units to associate with persisting
                           a block when computing the cost model. (default: 1)
  --tx-cost COST_UNITS     The number of cost units to associate with persisting
                           a transaction when computing the cost model.
                           (default: 10)
  --max-cost COST_UNITS    The maximum number of cost units that can be batched
                           when persisting blocks. If the cost of the current
                           batch would exceed this value, the chain sync client
                           will wait until the current batch is persisted before
                           requesting another block. (default: 100000)
```