# Chain Indexer Daemon

The `marlowe-chain-indexer` executable follows a local blockchain node and writes the blocks and transactions to a database.

```console
marlowe-chain-indexer : a chain indexer for the Marlowe Runtime.

Usage: marlowe-chain-indexer [--version] (-s|--socket-path SOCKET_FILE) 
                  [-m|--testnet-magic INTEGER] (-d|--database-uri DATABASE_URI)
                  --genesis-config-file-hash CONFIG_HASH
                  --genesis-config-file CONFIG_FILE [--block-cost COST_UNITS]
                  --shelley-genesis-config-file CONFIG_FILE
                  [--tx-cost COST_UNITS] [--max-cost COST_UNITS]

  Chain indexer for Marlowe Runtime.

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
  --shelley-genesis-config-file CONFIG_FILE
                           Path to the Shelley Genesis Config JSON File.
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
