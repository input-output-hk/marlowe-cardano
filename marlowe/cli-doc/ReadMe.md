# Marlowe CLI Tool

The `marlowe-cli` tool provides several utilities for serialising Marlowe contracts to validators, datums, and redeemers. It also computes hashes and addresses. It can be used in conjunction with [`cardano-cli`](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md) to submit Marlowe transactions to the Cardano blockchain.


## Available Commands

    $ marlowe-cli --help
    
    Usage: marlowe-cli [--version] COMMAND
      Utilities for Marlowe.
    
    Available options:
      -h,--help                Show this help text
      --version                Show version.
    
    Available commands:
      export                   Export a Marlowe contract to a JSON file.
      address                  Print a validator address.
      validator                Export a validator to a JSON file.
      datum                    Export a datum to a JSON file.
      redeemer                 Export a redeemer to a JSON file.

The [`export`](export.md) command writes a JSON file with sufficient information to run the contract on the blockchain.

*   Address
*   Validator hash
*   CBOR for its Plutus script
*   Size in bytes
*   Execution cost

The [`address`](address.md), [`validator`](validator.md), [`datum`](datum.md), and [`redeemer`](redeemer.md) commands write the corresponding subset of information to a JSON file or to the console.


## Running Marlowe on Testnet


### Using the `export` command and the `jq` tool

See [example-jq.sh](example-jq.sh).

```diff
- FIXME: Write a narrative tutorial for this example.
```


### Using the `address`, `validator`, `datum`, and `redeemer` commands

See [example.sh](example.sh).

```diff
- FIXME: Write a narrative tutorial for this example.
```
