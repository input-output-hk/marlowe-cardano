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


## Workflows

The `marlowe-cli` tools supports both granular and monolithic workflows for creating the files and hashes needed to submit Marlowe contracts with `cardano-cli`.


### Monolithic Workflow

The [`export`](export.md) command writes a JSON file with sufficient information to run the contract on the blockchain. It contains the following information.

*   Address
*   Validator hash
*   CBOR for its Plutus script
*   Size in bytes
*   Execution cost

The diagram below illusrates how the `export` command can be used in conjunction with [`jq`](https://stedolan.github.io/jq/manual/) and `cardano-cli`.

![Marlowe workflow using `marlowe-cli`, `jq`, and `cardano-cli`.](workflow-jq.svg)

See [example](example.marlowe) for an example file containing this information for a simple contract, and see [example-jq.sh](example-jq.sh) for an example bash script embodying this workflow.

```diff
- FIXME: Write a narrative tutorial for this example.
```


### Granular Workflow

The [`address`](address.md), [`validator`](validator.md), [`datum`](datum.md), and [`redeemer`](redeemer.md) commands write the corresponding subset of information to a JSON file or to the console. These allows finer-grain access to the capabilities of the `export` command. The diagram below illustrates how these commands can be used in conjunction with `cardano-cli`.

![Marlowe workflow using `marlowe-cli` and `cardano-cli`.](workflow.svg)

See [example.sh](example.sh) for an example bash script embodying this workflow.

```diff
- FIXME: Write a narrative tutorial for this example.
```
