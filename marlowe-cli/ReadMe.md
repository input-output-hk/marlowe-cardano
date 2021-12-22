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
      export-marlowe           Export a Marlowe contract to a JSON file.
      export-address           Print a validator address.
      export-validator         Export a validator to a JSON file.
      export-datum             Export a datum to a JSON file.
      export-redeemer          Export a redeemer to a JSON file.
      transaction-simple       Build a non-Marlowe transaction.
      transaction-create       Build a transaction that pays to a Marlowe script.
      transaction-advance      Build a transaction that both spends from and pays to a Marlowe script.
      transaction-close        Build a transaction that spends from a Marlowe script.
      transaction-submit       Submit a transaction body.
      input-deposit            Create Marlowe input for a deposit.
      input-choose             Create Marlowe input for a choice.
      input-notify             Create Marlowe input for a notification.
      compute                  Compute a Marlowe contract and write the output to a JSON file.

Individual help pages:

*   Creating validator, datum, and redeemer.
    *   [`marlowe-cli export-marlowe`](doc/export-marlowe.md)
    *   [`marlowe-cli export-address`](doc/export-address.md)
    *   [`marlowe-cli export-validator`](doc/export-validator.md)
    *   [`marlowe-cli export-datum`](doc/export-datum.md)
    *   [`marlowe-cli export-redeemer`](doc/export-redeemer.md)
*   Building transactions.
    *   [`marlowe-cli transaction-simple`](doc/transaction-simple.md)
    *   [`marlowe-cli transaction-create`](doc/transaction-create.md)
    *   [`marlowe-cli transaction-advance`](doc/transaction-advance.md)
    *   [`marlowe-cli transaction-close`](doc/transaction-close.md)
    *   [`marlowe-cli transaction-submit`](doc/transaction-submit.md)
*   Submitting transactions.
    *   [`input-deposit`](doc/input-deposit.md)
    *   [`input-choose`](doc/input-choose.md)
    *   [`input-notify`](doc/input-notify.md)
*   Computing steps of Marlowe contracts.
    *   [`compute`](doc/compute.md)


## Installation

Either install via using Nix and Cabal:

    git clone https://github.com/input-output-hk/marlowe-cardano.git
    nix-shell
    cabal install exe:marlowe-cli

or just using Cabal, if Cabal and GHC are already installed:

    git clone https://github.com/input-output-hk/marlowe-cardano.git
    cabal install exe:marlowe-cli


## Workflows for Building Marlowe Scripts, Datums, and Redeemers

The `marlowe-cli` tools supports both granular and monolithic workflows for creating the files and hashes needed to submit Marlowe contracts with `cardano-cli`.


### Monolithic Workflow

The [`export-marlowe`](doc/export.md) command writes a JSON file with sufficient information to run the contract on the blockchain. It contains the following information.

*   Script address
*   Validator hash
*   Datum hash
*   CBOR for Plutus script
*   JSON and CBOR for datum.
*   JSON and CBOR for redeemer.
*   Size of the above CBOR in bytes
*   Execution cost

The diagram below illusrates how the `export` command can be used in conjunction with [`jq`](https://stedolan.github.io/jq/manual/) and `cardano-cli`.

![Marlowe workflow using `marlowe-cli`, `jq`, and `cardano-cli`.](doc/diagrams/workflow-jq.svg)

See [example.marlowe](doc/example.marlowe) for an example file containing this information for a simple contract, and see [example-jq.sh](doc/example-jq.sh) for an example bash script embodying this workflow. A tutorial for this workflow is available [here](doc/tutorial-jq.md).


### Granular Workflow

The [`address`](doc/address.md), [`validator`](doc/validator.md), [`datum`](doc/datum.md), and [`redeemer`](doc/redeemer.md) commands write the corresponding subset of information to a JSON file or to the console. These allows finer-grain access to the capabilities of the `export` command. The diagram below illustrates how these commands can be used in conjunction with `cardano-cli`.

![Marlowe workflow using `marlowe-cli` and `cardano-cli`.](doc/diagrams/workflow.svg)

See [example.sh](doc/example.sh) for an example bash script embodying this workflow. A tutorial for this workflow is available [here](doc/tutorial.md).


## Examples

*   [simple contract](examples/simple/ReadMe.md)
*   [escrow](examples/escrow/ReadMe.md)
*   [swap](examples/swap/ReadMe.md)


## Testing

*   [simple contract](examples/simple/run-test.sh)
*   escrow
    *   ["everything is alright"](examples/escrow/run-everything-is-alright.sh)
    *   ["confirm problem"](examples/escrow/run-confirm-problem.sh)
    *   ["dismiss claim"](examples/escrow/run-dimiss-claim.sh)
    *   ["confirm claim"](examples/escrow/run-confirm-claim.sh)
*   [swap](examples/swap/run-swap.sh)
