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
      contract                 Export contract address, validator, datum, or redeemer. [low-level]
      input                    Create inputs to a contract.
      role                     Export role address, validator, datum, or redeemer.  [low-level]
      run                      Run a contract.
      template                 Create a contract from a template.
      transaction              Create and submit transactions. [low-level]

Help for subcommands:

* [`marlowe-cli contract`](#contract-commands)
* [`marlowe-cli input`](#input-commands)
* [`marlowe-cli role`](#role-commands)
* [`marlowe-cli template`](#template-commands)
* [`marlowe-cli transaction`](#transaction-commands)


### "Contract" Commands

    $ marlowe-cli contract --help
    
    Usage: marlowe-cli contract COMMAND
      Export contract address, validator, datum, or redeemer. [low-level]
    
    Available options:
      -h,--help                Show this help text
    
    Low-level commands for exporting Marlowe contract information:
      address                  Print a contract validator address.
      datum                    Export a contract datum to a JSON file.
      marlowe                  Export a Marlowe contract to a JSON file.
      redeemer                 Export a contract redeemer to a JSON file.
      validator                Export a contract validator to a JSON file.

Individual help pages:

*   [`marlowe-cli contract marlowe`](doc/export-marlowe.md)
*   [`marlowe-cli contract address`](doc/export-address.md)
*   [`marlowe-cli contract validator`](doc/export-validator.md)
*   [`marlowe-cli contract datum`](doc/export-datum.md)
*   [`marlowe-cli contract redeemer`](doc/export-redeemer.md)


### "Input" Commands

    $ marlowe-cli input --help
    
    Usage: marlowe-cli input COMMAND
      Create inputs to a contract.
    
    Available options:
      -h,--help                Show this help text
    
    Commands for creating inputs to a contract:
      choose                   Create Marlowe input for a choice.
      deposit                  Create Marlowe input for a deposit.
      notify                   Create Marlowe input for a notification.

Individual help pages:

*   [`marlowe-cli input deposit`](doc/input-deposit.md)
*   [`marlowe-cli input choose`](doc/input-choose.md)
*   [`marlowe-cli input notify`](doc/input-notify.md)


### "Role" Commands

    $ marlowe-cli role --help
    
    Usage: marlowe-cli role COMMAND
      Export role address, validator, datum, or redeemer. [low-level]
    
    Available options:
      -h,--help                Show this help text
    
    Low-level commands for exporting Marlowe role information:
      address                  Print a role validator address.
      datum                    Export a role datum to a JSON file.
      redeemer                 Export a role redeemer to a JSON file.
      validator                Export a role validator to a JSON file.


### "Run" Commands

    $ marlowe-cli run --help
    
    Usage: marlowe-cli run COMMAND
      Run a contract.
    
    Available options:
      -h,--help                Show this help text
    
    Commands for running contracts:
      compute                  Compute the next step of a Marlowe contract and write the output to a JSON file.

Individual help pages:

*   [`marlowe-cli run compute`](doc/compute.md)


### "Template" Commands

    $ marlowe-cli template --help
    
    Usage: marlowe-cli template COMMAND
      Create a contract from a template.
    
    Available options:
      -h,--help                Show this help text
    
    Commands for creating Marlowe contracts from templates:
      escrow                   Create an escrow contract.
      simple                   Create a simple example contract.
      swap                     Create a swap contract.
      zcb                      Create a zero-coupon bond.


### "Transaction" Commands

    $ marlowe-cli transaction --help
    
    Usage: marlowe-cli transaction COMMAND
      Create and submit transactions. [low-level]
    
    Available options:
      -h,--help                Show this help text
    
    Low-level commands for creating and submitting transactions:
      advance                  Build a transaction that both spends from and pays to
                               a Marlowe script.
      close                    Build a transaction that spends from a Marlowe
                               script.
      create                   Build a transaction that pays to a Marlowe script.
      simple                   Build a non-Marlowe transaction.
      submit                   Submit a transaction body.

Individual help pages:

*   [`marlowe-cli transaction simple`](doc/transaction-simple.md)
*   [`marlowe-cli transaction create`](doc/transaction-create.md)
*   [`marlowe-cli transaction advance`](doc/transaction-advance.md)
*   [`marlowe-cli transaction close`](doc/transaction-close.md)
*   [`marlowe-cli transaction submit`](doc/transaction-submit.md)


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
