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
    *   [`marlowe-cli export-marlowe`](export-marlowe.md)
    *   [`marlowe-cli export-address`](export-address.md)
    *   [`marlowe-cli export-validator`](export-validator.md)
    *   [`marlowe-cli export-datum`](export-datum.md)
    *   [`marlowe-cli export-redeemer`](export-redeemer.md)
*   Building transactions.
    *   [`marlowe-cli transaction-simple`](transaction-simple.md)
    *   [`marlowe-cli transaction-create`](transaction-create.md)
    *   [`marlowe-cli transaction-advance`](transaction-advance.md)
    *   [`marlowe-cli transaction-close`](transaction-close.md)
    *   [`marlowe-cli transaction-submit`](transaction-submit.md)
*   Submitting transactions.
    *   [`input-deposit`](input-deposit.md)
    *   [`input-choose`](input-choose.md)
    *   [`input-notify`](input-notify.md)
*   Computing steps of Marlowe contracts.
    *   [`compute`](compute.md)


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

The [`export`](export.md) command writes a JSON file with sufficient information to run the contract on the blockchain. It contains the following information.

*   Script address
*   Validator hash
*   Datum hash
*   CBOR for Plutus script
*   JSON and CBOR for datum.
*   JSON and CBOR for redeemer.
*   Size of the above CBOR in bytes
*   Execution cost

The diagram below illusrates how the `export` command can be used in conjunction with [`jq`](https://stedolan.github.io/jq/manual/) and `cardano-cli`.

![Marlowe workflow using `marlowe-cli`, `jq`, and `cardano-cli`.](diagrams/workflow-jq.svg)

See [example.marlowe](example.marlowe) for an example file containing this information for a simple contract, and see [example-jq.sh](example-jq.sh) for an example bash script embodying this workflow. A tutorial for this workflow is available [here](tutorial-jq.md).


### Granular Workflow

The [`address`](address.md), [`validator`](validator.md), [`datum`](datum.md), and [`redeemer`](redeemer.md) commands write the corresponding subset of information to a JSON file or to the console. These allows finer-grain access to the capabilities of the `export` command. The diagram below illustrates how these commands can be used in conjunction with `cardano-cli`.

![Marlowe workflow using `marlowe-cli` and `cardano-cli`.](diagrams/workflow.svg)

See [example.sh](example.sh) for an example bash script embodying this workflow. A tutorial for this workflow is available [here](tutorial.md).


## Extended Example

An extended example showing the execution of a three-step contract is available in in a [tutorial](extended-tutorial.md) and in the script [extended-example.sh](extended-example.sh). It runs the following contract:

<table>
<tr>
<td>
<ol>
<li value="0">The contract is initially funded with 3 ADA.
<li value="1">The sole party then deposits 10 ADA.
<li value="2">The contract then permits 5 ADA to be payed back to the party.
<li value="3">Finally, the contract closes and the remaining 8 ADA is withdrawn.
</ol>
<td>
<td rowspan="2">
<img alt="A Three-Step Marlowe Contract" src="extended-example.png"/>
</td>
</tr>
<tr>
<td>
<pre>
When
    [Case
        (Deposit
            (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07")
            (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07")
            (Token "" "")
            (Constant 10)
        )
        (Pay
            (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07")
            (Account (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07"))
            (Token "" "")
            (Constant 5)
            Close
        )]
    45000000 Close
</pre>
</td>
</tr>
</table>


## Testing

A automated test script is available at [simple-test.sh](simple-test.sh).
