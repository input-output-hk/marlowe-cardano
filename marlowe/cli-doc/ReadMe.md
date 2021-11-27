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
      transact                 Build a non-Marlowe transaction.
      create                   Build a transaction that pays to a Marlowe script.
      advance                  Build a transaction that both spends from and pays to a Marlowe script.
      close                    Build a transaction that spends from a Marlowe script.
      submit                   Submit a transaction body.
      example                  Hardwired example.


Individual help pages:

*   Creating validator, datum, and redeemer.
    *   [`marlowe-cli export`](export.md)
    *   [`marlowe-cli address`](address.md)
    *   [`marlowe-cli validator`](validator.md)
    *   [`marlowe-cli datum`](datum.md)
    *   [`marlowe-cli redeemer`](redeemer.md)
*   Building transactions.
    *   [`marlowe-cli transact`](transact.md)
    *   [`marlowe-cli create`](create.md)
    *   [`marlowe-cli advance`](advance.md)
    *   [`marlowe-cli close`](close.md)
*   Submitting transactions.
    *   [`marlowe-cli submit`](submit.md)


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

An extended example showing the execution of a three-step contract is available in [extended-example.sh](extended-example.sh). It runs the following contract:

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
