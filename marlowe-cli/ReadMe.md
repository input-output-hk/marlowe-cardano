---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

# Marlowe Command-Line Interface (CLI) Tool

The `marlowe-cli` tool provides several utilities for serialising
Marlowe contracts to validators, datums, and redeemers. It also computes
hashes and addresses. It can be used in conjunction with
[`cardano-cli`](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
to submit Marlowe transactions to the Cardano blockchain.

See the [Marlowe Debugging Cookbook](../marlowe/debugging-cookbook.md)
for troubleshooting information, or the [Marlowe CLI Pioneers
Lectures](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/lectures/ReadMe.md).

</div>

<div class="cell markdown">

## Contents

-   [Installation](#installation)
-   [Available commands](#available-commands)
-   [Workflows](#workflows)
    -   [High-level workflow](#high-level-workflow)
        -   [Examples](#examples)
        -   [Test cases](#test-cases)
    -   [Low-level workflow](#low-level-workflow)
    -   [Backend workflow](#backend-workflow)
-   [Automated Tests](#automated-tests)
-   [Editing and rebuilding this
    documentation](#editing-and-rebuilding-this-documentation)

</div>

<div class="cell markdown">

## Installation

One can install `marlowe-cli` either using Nix or Cabal. Detailed
instructions are available
[here](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc).

</div>

<div class="cell markdown">

### Installation via Nix

NixOS and the Nix package manager are available at
\<<https://nixos.org/>\>.

👉 See
\<<https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc#how-to-set-up-the-iohk-binary-caches>\>
on how to set up binary caches for the Nix build. This will greatly
speed the build process.

Once the binary caches have been set up, clone the Marlowe repository
and simply enter a nix shell.

    git clone https://github.com/input-output-hk/marlowe-cardano.git
    cd marlowe-cardano
    nix develop

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli --version
```

<div class="output stream stdout">

    marlowe-cli 0.0.8.0

</div>

</div>

<div class="cell markdown">

### Installation via Cabal

Cabal and GHC are available at [GHCup](https://www.haskell.org/ghcup/).

Installing directly via `cabal` and `ghc` involves lengthy compilation,
but avoids the use of Nix. First ensure that Cabal 3.4 and GHC 8.10.7
are installed.

</div>

<div class="cell code" execution_count="2">

``` bash
cabal --version
```

<div class="output stream stdout">

    cabal-install version 3.4.0.0
    compiled using version 3.4.1.0 of the Cabal library 

</div>

</div>

<div class="cell code" execution_count="3">

``` bash
ghc --version
```

<div class="output stream stdout">

    The Glorious Glasgow Haskell Compilation System, version 8.10.7

</div>

</div>

<div class="cell markdown">

Clone the Marlowe repository and execute `cabal`:

    git clone https://github.com/input-output-hk/marlowe-cardano.git
    cd marlowe-cardano
    cabal install exe:marlowe-cli

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="4">

``` bash
marlowe-cli --help
```

<div class="output stream stdout">

    marlowe-cli : a command-line tool for Marlowe contracts

    Usage: marlowe-cli [--version] 
                       (COMMAND | COMMAND | [--alonzo-era] (COMMAND | COMMAND) | 
                         --babbage-era (COMMAND | COMMAND))

      Utilities for Marlowe.

    Available options:
      -h,--help                Show this help text
      --version                Show version.
      --alonzo-era             Read and write Alonzo transactions
      --babbage-era            Read and write Babbage transactions

    High-level commands:
      run                      Run a contract.
      template                 Create a contract from a template.
      test                     Test contracts.

    Low-level commands:
      contract                 Export contract address, validator, datum, or
                               redeemer.
      input                    Create inputs to a contract.
      role                     Export role address, validator, datum, or redeemer.
      transaction              Create and submit transactions.
      util                     Miscellaneous utilities.

</div>

</div>

<div class="cell markdown">

Further help is available for each subcommand:

-   high-level commands
    -   [`marlowe-cli run`](doc/run.md)
    -   [`marlowe-cli template`](doc/template.md)
    -   [`marlowe-cli test`](doc/test.md)
-   low-level commands
    -   [`marlowe-cli contract`](doc/contract.md)
    -   [`marlowe-cli input`](doc/input.md)
    -   [`marlowe-cli role`](doc/role.md)
    -   [`marlowe-cli transaction`](doc/transaction.md)
    -   [`marlowe-cli util`](doc/util.md)

</div>

<div class="cell markdown">

## Workflows

Marlowe CLI supports workflows for specific use cases:

-   [A high-level workflow](#high-level-workflow) for interacting with
    Marlowe contracts without dealing with the underlying its Plutus
    mechanics.
-   [A low-level workflow](#low-level-workflow) that exposes the Plutus
    mechanics of Marlowe contracts.

</div>

<div class="cell markdown" tags="[]">

### High-Level Workflow

In the high-level workflow for `marlowe-cli`, the user creates a
contract from a template, using Marlowe Playground, programmatically, or
by hand. The user provides input at each step of contract execution. The
tool manages the contract state transitions and handles the construction
and submission of transactions.

![High-level Marlowe CLI
workflow.](doc/diagrams/high-level-workflow.svg)

</div>

<div class="cell markdown" jp-MarkdownHeadingCollapsed="true" tags="[]">

#### Examples

-   [simple contract](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/simple/ReadMe.md)
-   [escrow](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/escrow/ReadMe.md)
-   [swap](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/swap/ReadMe.md)
-   [zero-coupon bond](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/zcb/ReadMe.md)
-   [contract for differences](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/cfd/ReadMe.md)
-   [covered call](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/coveredCall/ReadMe.md)

</div>

<div class="cell markdown">

#### Test Cases

-   [simple contract](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/simple/run-test.sh)
-   escrow
    -   ["everything is
        alright"](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/xamples/escrow/run-everything-is-alright.sh)
    -   ["confirm problem"](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/escrow/run-confirm-problem.sh)
    -   ["dismiss claim"](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/escrow/run-dimiss-claim.sh)
    -   ["confirm claim"](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/escrow/run-confirm-claim.sh)
-   [swap](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/swap/run-swap.sh)
-   [zero-coupon bond](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/zcb/run-zcb.sh)
-   [contract for differences](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/cfd/run-cfd.sh)
-   [covered call](https://github.com/input-output-hk/real-world-marlowe/tree/main/archives/marlowe-cli/examples/coveredCall/run-coveredCall.sh)

</div>

<div class="cell markdown">

### Low-Level Workflow

The `marlowe-cli` tools supports both granular and monolithic workflows
for creating the files and hashes needed to submit Marlowe contracts
with `cardano-cli`. The workflows only differ in how information is
packaged.

</div>

<div class="cell markdown">

#### Monolithic Workflow

The [`export-marlowe`](doc/export.md) command writes a JSON file with
sufficient information to run the contract on the blockchain. It
contains the following information.

-   Script address
-   Validator hash
-   Datum hash
-   CBOR for Plutus script
-   JSON and CBOR for datum.
-   JSON and CBOR for redeemer.
-   Size of the above CBOR in bytes
-   Execution cost

The diagram below illusrates how the `export` command can be used in
conjunction with [`jq`](https://stedolan.github.io/jq/manual/) and
`cardano-cli`.

![Marlowe workflow using `marlowe-cli`, `jq`, and
`cardano-cli`.](doc/diagrams/monolithic.svg)

See [monolithic.marlowe](doc/monolithic.marlowe) for an example file
containing this information for a simple contract. A tutorial for this
workflow is available [here](doc/monolithic.md).

</div>

<div class="cell markdown" tags="[]">

### Granular Workflow

The [`contract address`](doc/contract.md#address),
[`contract validator`](doc/contract.md#validator),
[`contract datum`](doc/contract.md#datum), and
[`contract redeemer`](doc/contract.md#redeemer) commands write the
corresponding subset of information to a JSON file or to the console.
These allows finer-grain access to the capabilities of the
`contract marlowe` command. The diagram below illustrates how these
commands can be used in conjunction with `cardano-cli`.

![Marlowe workflow using `marlowe-cli` and
`cardano-cli`.](doc/diagrams/granular.svg)

A tutorial for this workflow is available [here](doc/granular.md).

</div>

<div class="cell markdown">

## Automated Tests

</div>

<div class="cell markdown">

-   Tests that interact directly with the Cardano blockchain:
    [run-nonpab-tests.sh](run-nonpab-tests.sh)

</div>

<div class="cell markdown">

## Editing and Rebuilding This Documentation

Most of this documentation is edited in Jupyter notebooks, execute
`nix develop --command jupyter-lab` to launch Jupyter.

Execute `make` or `./Makefile` to rebuild this documentation.

</div>
