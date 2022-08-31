---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe Command-Line Interface (CLI) Tool

The `marlowe-cli` tool provides several utilities for serialising
Marlowe contracts to validators, datums, and redeemers. It also computes
hashes and addresses. It can be used in conjunction with
[`cardano-cli`](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
to submit Marlowe transactions to the Cardano blockchain.

See the [Marlowe Debugging Cookbook](../marlowe/debugging-cookbook.md)
for troubleshooting information, or the [Marlowe CLI Pioneers
Lectures](lectures/ReadMe.md).

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

## Installation

One can install `marlowe-cli` either using Nix or Cabal. Detailed
instructions are available
[here](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc).

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
    nix-shell

``` bash
marlowe-cli --version
```

    marlowe-cli 0.0.5.0

### Installation via Cabal

Cabal and GHC are available at [GHCup](https://www.haskell.org/ghcup/).

Installing directly via `cabal` and `ghc` involves lengthy compilation,
but avoids the use of Nix. First ensure that Cabal 3.4 and GHC 8.10.7
are installed.

``` bash
cabal --version
```

    cabal-install version 3.4.0.0
    compiled using version 3.4.1.0 of the Cabal library 

``` bash
ghc --version
```

    The Glorious Glasgow Haskell Compilation System, version 8.10.7

Clone the Marlowe repository and execute `cabal`:

    git clone https://github.com/input-output-hk/marlowe-cardano.git
    cd marlowe-cardano
    cabal install exe:marlowe-cli

## Available Commands

``` bash
marlowe-cli --help
```

    marlowe-cli : a command-line tool for Marlowe contracts

    Usage: marlowe-cli [--version] (COMMAND | COMMAND)

      Utilities for Marlowe.

    Available options:
      -h,--help                Show this help text
      --version                Show version.

    High-level commands:
      run                      Run a contract.
      pab                      Run a contract via the PAB.
      template                 Create a contract from a template.
      test                     Test contracts.

    Low-level commands:
      contract                 Export contract address, validator, datum, or
                               redeemer.
      input                    Create inputs to a contract.
      role                     Export role address, validator, datum, or redeemer.
      query                    Blockchain queries for Marlowe.
      transaction              Create and submit transactions.
      util                     Miscellaneous utilities.

Further help is available for each subcommand:

-   high-level commands
    -   [`marlowe-cli run`](doc/run.md)
    -   [`marlowe-cli pab`](doc/pab.md)
    -   [`marlowe-cli template`](doc/template.md)
    -   [`marlowe-cli test`](doc/test.md)
-   low-level commands
    -   [`marlowe-cli contract`](doc/contract.md)
    -   [`marlowe-cli input`](doc/input.md)
    -   [`marlowe-cli role`](doc/role.md)
    -   [`marlowe-cli query`](doc/query.md)
    -   [`marlowe-cli transaction`](doc/transaction.md)
    -   [`marlowe-cli util`](doc/util.md)

## Workflows

Marlowe CLI supports workflows for specific use cases:

-   [A high-level workflow](#high-level-workflow) for interacting with
    Marlowe contracts without dealing with the underlying its Plutus
    mechanics.
-   [A low-level workflow](#low-level-workflow) that exposes the Plutus
    mechanics of Marlowe contracts.
-   [A backend workflow](#backend-worfklow) for interacting with Marlowe
    via the Plutus Application Backend (PAB).

### High-Level Workflow

In the high-level workflow for `marlowe-cli`, the user creates a
contract from a template, using Marlowe Playground, programmatically, or
by hand. The user provides input at each step of contract execution. The
tool manages the contract state transitions and handles the construction
and submission of transactions.

![High-level Marlowe CLI
workflow.](doc/diagrams/high-level-workflow.svg)

#### Examples

-   [simple contract](examples/simple/ReadMe.md)
-   [escrow](examples/escrow/ReadMe.md)
-   [swap](examples/swap/ReadMe.md)
-   [zero-coupon bond](examples/zcb/ReadMe.md)
-   [contract for differences](examples/cfd/ReadMe.md)
-   [covered call](examples/coveredCall/ReadMe.md)

#### Test Cases

-   [simple contract](examples/simple/run-test.sh)
-   escrow
    -   [\"everything is
        alright\"](examples/escrow/run-everything-is-alright.sh)
    -   [\"confirm problem\"](examples/escrow/run-confirm-problem.sh)
    -   [\"dismiss claim\"](examples/escrow/run-dimiss-claim.sh)
    -   [\"confirm claim\"](examples/escrow/run-confirm-claim.sh)
-   [swap](examples/swap/run-swap.sh)
-   [zero-coupon bond](examples/zcb/run-zcb.sh)
-   [contract for differences](examples/cfd/run-cfd.sh)
-   [covered call](examples/coveredCall/run-coveredCall.sh)

### Low-Level Workflow

The `marlowe-cli` tools supports both granular and monolithic workflows
for creating the files and hashes needed to submit Marlowe contracts
with `cardano-cli`. The workflows only differ in how information is
packaged.

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

## Backend Workflow

Marlowe currently uses the Plutus Application Backend (PAB) as its
backend. A [detailed tutorial](pab-tutorial.md) provides step-by-step
instructions for interacting with it via the command line.

## Automated Tests

-   Tests that interact directly with the Cardano blockchain:
    [run-nonpab-tests.sh](run-nonpab-tests.sh)
-   Tests that interact with the Cardano blockchain via the Marlowe
    backend: [run-tests.sh](run-tests.sh).

## Editing and Rebuilding This Documentation

Most of this documentation is edited in Jupyter notebooks, execute
`nix run` to launch Jupyter.

Execute `make` or `./Makefile` to rebuild this documentation.
