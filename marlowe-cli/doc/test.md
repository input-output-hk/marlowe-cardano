---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Marlowe CLI `test` Commands

The `marlowe-cli test` command provided scripted testing of Marlowe
contracts.

</div>

<div class="cell markdown" jp-MarkdownHeadingCollapsed="true" tags="[]">

## Contents

-   [Contracts](#contracts)

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli test --help
```

<div class="output stream stdout">

    Usage: marlowe-cli test COMMAND

      Test contracts.

    Available options:
      -h,--help                Show this help text

    Commands for testing contracts:
      scripts                  Test Marlowe scripts on-chain.

</div>

</div>

<div class="cell markdown">

## Scripts

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli test scripts --help
```

<div class="output stream stdout">

    Usage: marlowe-cli test scripts --testnet-magic INTEGER
                                    --socket-path SOCKET_FILE
                                    --faucet-key SIGNING_FILE
                                    --faucet-address ADDRESS --burn-address ADDRESS
                                    TEST_FILE

      Test Marlowe scripts on-chain.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --faucet-key SIGNING_FILE
                               The file containing the signing key for the faucet.
      --faucet-address ADDRESS The address of the faucet.
      --burn-address ADDRESS   Burn address for discarding used tokens.
      TEST_FILE                JSON file containing a test case.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

</div>

<div class="cell markdown">

See the folder [marlowe-cli/test/non-pab/](../test/non-pab/) for example
input files.

</div>
