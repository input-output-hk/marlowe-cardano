---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe CLI `test` Commands

The `marlowe-cli test` command provided scripted testing of Marlowe
contracts.

## Contents

-   [Contracts](#contracts)

## Available Commands

``` bash
marlowe-cli test --help
```

    Usage: marlowe-cli test COMMAND

      Test contracts.

    Available options:
      -h,--help                Show this help text

    Commands for testing contracts:
      contracts                Test Marlowe contracts using the Marlowe PAB.

## Contracts

``` bash
marlowe-cli test contracts --help
```

    Usage: marlowe-cli test contracts --testnet-magic INTEGER
                                      --socket-path SOCKET_FILE --wallet-url URL
                                      --pab-url URL --faucet-key SIGNING_FILE
                                      --faucet-address ADDRESS
                                      --burn-address ADDRESS --passphrase PASSWORD
                                      TEST_FILE

      Test Marlowe contracts using the Marlowe PAB.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --wallet-url URL         URL for Cardano Wallet.
      --pab-url URL            URL for the Marlowe PAB.
      --faucet-key SIGNING_FILE
                               The file containing the signing key for the faucet.
      --faucet-address ADDRESS The address of the faucet.
      --burn-address ADDRESS   Burn address for discarding used tokens.
      --passphrase PASSWORD    The passphrase used for the Marlowe PAB.
      TEST_FILE                JSON file containing a test case.
      -h,--help                Show this help text

### Example

See [run-tests.sh](../run-tests.sh) for an example of testing Marlowe
contracts on the PAB.
