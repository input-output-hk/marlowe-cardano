---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown" tags="[]">

# Marlowe CLI `run` Subcommands

The `marlowe-cli run` subcommands support [a high-level
workflow](../ReadMe.md#high-level-workflow) for interacting with Marlowe
contracts without dealing with the underlying its Plutus mechanics.

The ["contract for differences" example](../examples/cfd/ReadMe.md)
illustrates the use of `marlowe-cli run`.

</div>

<div class="cell markdown">

## Contents

-   [Initialize](#initialize)
-   [Prepare](#prepare)
-   [Execute](#execute)
-   [Withdraw](#withdraw)

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli run --help
```

<div class="output stream stdout">

    Usage: marlowe-cli run (COMMAND | COMMAND)

      Run a contract.

    Available options:
      -h,--help                Show this help text

    Commands for running contracts:
      execute                  Run a Marlowe transaction.
      initialize               Initialize the first transaction of a Marlowe
                               contract and write output to a JSON file.
      prepare                  Prepare the next step of a Marlowe contract and write
                               the output to a JSON file.
      withdraw                 Withdraw funds from the Marlowe role address.

    Experimental commands for running contracts, with automatic balancing.
      auto-execute             [EXPERIMENTAL] Run a Marlowe transaction, selecting
                               transaction inputs and outputs automatically.
      auto-withdraw            [EXPERIMENTAL] Withdraw funds from the Marlowe role
                               address, selecting transaction inputs and outputs
                               automatically.

</div>

</div>

<div class="cell markdown">

## Initialize

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli run initialize --help
```

<div class="output stream stdout">

    Usage: marlowe-cli run initialize --testnet-magic INTEGER
                                      --socket-path SOCKET_FILE 
                                      [--stake-address ADDRESS] 
                                      [--roles-currency CURRENCY_SYMBOL]
                                      --contract-file CONTRACT_FILE
                                      --state-file STATE_FILE 
                                      [--out-file OUTPUT_FILE] [--merkleize] 
                                      [--print-stats]

      Initialize the first transaction of a Marlowe contract and write output to a
      JSON file.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --stake-address ADDRESS  Stake address, if any.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --contract-file CONTRACT_FILE
                               JSON input file for the contract.
      --state-file STATE_FILE  JSON input file for the contract state.
      --out-file OUTPUT_FILE   JSON output file for initialize.
      --merkleize              Whether to deeply merkleize the contract.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Here is a sample invocation of `marlowe-cli run initialize`: it takes
the initial contract [run-1.contract](run-1.contract) file and state
file [run-1.state](run-1.state) as input and it outputs the
comprehensive bundle of Marlowe transaction information
[run-1.marlowe](run-1.marlowe).

``` bash
$ marlowe-cli run initialize --testnet-magic 1566 \
                             --socket-path node.socket \
                             --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f \
                             --contract-file run-1.contract \
                             --state-file run-1.state \
                             --out-file run-1.marlowe \
                             --print-stats
```

``` console
Validator size: 12415
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}
```

</div>

<div class="cell markdown">

## Prepare

</div>

<div class="cell code" execution_count="3">

``` bash
marlowe-cli run prepare --help
```

<div class="output stream stdout">

    Usage: marlowe-cli run prepare --marlowe-file MARLOWE_FILE 
                                   [--deposit-account PARTY --deposit-party PARTY 
                                     [--deposit-token TOKEN]
                                     --deposit-amount INTEGER |
                                     --choice-name NAME --choice-party PARTY
                                     --choice-number INTEGER |
                                     --notify] --invalid-before POSIX_TIME
                                   --invalid-hereafter POSIX_TIME 
                                   [--out-file OUTPUT_FILE] [--print-stats]

      Prepare the next step of a Marlowe contract and write the output to a JSON
      file.

    Available options:
      --marlowe-file MARLOWE_FILE
                               JSON input file for the Marlowe state and contract.
      --deposit-account PARTY  The account for the deposit.
      --deposit-party PARTY    The party making the deposit.
      --deposit-token TOKEN    The token being deposited, if not Ada.
      --deposit-amount INTEGER The amount of token being deposited.
      --choice-name NAME       The name of the choice made.
      --choice-party PARTY     The party making the choice.
      --choice-number INTEGER  The number chosen.
      --notify                 Notify the contract.
      --invalid-before POSIX_TIME
                               Minimum time for the input, in POSIX milliseconds.
      --invalid-hereafter POSIX_TIME
                               Maximum time for the input, in POSIX milliseconds.
      --out-file OUTPUT_FILE   JSON output file for contract.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Here is a sample invocation of `marlowe-cli run prepare`: it takes the
comprehensive Marlowe transaction information
[run-1.marlowe](run-1.marlowe) as input and it outputs the new
comphrensive Marlowe transaction information
[run-2.marlowe](run-2.marlowe) that results from applying the input to
the contract.

``` bash
$ marlowe-cli run prepare --marlowe-file run-1.marlowe \
                          --deposit-account Role=FB \
                          --deposit-party Role=FB \
                          --deposit-amount 7000000 \
                          --invalid-before 1655566811000 \
                          --invalid-hereafter 1655578691000 \
                          --out-file run-2.marlowe \
                          --print-stats
```

``` console
Datum size: 901
```

</div>

<div class="cell markdown">

## Execute

</div>

<div class="cell code" execution_count="4">

``` bash
marlowe-cli run execute --help
```

<div class="output stream stdout">

    Usage: marlowe-cli run execute --testnet-magic INTEGER --socket-path SOCKET_FILE
                                   [--marlowe-in-file MARLOWE_FILE
                                     --tx-in-marlowe TXID#TXIX
                                     --tx-in-collateral TXID#TXIX]
                                   --marlowe-out-file MARLOWE_FILE 
                                   [--tx-in TXID#TXIX] [--tx-out ADDRESS+VALUE]
                                   --change-address ADDRESS
                                   (--required-signer SIGNING_FILE) 
                                   [--metadata-file METADATA_FILE] --out-file FILE 
                                   [--submit SECONDS] [--print-stats] 
                                   [--script-invalid]

      Run a Marlowe transaction.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --marlowe-in-file MARLOWE_FILE
                               JSON file with the Marlowe initial state and initial
                               contract, if any.
      --tx-in-marlowe TXID#TXIX
                               UTxO spent from Marlowe contract, if any.
      --tx-in-collateral TXID#TXIX
                               Collateral for transaction, if any.
      --marlowe-out-file MARLOWE_FILE
                               JSON file with the Marlowe inputs, final state, and
                               final contract.
      --tx-in TXID#TXIX        Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE   Transaction output in ADDRESS+VALUE format.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --metadata-file METADATA_FILE
                               JSON file containing metadata.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      --print-stats            Print statistics.
      --script-invalid         Assert that the transaction is invalid.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Here is a sample invocation of `marlowe-cli run execute`: it takes the
comprehensive Marlowe information for the prior and new transaction,
respectively [run-1.marlowe](run-1.marlowe) and
[run-2.marlowe](run-2.marlowe) as input and it outputs the Cardano
transaction body [run-2.raw](run-2.raw).

``` bash
$ marlowe-cli run execute --testnet-magic 1566 \
                          --socket-path node.socket \
                          --marlowe-in-file run-1.marlowe \
                          --tx-in-marlowe 'c2044f136b94a9c8f272ef8108c859733d43e2afc36ffb0c68de0d4894f44cbe#1' \
                          --tx-in-collateral 'e338b26be569e3dc79f9a07c4d75ab79fdde9534860f984635556dac3a620b1c#0' \
                          --tx-in 'e338b26be569e3dc79f9a07c4d75ab79fdde9534860f984635556dac3a620b1c#0' \
                          --tx-in 'e338b26be569e3dc79f9a07c4d75ab79fdde9534860f984635556dac3a620b1c#3' \
                          --required-signer francis-beaumont.skey \
                          --marlowe-out-file run-2.marlowe \
                          --tx-out 'addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q+2000000+1 d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f.FB' \
                          --change-address addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q \
                          --out-file run-2.raw \
                          --print-stats \
                          --submit=600
```

``` console
Fee: Lovelace 1392248
Size: 14729 / 32768 = 44%
Execution units:
  Memory: 6779848 / 30000000 = 22%
  Steps: 2467753419 / 10000000000 = 24%
```

</div>

<div class="cell markdown">

## Withdraw

</div>

<div class="cell code" execution_count="5">

``` bash
marlowe-cli run withdraw --help
```

<div class="output stream stdout">

    Usage: marlowe-cli run withdraw --testnet-magic INTEGER
                                    --socket-path SOCKET_FILE
                                    --marlowe-file MARLOWE_FILE
                                    --role-name TOKEN_NAME
                                    --tx-in-collateral TXID#TXIX [--tx-in TXID#TXIX]
                                    [--tx-out ADDRESS+VALUE]
                                    --change-address ADDRESS
                                    (--required-signer SIGNING_FILE) 
                                    [--metadata-file METADATA_FILE] --out-file FILE 
                                    [--submit SECONDS] [--print-stats] 
                                    [--script-invalid]

      Withdraw funds from the Marlowe role address.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --marlowe-file MARLOWE_FILE
                               JSON file with the Marlowe inputs, final state, and
                               final contract.
      --role-name TOKEN_NAME   The role name for the withdrawal.
      --tx-in-collateral TXID#TXIX
                               Collateral for transaction.
      --tx-in TXID#TXIX        Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE   Transaction output in ADDRESS+VALUE format.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --metadata-file METADATA_FILE
                               JSON file containing metadata.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      --print-stats            Print statistics.
      --script-invalid         Assert that the transaction is invalid.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Here is a sample invocation of `marlowe-cli run withdraw`: it takes the
comprehensive Marlowe transaction information
[run-5.marlowe](run-5.marlowe) as input and it outputs the Cardano
transaction body file [run-6.raw](run-6.raw).

``` bash
$ marlowe-cli run withdraw --testnet-magic 1566 \
                           --socket-path node.socket \
                           --marlowe-file run-5.marlowe \
                           --role-name FB \
                           --tx-in 'cc618dd85f19629509a6eacf11cd59357c18e3e9699c0b02797889e0c717d2f9#0' \
                           --tx-in 'cc618dd85f19629509a6eacf11cd59357c18e3e9699c0b02797889e0c717d2f9#2' \
                           --tx-in-collateral 'cc618dd85f19629509a6eacf11cd59357c18e3e9699c0b02797889e0c717d2f9#0' \
                           --required-signer francis-beaumont.skey \
                           --tx-out 'addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q+2000000+1 d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f.FB' \
                           --change-address addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q \
                           --out-file run-6.raw \
                           --print-stats \
                           --submit=600
```

``` console
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
```

</div>

<div class="cell markdown" tags="[]">

## Automatically Execute

</div>

<div class="cell code" execution_count="6">

``` bash
marlowe-cli run auto-execute --help
```

<div class="output stream stdout">

    Usage: marlowe-cli run auto-execute 
             --testnet-magic INTEGER --socket-path SOCKET_FILE 
             [--marlowe-in-file MARLOWE_FILE --tx-in-marlowe TXID#TXIX]
             --marlowe-out-file MARLOWE_FILE --change-address ADDRESS
             (--required-signer SIGNING_FILE) [--metadata-file METADATA_FILE]
             --out-file FILE [--submit SECONDS] [--print-stats] [--script-invalid]

      [EXPERIMENTAL] Run a Marlowe transaction, selecting transaction inputs and
      outputs automatically.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --marlowe-in-file MARLOWE_FILE
                               JSON file with the Marlowe initial state and initial
                               contract, if any.
      --tx-in-marlowe TXID#TXIX
                               UTxO spent from Marlowe contract, if any.
      --marlowe-out-file MARLOWE_FILE
                               JSON file with the Marlowe inputs, final state, and
                               final contract.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --metadata-file METADATA_FILE
                               JSON file containing metadata.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      --print-stats            Print statistics.
      --script-invalid         Assert that the transaction is invalid.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Here is a sample invocation of `marlowe-cli run execute`: it takes the
comprehensive Marlowe information for the prior and new transaction,
respectively [run-1.marlowe](run-1.marlowe) and
[run-2.marlowe](run-2.marlowe) as input and it outputs the Cardano
transaction body [run-2.raw](run-2.raw).

``` bash
$ marlowe-cli run auto-execute --testnet-magic 1566 \
                               --socket-path node.socket \
                               --marlowe-in-file run-1.marlowe \
                               --tx-in-marlowe 'c2044f136b94a9c8f272ef8108c859733d43e2afc36ffb0c68de0d4894f44cbe#1' \
                               --required-signer francis-beaumont.skey \
                               --marlowe-out-file run-2.marlowe \
                               --change-address addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q \
                               --out-file run-2.raw \
                               --print-stats \
                               --submit=600
```

``` console
Fee: Lovelace 1392248
Size: 14729 / 32768 = 44%
Execution units:
  Memory: 6779848 / 30000000 = 22%
  Steps: 2467753419 / 10000000000 = 24%
```

</div>

<div class="cell markdown">

## Automatically Withdraw

</div>

<div class="cell code" execution_count="7">

``` bash
marlowe-cli run auto-withdraw --help
```

<div class="output stream stdout">

    Usage: marlowe-cli run auto-withdraw 
             --testnet-magic INTEGER --socket-path SOCKET_FILE
             --marlowe-file MARLOWE_FILE --role-name TOKEN_NAME
             --change-address ADDRESS (--required-signer SIGNING_FILE) 
             [--metadata-file METADATA_FILE] --out-file FILE [--submit SECONDS] 
             [--print-stats] [--script-invalid]

      [EXPERIMENTAL] Withdraw funds from the Marlowe role address, selecting
      transaction inputs and outputs automatically.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --marlowe-file MARLOWE_FILE
                               JSON file with the Marlowe inputs, final state, and
                               final contract.
      --role-name TOKEN_NAME   The role name for the withdrawal.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --metadata-file METADATA_FILE
                               JSON file containing metadata.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      --print-stats            Print statistics.
      --script-invalid         Assert that the transaction is invalid.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Here is a sample invocation of `marlowe-cli run withdraw`: it takes the
comprehensive Marlowe transaction information
[run-5.marlowe](run-5.marlowe) as input and it outputs the Cardano
transaction body file [run-6.raw](run-6.raw).

``` bash
$ marlowe-cli run auto-withdraw --marlowe-file run-5.marlowe \
                                --role-name FB \
                                --required-signer francis-beaumont.skey \
                                --change-address addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q \
                                --out-file run-6.raw \
                                --print-stats \
                                --submit=600
```

``` console
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
```

</div>
