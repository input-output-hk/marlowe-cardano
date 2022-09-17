---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Marlowe CLI `transaction` Subcommands

The `marlowe-cli transaction` commands construct, and optionally submit,
low-level Marlowe transactions.

</div>

<div class="cell markdown">

## Contents

-   [Simple](#simple)
-   [Create](#create)
-   [Advance](#advance)
-   [Close](#close)
-   [Submit](#submit)

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli transaction --help
```

<div class="output stream stdout">

    Usage: marlowe-cli transaction COMMAND

      Create and submit transactions.

    Available options:
      -h,--help                Show this help text

    Low-level commands for creating and submitting transactions:
      advance                  Build a transaction that both spends from and pays to
                               a Marlowe script.
      close                    Build a transaction that spends from a Marlowe
                               script.
      create                   Build a transaction that pays to a Marlowe script.
      simple                   Build a non-Marlowe transaction.
      find-published           Publish Marlowe validator and role validator on the
                               chain.
      publish                  Publish Marlowe validator and role validator on the
                               chain.
      submit                   Submit a transaction body.

</div>

</div>

<div class="cell markdown">

## Simple

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli transaction simple --help
```

<div class="output stream stdout">

    Usage: marlowe-cli transaction simple 
             --testnet-magic INTEGER --socket-path SOCKET_FILE
             (--required-signer SIGNING_FILE) (--tx-in TXID#TXIX) 
             [--tx-out ADDRESS+VALUE] --change-address ADDRESS 
             [--metadata-file METADATA_FILE] --out-file FILE [--submit SECONDS] 
             [--print-stats] [--script-invalid]

      Build a non-Marlowe transaction.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --tx-in TXID#TXIX        Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE   Transaction output in ADDRESS+VALUE format.
      --change-address ADDRESS Address to receive ADA in excess of fee.
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

``` bash
$ marlowe-cli transaction simple --testnet-magic 1566 \
                                 --socket-path node.socket \
                                 --tx-in 'ddeebf86ee6390c10ae6d204f8c13ae74ecc02151fcce9e3e45099d32815b963#0' \
                                 --tx-in 'ddeebf86ee6390c10ae6d204f8c13ae74ecc02151fcce9e3e45099d32815b963#1' \
                                 --tx-in 'ddeebf86ee6390c10ae6d204f8c13ae74ecc02151fcce9e3e45099d32815b963#2' \
                                 --tx-out 'addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz+1400000+1 d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f.FB' \
                                 --required-signer francis-beaumont.skey \
                                 --change-address addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8 \
                                 --out-file /dev/null \
                                 --submit 600
```

</div>

<div class="cell markdown">

## Create

</div>

<div class="cell code" execution_count="3">

``` bash
marlowe-cli transaction create --help
```

<div class="output stream stdout">

    Usage: marlowe-cli transaction create 
             --testnet-magic INTEGER --socket-path SOCKET_FILE
             --script-address ADDRESS (--required-signer SIGNING_FILE)
             --tx-out-datum-file DATUM_FILE --tx-out-marlowe VALUE
             (--tx-in TXID#TXIX) [--tx-out ADDRESS+VALUE] --change-address ADDRESS 
             [--metadata-file METADATA_FILE] --out-file FILE [--submit SECONDS] 
             [--print-stats] [--script-invalid]

      Build a transaction that pays to a Marlowe script.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --script-address ADDRESS Address of the Marlowe contract.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --tx-out-datum-file DATUM_FILE
                               Datum JSON file datum paid to Marlowe contract.
      --tx-out-marlowe VALUE   Value paid to Marlowe contract.
      --tx-in TXID#TXIX        Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE   Transaction output in ADDRESS+VALUE format.
      --change-address ADDRESS Address to receive ADA in excess of fee.
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

``` bash
$ marlowe-cli transaction create --testnet-magic 1566 \
                                 --socket-path node.socket \
                                 --script-address addr_test1wquea223tl4cdz6n6w000g84hznt2tedafkdvwr6njhex9q39w5zm \
                                 --tx-out-datum-file monolithic.datum \
                                 --tx-out-marlowe 3000000 \
                                 --tx-in 'e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8#0' \
                                 --change-address addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n \
                                 --out-file tx.raw \
                                 --required-signer path/to/payment.skey \
                                 --print-stats \
                                 --submit=600
```

</div>

<div class="cell markdown">

## Advance

</div>

<div class="cell code" execution_count="4">

``` bash
marlowe-cli transaction advance --help
```

<div class="output stream stdout">

    Usage: marlowe-cli transaction advance 
             --testnet-magic INTEGER --socket-path SOCKET_FILE
             --script-address ADDRESS --tx-in-script-file PLUTUS_FILE
             --tx-in-redeemer-file REDEEMER_FILE --tx-in-datum-file DATUM_FILE
             (--required-signer SIGNING_FILE) --tx-in-marlowe TXID#TXIX
             --tx-out-datum-file DATUM_FILE --tx-out-marlowe VALUE
             (--tx-in TXID#TXIX) [--tx-out ADDRESS+VALUE]
             --tx-in-collateral TXID#TXIX --change-address ADDRESS
             --invalid-before SLOT --invalid-hereafter SLOT 
             [--metadata-file METADATA_FILE] --out-file FILE [--submit SECONDS] 
             [--print-stats] [--script-invalid]

      Build a transaction that both spends from and pays to a Marlowe script.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --script-address ADDRESS Address of the Marlowe contract.
      --tx-in-script-file PLUTUS_FILE
                               Plutus file for Marlowe contract.
      --tx-in-redeemer-file REDEEMER_FILE
                               Redeemer JSON file spent from Marlowe contract.
      --tx-in-datum-file DATUM_FILE
                               Datum JSON file spent from Marlowe contract.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --tx-in-marlowe TXID#TXIX
                               UTxO spent from Marlowe contract.
      --tx-out-datum-file DATUM_FILE
                               Datum JSON file datum paid to Marlowe contract.
      --tx-out-marlowe VALUE   Value paid to Marlowe contract.
      --tx-in TXID#TXIX        Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE   Transaction output in ADDRESS+VALUE format.
      --tx-in-collateral TXID#TXIX
                               Collateral for transaction.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --invalid-before SLOT    Minimum slot for the redemption.
      --invalid-hereafter SLOT Maximum slot for the redemption.
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

``` bash
$ marlowe-cli transaction advance --testnet-magic 1566 \
                                  --socket-path node.socket \
                                  --script-address addr_test1wquea223tl4cdz6n6w000g84hznt2tedafkdvwr6njhex9q39w5zm \
                                  --tx-in-script-file monolithic.plutus \
                                  --tx-in-redeemer-file monolithic.redeemer \
                                  --tx-in-datum-file monolithic.datum \
                                  --tx-in-marlowe '49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#1' \
                                  --tx-out-datum-file monolithic.datum \
                                  --tx-out-marlowe 3000000 \
                                  --tx-in 'e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8#0' \
                                  --tx-in-marlowe '49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#1' \
                                  --tx-in '49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#0' \
                                  --tx-in-collateral '49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#0' \
                                  --change-address addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n \
                                  --invalid-before 100 \
                                  --invalid-hereafter 100000000 \
                                  --out-file tx.raw \
                                  --required-signer path/to/payment.skey \
                                  --print-stats \
                                  --submit=600
```

</div>

<div class="cell markdown">

## Close

</div>

<div class="cell code" execution_count="5">

``` bash
marlowe-cli transaction close --help
```

<div class="output stream stdout">

    Usage: marlowe-cli transaction close 
             --testnet-magic INTEGER --socket-path SOCKET_FILE
             --tx-in-script-file PLUTUS_FILE --tx-in-redeemer-file REDEEMER_FILE
             --tx-in-datum-file DATUM_FILE (--required-signer SIGNING_FILE)
             --tx-in-marlowe TXID#TXIX (--tx-in TXID#TXIX) [--tx-out ADDRESS+VALUE]
             --tx-in-collateral TXID#TXIX --change-address ADDRESS
             --invalid-before SLOT --invalid-hereafter SLOT 
             [--metadata-file METADATA_FILE] --out-file FILE [--submit SECONDS] 
             [--print-stats] [--script-invalid]

      Build a transaction that spends from a Marlowe script.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --tx-in-script-file PLUTUS_FILE
                               Plutus file for Marlowe contract.
      --tx-in-redeemer-file REDEEMER_FILE
                               Redeemer JSON file spent from Marlowe contract.
      --tx-in-datum-file DATUM_FILE
                               Datum JSON file spent from Marlowe contract.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --tx-in-marlowe TXID#TXIX
                               UTxO spent from Marlowe contract.
      --tx-in TXID#TXIX        Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE   Transaction output in ADDRESS+VALUE format.
      --tx-in-collateral TXID#TXIX
                               Collateral for transaction.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --invalid-before SLOT    Minimum slot for the redemption.
      --invalid-hereafter SLOT Maximum slot for the redemption.
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

``` bash
$ marlowe-cli transaction close --testnet-magic 1566 \
                                --socket-path node.socket \
                                --tx-in-script-file monolithic.plutus \
                                --tx-in-redeemer-file monolithic.redeemer \
                                --tx-in-datum-file monolithic.datum \
                                --tx-in-marlowe '49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#1' \
                                --tx-in '49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#0' \
                                --tx-in-collateral '49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#0' \
                                --tx-out 'addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n+3000000' \
                                --change-address addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n \
                                --invalid-before 100 \
                                --invalid-hereafter 100000000 \
                                --out-file tx.raw \
                                --required-signer path/to/payment.skey \
                                --print-stats \
                                --submit=600
```

</div>

<div class="cell markdown">

## Submit

</div>

<div class="cell code" execution_count="6">

``` bash
marlowe-cli transaction submit --help
```

<div class="output stream stdout">

    Usage: marlowe-cli transaction submit 
             --testnet-magic INTEGER --socket-path SOCKET_FILE
             --tx-body-file BODY_FILE (--required-signer SIGNING_FILE) 
             [--timeout SECONDS]

      Submit a transaction body.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file. Defaults to
                               the CARDANO_NODE_SOCKET_PATH environment variable's
                               value.
      --tx-body-file BODY_FILE File containing the transaction body.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --timeout SECONDS        Also submit the transaction, and wait for
                               confirmation.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

``` bash
$ marlowe-cli transaction submit --testnet-magic 1566 \
                                 --socket-path node.socket \
                                 --tx-body-file tx.raw \
                                 --required-signer christopher-marlowe.skey \
                                 --timeout 600
```

</div>
