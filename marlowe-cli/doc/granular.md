---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Example of Granular Low-Level Workflow with Marlowe CLI

The granular workflow for `marlowe-cli` follows the data flow in the
diagram below. The address, validator, datum, and redeemer for a
transaction are built separately, but then combined into a transaction.

![Marlowe workflow using `marlowe-cli` and
`cardano-cli`.](diagrams/granular.svg)

## 1. Select network. {#1-select-network}

Make sure that `marlowe-cli`, `cardano-cli`, and `jq` have been
installed on the path for the `bash` shell. Set the environment variable
`CARDANO_NODE_SOCKET_PATH` to point to the location of the socket for
the `cardano-node` service: see
<https://developers.cardano.org/docs/get-started/running-cardano/#querying-the-cardano-blockchain>.
In this tutorial, we use the public `testnet`:

``` bash
MAGIC=1566
```

``` bash
export CARDANO_NODE_SOCKET_PATH=node.socket
```

## 2. Select wallet. {#2-select-wallet}

Select a wallet for use in this tutorial and specify the files with the
signing and payment keys. The address of this wallet is stored in the
environment variable `ADDRESS_P`.

``` bash
PAYMENT_SKEY=path/to/payment.skey
PAYMENT_VKEY=path/to/payment.vkey
```

``` bash
ADDRESS_P=$(cardano-cli address build --testnet-magic $MAGIC --payment-verification-key-file $PAYMENT_VKEY)
echo $ADDRESS_P
```

    addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n

``` bash
PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)
echo $PUBKEYHASH_P
```

    bf58c039d16d33213fa640618ce5fbad94c73f85b7d0c982d935db6f

## 3. Design the Marlowe contract. {#3-design-the-marlowe-contract}

First, we choose names for the files containing the validator, datum,
and redeemer.

``` bash
PLUTUS_FILE=granular.plutus
DATUM_FILE=granular.datum
REDEEMER_FILE=granular.redeemer
```

We just use the simplest contract, `Close`, which is serialised in
[granular.contract](granular.contract). We use JSON files for the
contract and its current state:

``` bash
CONTRACT_FILE=granular.contract
STATE_FILE=granular.state
```

``` bash
cat granular.contract
```

    "close"

We will put 3 ADA into the account for the wallet, as recorded in the
contract\'s state:

``` bash
DATUM_LOVELACE=3000000
```

``` bash
cat << EOI > $STATE_FILE
{
  "choices": [],
  "accounts": [
    [
      [
        {
          "pk_hash": "$PUBKEYHASH_P"
        },
        {
          "currency_symbol": "",
          "token_name": ""
        }
      ],
      $DATUM_LOVELACE
    ]
  ],
  "minTime": 10,
  "boundValues": []
}
EOI
```

We will redeem the ADA within a particular range of slots:

``` bash
REDEEM_MIN_SLOT=100
REDEEM_MAX_SLOT=100000000
```

## 4. Create the validator, datum, and redeemer. {#4-create-the-validator-datum-and-redeemer}

We now create the Plutus script for the contract and compute its script
address:

### Validator

``` bash
ADDRESS_S=$(marlowe-cli contract address --testnet-magic $MAGIC)
```

This default Marlowe validator has the following address:

``` bash
echo $ADDRESS_S
```

    addr_test1wquea223tl4cdz6n6w000g84hznt2tedafkdvwr6njhex9q39w5zm

``` bash
marlowe-cli contract validator --testnet-magic $MAGIC  \
                               --out-file $PLUTUS_FILE \
                               --print-stats
```

    addr_test1wquea223tl4cdz6n6w000g84hznt2tedafkdvwr6njhex9q39w5zm

    Validator size: 12386
    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}

``` bash
head -c 1000 $PLUTUS_FILE
```

    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "59306259305f010000332323233223232323232323232323232323232323322323232323232323232323232323322323232323232323232323322332232323232323233223232323232323232323232323322332232323232323232323232323232323232323232323232323232323232323232323232323232332232323232323232323232323232323232323232232222323253353332223500a2235005232322350072323232323223353235001223500223223355335333573466e2000400c23804234044c0d0c8488c00400ccd5421c0400c00454cd4ccd5cd19b88001501008e0108d0113034332212233002004003501033550870100300113322122330010040033355087015002001350112222333308a01004003002500623033122222300200622533335333333305408f0100200101000650a10150a101130341222220051303412222200313034122222004222221533500513333038004003002001153333335015221303b03c13501822225335333355307712001505f2209a01004099011303d03e1333303c0080070060052221303c03d2221303c03d222221303e03f2221303c03d15335333573466e2400540382300422c04540384004cc8848cc00400c008d4d401c888888888

### Datum

The contract and its state form the datum, which we also compute:

``` bash
DATUM_HASH=$(
  marlowe-cli contract datum --contract-file $CONTRACT_FILE \
                             --state-file $STATE_FILE       \
                             --out-file $DATUM_FILE         \
)
```

``` bash
json2yaml $DATUM_FILE
```

    constructor: 0
    fields:
    - constructor: 0
      fields:
      - map:
        - k:
            constructor: 0
            fields:
            - constructor: 0
              fields:
              - bytes: bf58c039d16d33213fa640618ce5fbad94c73f85b7d0c982d935db6f
            - constructor: 0
              fields:
              - bytes: ''
              - bytes: ''
          v:
            int: 3000000
      - map: []
      - map: []
      - int: 10
    - constructor: 0
      fields: []

### Redeemer

Since this is the `Close` contract, the redeemer needs no input:

``` bash
marlowe-cli contract redeemer --out-file $REDEEMER_FILE
```

``` bash
json2yaml $REDEEMER_FILE
```

    list: []

## 5. Fund the contract. {#5-fund-the-contract}

Before running the contract, we need to put funds into it. Examine the
UTxOs at the wallet address:

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address $ADDRESS_P
```

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    c072b723d9dc197b68b2c84a97ade77d7f14c142df6aeac3a4d5b691b8bc6715     2        50000000 lovelace + TxOutDatumNone

Select one of these UTxOs for use in funding the contract, naming it
`TX_0`, and then build and submit the funding transaction:

``` bash
TX_0="c072b723d9dc197b68b2c84a97ade77d7f14c142df6aeac3a4d5b691b8bc6715#2"
```

``` bash
marlowe-cli transaction create --testnet-magic $MAGIC                    \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --script-address "$ADDRESS_S"             \
                               --tx-out-datum-file $DATUM_FILE           \
                               --tx-out-marlowe $DATUM_LOVELACE          \
                               --tx-in "$TX_0"                           \
                               --change-address "$ADDRESS_P"             \
                               --out-file tx.raw                         \
                               --required-signer $PAYMENT_SKEY           \
                               --print-stats                             \
                               --submit=600
```


    Fee: Lovelace 173289
    Size: 265 / 32768 = 0%
    Execution units:
      Memory: 0 / 30000000 = 0%
      Steps: 0 / 10000000000 = 0%
    TxId "b05bfbbbd8764124e142d88771b3cdb30d6e63f446839dc66dc93fe814a04bb6"

We name the funding transaction as `TX_1`. It is visible on Cardano
explorer at
<https://explorer.dev.testnet.marlowe-finance.io/en/transaction?id=b05bfbbbd8764124e142d88771b3cdb30d6e63f446839dc66dc93fe814a04bb6>.

``` bash
TX_1=b05bfbbbd8764124e142d88771b3cdb30d6e63f446839dc66dc93fe814a04bb6
```

After the transaction is recorded on the blockchain, there are funds at
the contract address with the data hash `DATUM_HASH`.

``` bash
echo $DATUM_HASH
```

    342a77d2f6fd2714b100775083f8976bfad989fca7c9a5ec1f8786a44748f210

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address "$ADDRESS_S" | sed -n -e "1p; 2p; /$TX_1/p"
```

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    b05bfbbbd8764124e142d88771b3cdb30d6e63f446839dc66dc93fe814a04bb6     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "342a77d2f6fd2714b100775083f8976bfad989fca7c9a5ec1f8786a44748f210"

## 6. Redeem the funds by running the contract. {#6-redeem-the-funds-by-running-the-contract}

We now use the previously computed redeemer and datum to remove the
funds from the contract. This involves computing the fee, building the
transaction, signing it, and submitting it.

``` bash
marlowe-cli transaction close --testnet-magic $MAGIC                    \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --tx-in-script-file $PLUTUS_FILE          \
                              --tx-in-redeemer-file $REDEEMER_FILE      \
                              --tx-in-datum-file $DATUM_FILE            \
                              --tx-in-marlowe "$TX_1#1"                 \
                              --tx-in "$TX_1#0"                         \
                              --tx-in-collateral "$TX_1#0"              \
                              --tx-out "$ADDRESS_P"+$DATUM_LOVELACE     \
                              --change-address "$ADDRESS_P"             \
                              --invalid-before $REDEEM_MIN_SLOT         \
                              --invalid-hereafter $REDEEM_MAX_SLOT      \
                              --out-file tx.raw                         \
                              --required-signer $PAYMENT_SKEY           \
                              --print-stats                             \
                              --submit=600
```


    Fee: Lovelace 908305
    Size: 12748 / 32768 = 38%
    Execution units:
      Memory: 2132768 / 30000000 = 7%
      Steps: 745166573 / 10000000000 = 7%
    TxId "e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8"

We name the closing transaction as `TX_2`. It is visible on Cardano
explorer at
<https://explorer.dev.testnet.marlowe-finance.io/en/transaction?id=e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8>.

``` bash
TX_2=e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8
```

After the transaction is recorded on the blockchain, we see that the
funds were removed from the script address and are in the wallet.

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address "$ADDRESS_S" | sed -n -e "1p; 2p; /$TX_1/p; /$TX_2/p"
```

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address "$ADDRESS_P" | sed -n -e "1p; 2p; /$TX_1/p; /$TX_2/p"
```

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8     0        45918406 lovelace + TxOutDatumNone
    e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8     1        3000000 lovelace + TxOutDatumNone
