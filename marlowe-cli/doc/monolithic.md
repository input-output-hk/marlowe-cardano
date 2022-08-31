---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Example of Monolithic Low-Level Workflow with Marlowe CLI

The monolithic workflow for `marlowe-cli` follows the data flow in the
diagram below. The address, validator, datum, and redeemer for a
transaction are built into one JSON file, and then extracted with `jq`
for use with `cardano-cli`.

![Marlowe workflow using `marlowe-cli`, `cardano-cli`, and
`jq`.](diagrams/monolithic.svg)

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
MARLOWE_FILE=monolithic.marlowe
PLUTUS_FILE=monolithic.plutus
DATUM_FILE=monolithic.datum
REDEEMER_FILE=monolithic.redeemer
```

We just use the simplest contract, `Close`, which is serialised in
[monolithic.contract](monolithic.contract). We use JSON files for the
contract and its current state:

``` bash
CONTRACT_FILE=monolithic.contract
STATE_FILE=monolithic.state
```

``` bash
cat monolithic.contract
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

We now create the Marlowe contract and transaction:

``` bash
marlowe-cli contract marlowe --testnet-magic $MAGIC         \
                             --contract-file $CONTRACT_FILE \
                             --state-file $STATE_FILE       \
                             --out-file $MARLOWE_FILE       \
                             --print-stats
```


    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}
    Validator size: 12386
    Datum size: 64
    Redeemer size: 1
    Total size: 12451

``` bash
jq 'to_entries[] | .key' $MARLOWE_FILE
```

    "redeemer"
    "validator"
    "datum"

We now extract the address, validator, datum, datum hash, and redeemer
from the JSON file `MARLOWE_FILE`:

### Address

``` bash
ADDRESS_S=$(jq -r '.validator.address' $MARLOWE_FILE)
```

``` bash
echo $ADDRESS_S
```

    addr_test1wquea223tl4cdz6n6w000g84hznt2tedafkdvwr6njhex9q39w5zm

### Validator

``` bash
jq '.validator.script' $MARLOWE_FILE > $PLUTUS_FILE
```

``` bash
head -c 1000 $PLUTUS_FILE
```

    {
      "type": "PlutusScriptV1",
      "cborHex": "59306259305f010000332323233223232323232323232323232323232323322323232323232323232323232323322323232323232323232323322332232323232323233223232323232323232323232323322332232323232323232323232323232323232323232323232323232323232323232323232323232332232323232323232323232323232323232323232232222323253353332223500a2235005232322350072323232323223353235001223500223223355335333573466e2000400c23804234044c0d0c8488c00400ccd5421c0400c00454cd4ccd5cd19b88001501008e0108d0113034332212233002004003501033550870100300113322122330010040033355087015002001350112222333308a01004003002500623033122222300200622533335333333305408f0100200101000650a10150a101130341222220051303412222200313034122222004222221533500513333038004003002001153333335015221303b03c13501822225335333355307712001505f2209a01004099011303d03e1333303c0080070060052221303c03d2221303c03d222221303e03f2221303c03d15335333573466e2400540382300422c04540384004cc8848cc00400c008d4d401c888888888801088d4008894ccd400884d401

### Datum

``` bash
DATUM_HASH=$(jq -r '.datum.hash' $MARLOWE_FILE)
```

``` bash
echo $DATUM_HASH
```

    342a77d2f6fd2714b100775083f8976bfad989fca7c9a5ec1f8786a44748f210

``` bash
jq '.datum.json' $MARLOWE_FILE > $DATUM_FILE
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

``` bash
jq '.redeemer.json' $MARLOWE_FILE > $REDEEMER_FILE
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
    e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8     0        45918406 lovelace + TxOutDatumNone
    e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8     1        3000000 lovelace + TxOutDatumNone

Select one of these UTxOs for use in funding the contract, naming it
`TX_0`, and then build and submit the funding transaction:

``` bash
TX_0="e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8#0"
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
    TxId "49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892"

We name the funding transaction as `TX_1`. It is visible on Cardano
explorer at
<https://explorer.dev.testnet.marlowe-finance.io/en/transaction?id=49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892>.

``` bash
TX_1=49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892
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
    49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "342a77d2f6fd2714b100775083f8976bfad989fca7c9a5ec1f8786a44748f210"

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
    TxId "fcba2dd94c87e36840582e8444f578d977cb29f4af1a287a2e4019f38e523071"

We name the closing transaction as `TX_2`. It is visible on Cardano
explorer at
<https://explorer.dev.testnet.marlowe-finance.io/en/transaction?id=fcba2dd94c87e36840582e8444f578d977cb29f4af1a287a2e4019f38e523071>.

``` bash
TX_2=fcba2dd94c87e36840582e8444f578d977cb29f4af1a287a2e4019f38e523071
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
    fcba2dd94c87e36840582e8444f578d977cb29f4af1a287a2e4019f38e523071     0        41836812 lovelace + TxOutDatumNone
    fcba2dd94c87e36840582e8444f578d977cb29f4af1a287a2e4019f38e523071     1        3000000 lovelace + TxOutDatumNone

``` bash
echo marlowe-cli transaction close --testnet-magic $MAGIC                    \
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

    marlowe-cli transaction close --testnet-magic 1566 --socket-path node.socket --tx-in-script-file monolithic.plutus --tx-in-redeemer-file monolithic.redeemer --tx-in-datum-file monolithic.datum --tx-in-marlowe 49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#1 --tx-in 49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#0 --tx-in-collateral 49315e78af81a9025ba136dfcdaf58d2356eaee63553ae42dd3219a7eea3f892#0 --tx-out addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n+3000000 --change-address addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n --invalid-before 100 --invalid-hereafter 100000000 --out-file tx.raw --required-signer path/to/payment.skey --print-stats --submit=600

``` bash
echo marlowe-cli transaction create --testnet-magic $MAGIC                    \
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

    marlowe-cli transaction create --testnet-magic 1566 --socket-path node.socket --script-address addr_test1wquea223tl4cdz6n6w000g84hznt2tedafkdvwr6njhex9q39w5zm --tx-out-datum-file monolithic.datum --tx-out-marlowe 3000000 --tx-in e7fe7883a3d7f78694a7706b74b221762c9777884dd2dbd23115b31862bcf0d8#0 --change-address addr_test1vzl43spe69knxgfl5eqxrr89lwkef3elskmapjvzmy6akmc29ya5n --out-file tx.raw --required-signer path/to/payment.skey --print-stats --submit=600

``` bash
```
