---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Example of Monolithic Low-Level Workflow with Marlowe CLI

The monolithic workflow for `marlowe-cli` follows the data flow in the
diagram below. The address, validator, datum, and redeemer for a
transaction are built into one JSON file, and then extracted with `jq`
for use with `cardano-cli`.

![Marlowe workflow using `marlowe-cli`, `cardano-cli`, and
`jq`.](diagrams/monolithic.svg)

</div>

<div class="cell markdown">

## 1. Select network.

Make sure that `marlowe-cli`, `cardano-cli`, and `jq` have been
installed on the path for the `bash` shell. Set the environment variable
`CARDANO_NODE_SOCKET_PATH` to point to the location of the socket for
the `cardano-node` service: see
<https://developers.cardano.org/docs/get-started/running-cardano/#querying-the-cardano-blockchain>.
In this tutorial, we use the public `testnet`:

</div>

<div class="cell code" execution_count="1">

``` bash
MAGIC=2
```

</div>

<div class="cell code" execution_count="2">

``` bash
export CARDANO_NODE_SOCKET_PATH=node.socket
```

</div>

<div class="cell markdown">

## 2. Select wallet.

Select a wallet for use in this tutorial and specify the files with the
signing and payment keys. The address of this wallet is stored in the
environment variable `ADDRESS_P`.

</div>

<div class="cell code" execution_count="3">

``` bash
PAYMENT_SKEY=path/to/payment.skey
PAYMENT_VKEY=path/to/payment.vkey
```

</div>

<div class="cell code" execution_count="4">

``` bash
ADDRESS_P=$(cardano-cli address build --testnet-magic $MAGIC --payment-verification-key-file $PAYMENT_VKEY)
echo $ADDRESS_P
```

<div class="output stream stdout">

    addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j

</div>

</div>

<div class="cell markdown">

## 3. Design the Marlowe contract.

First, we choose names for the files containing the validator, datum,
and redeemer.

</div>

<div class="cell code" execution_count="5">

``` bash
MARLOWE_FILE=monolithic.marlowe
PLUTUS_FILE=monolithic.plutus
DATUM_FILE=monolithic.datum
REDEEMER_FILE=monolithic.redeemer
```

</div>

<div class="cell markdown">

We just use the simplest contract, `Close`, which is serialised in
[monolithic.contract](monolithic.contract). We use JSON files for the
contract and its current state:

</div>

<div class="cell code" execution_count="6">

``` bash
CONTRACT_FILE=monolithic.contract
STATE_FILE=monolithic.state
```

</div>

<div class="cell code" execution_count="7">

``` bash
cat monolithic.contract
```

<div class="output stream stdout">

    "close"

</div>

</div>

<div class="cell markdown">

We will put 3 ADA into the account for the wallet, as recorded in the
contract's state:

</div>

<div class="cell code" execution_count="8">

``` bash
DATUM_LOVELACE=3000000
```

</div>

<div class="cell code" execution_count="9">

``` bash
cat << EOI > $STATE_FILE
{
  "choices": [],
  "accounts": [
    [
      [
        {
          "address": "$ADDRESS_P"
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

</div>

<div class="cell markdown">

We will redeem the ADA within a particular range of slots:

</div>

<div class="cell code" execution_count="10">

``` bash
REDEEM_MIN_SLOT=3425000
REDEEM_MAX_SLOT=3426000
```

</div>

<div class="cell markdown">

## 4. Create the validator, datum, and redeemer.

We now create the Marlowe contract and transaction:

</div>

<div class="cell code" execution_count="11">

``` bash
marlowe-cli contract marlowe --testnet-magic $MAGIC         \
                             --contract-file $CONTRACT_FILE \
                             --state-file $STATE_FILE       \
                             --out-file $MARLOWE_FILE       \
                             --print-stats
```

<div class="output stream stdout">


    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 18653100, exBudgetMemory = ExMemory 81200}
    Validator size: 12668
    Datum size: 85
    Redeemer size: 3
    Total size: 12756

</div>

</div>

<div class="cell code" execution_count="12">

``` bash
jq 'to_entries[] | .key' $MARLOWE_FILE
```

<div class="output stream stdout">

    "datum"
    "redeemer"
    "validator"

</div>

</div>

<div class="cell markdown">

We now extract the address, validator, datum, datum hash, and redeemer
from the JSON file `MARLOWE_FILE`:

</div>

<div class="cell markdown">

### Address

</div>

<div class="cell code" execution_count="13">

``` bash
ADDRESS_S=$(jq -r '.validator.address' $MARLOWE_FILE)
```

</div>

<div class="cell code" execution_count="14">

``` bash
echo $ADDRESS_S
```

<div class="output stream stdout">

    addr_test1wrv0vwr4megau50ujjwsktvajmsu6dzza2rnalufd3husaqs9v6rv

</div>

</div>

<div class="cell markdown">

### Validator

</div>

<div class="cell code" execution_count="15">

``` bash
jq '.validator.script' $MARLOWE_FILE > $PLUTUS_FILE
```

</div>

<div class="cell code" execution_count="16">

``` bash
head -c 1000 $PLUTUS_FILE
```

<div class="output stream stdout">

    {
      "cborHex": "59317c593179010000332323233223232323322323322323232323232323232323232323232323232323232323232323232323232323232332233223232323232332232323232323232323232323232332233223232323232323232323232323232323232323232323232323232323232323232323232323232323233223232323232323232323232323232323223222232325335333222350032232322350062323232323223353235001223500223223355335333573466e2000400c25404250044c0c0c8488c00400ccd542040400c00454cd4ccd5cd19b88001501009501094011303033221223300200400350103355081010030011332212233001004003335508101500200135011222233330840100400300250062302f122222300200622533335333333305309601002001010006509f01509f01130301222220051303012222200313030122222004222221533500513333034004003002001153333335015221303703813501822225335333355307112001505e220a1010040a0011303903a133330380080070060052221303803922213038039222221303a03b2221303803915335333573466e24005403824c0424804540384004cc8848cc00400c0094cd4c8d400488d4008894ccd400884d4010894ccd400884d426c04cd5421004cdc099b800044800

</div>

</div>

<div class="cell markdown">

### Datum

</div>

<div class="cell code" execution_count="17">

``` bash
DATUM_HASH=$(jq -r '.datum.hash' $MARLOWE_FILE)
```

</div>

<div class="cell code" execution_count="18">

``` bash
echo $DATUM_HASH
```

<div class="output stream stdout">

    582e18df5c61fdc3a839fa931e5b098ae04236f15e84c47949ccb8d382161d7f

</div>

</div>

<div class="cell code" execution_count="19">

``` bash
jq '.datum.json' $MARLOWE_FILE > $DATUM_FILE
```

</div>

<div class="cell code" execution_count="20">

``` bash
json2yaml $DATUM_FILE
```

<div class="output stream stdout">

    constructor: 0
    fields:
    - constructor: 0
      fields:
      - bytes: ''
    - constructor: 0
      fields:
      - map:
        - k:
            constructor: 0
            fields:
            - constructor: 0
              fields:
              - constructor: 0
                fields: []
              - constructor: 0
                fields:
                - constructor: 0
                  fields:
                  - bytes: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
                - constructor: 1
                  fields: []
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

</div>

</div>

<div class="cell markdown">

### Redeemer

</div>

<div class="cell code" execution_count="21">

``` bash
jq '.redeemer.json' $MARLOWE_FILE > $REDEEMER_FILE
```

</div>

<div class="cell code" execution_count="22">

``` bash
json2yaml $REDEEMER_FILE
```

<div class="output stream stdout">

    list: []

</div>

</div>

<div class="cell markdown">

## 5. Fund the contract.

Before running the contract, we need to put funds into it. Examine the
UTxOs at the wallet address:

</div>

<div class="cell code" execution_count="23">

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address $ADDRESS_P
```

<div class="output stream stdout">

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    280c4f7963729adc2bfd7afa2d45c80d1e6914308d90dfb46baf893468658bbe     0        208801084891 lovelace + TxOutDatumNone
    280c4f7963729adc2bfd7afa2d45c80d1e6914308d90dfb46baf893468658bbe     1        3000000 lovelace + TxOutDatumNone

</div>

</div>

<div class="cell markdown">

Select one of these UTxOs for use in funding the contract, naming it
`TX_0`, and then build and submit the funding transaction:

</div>

<div class="cell code" execution_count="24">

``` bash
TX_0="280c4f7963729adc2bfd7afa2d45c80d1e6914308d90dfb46baf893468658bbe#0"
```

</div>

<div class="cell code" execution_count="25">

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

<div class="output stream stdout">


    Fee: Lovelace 174521
    Size: 291 / 16384 = 1%
    Execution units:
      Memory: 0 / 14000000 = 0%
      Steps: 0 / 10000000000 = 0%
    TxId "b1b1c8d51e248728801e072545e8641daa4959778f458a3075413c7e9815e2ac"

</div>

</div>

<div class="cell markdown">

We name the funding transaction as `TX_1`.

</div>

<div class="cell code" execution_count="26">

``` bash
TX_1=b1b1c8d51e248728801e072545e8641daa4959778f458a3075413c7e9815e2ac
```

</div>

<div class="cell markdown">

After the transaction is recorded on the blockchain, there are funds at
the contract address with the data hash `DATUM_HASH`.

</div>

<div class="cell code" execution_count="27">

``` bash
echo $DATUM_HASH
```

<div class="output stream stdout">

    582e18df5c61fdc3a839fa931e5b098ae04236f15e84c47949ccb8d382161d7f

</div>

</div>

<div class="cell code" execution_count="28">

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address "$ADDRESS_S" | sed -n -e "1p; 2p; /$TX_1/p"
```

<div class="output stream stdout">

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    b1b1c8d51e248728801e072545e8641daa4959778f458a3075413c7e9815e2ac     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "582e18df5c61fdc3a839fa931e5b098ae04236f15e84c47949ccb8d382161d7f"

</div>

</div>

<div class="cell markdown">

## 6. Redeem the funds by running the contract.

We now use the previously computed redeemer and datum to remove the
funds from the contract. This involves computing the fee, building the
transaction, signing it, and submitting it.

</div>

<div class="cell code" execution_count="29">

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

<div class="output stream stdout">


    Fee: Lovelace 972116
    Size: 13060 / 16384 = 79%
    Execution units:
      Memory: 2962450 / 14000000 = 21%
      Steps: 774606677 / 10000000000 = 7%
    TxId "5f826ca0d59727e63721056cbe2afb24c6df7d67ffcb1fcc9c3014a675f96f26"

</div>

</div>

<div class="cell markdown">

We name the closing transaction as `TX_2`.

</div>

<div class="cell code" execution_count="30">

``` bash
TX_2=5f826ca0d59727e63721056cbe2afb24c6df7d67ffcb1fcc9c3014a675f96f26
```

</div>

<div class="cell markdown">

After the transaction is recorded on the blockchain, we see that the
funds were removed from the script address and are in the wallet.

</div>

<div class="cell code" execution_count="31">

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address "$ADDRESS_S" | sed -n -e "1p; 2p; /$TX_1/p; /$TX_2/p"
```

<div class="output stream stdout">

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------

</div>

</div>

<div class="cell code" execution_count="32">

``` bash
cardano-cli query utxo --testnet-magic $MAGIC --address "$ADDRESS_P" | sed -n -e "1p; 2p; /$TX_1/p; /$TX_2/p"
```

<div class="output stream stdout">

                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    5f826ca0d59727e63721056cbe2afb24c6df7d67ffcb1fcc9c3014a675f96f26     0        208796938254 lovelace + TxOutDatumNone
    5f826ca0d59727e63721056cbe2afb24c6df7d67ffcb1fcc9c3014a675f96f26     1        3000000 lovelace + TxOutDatumNone

</div>

</div>
