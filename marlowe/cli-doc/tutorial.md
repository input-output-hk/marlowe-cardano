# Marlowe CLI Tutorial: Granular Workflow

The granular workflow for `marlowe-cli` follows the data flow in the diagram below. The address, validator, datum, and redeemer for a transaction are built separately, but then combined into a transaction.

![Marlowe workflow using `marlowe-cli` and `cardano-cli`.](diagrams/workflow.svg)

This tutorial is embodied in the `bash` script [example.sh](example.sh).


## 1. Select network.

Make sure that `marlowe-cli`, `cardano-cli`, and `jq` have been installed on the path for the `bash` shell. Set the environment variable `CARDANO_NODE_SOCKET_PATH` to point to the location of the socket for the `cardano-node` service. In this tutorial, we use the public `testnet`:

    NETWORK=testnet
    MAGIC="--testnet-magic 1097911063"
    CARDANO_NODE_SOCKET_PATH=$PWD/$NETWORK.socket


## 2. Select wallet.

Select a wallet for use in this tutorial and specify the files with the signing and payment keys. The address of this wallet is stored in the environment variable `ADDRESS_P`.

    PAYMENT_SKEY=payment.skey
    PAYMENT_VKEY=payment.vkey
    ADDRESS_P=$(cardano-cli address build $MAGIC --payment-verification-key-file $PAYMENT_VKEY)
    PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


## 3. Design the Marlowe contract.

First, we choose names for the files containing the validator, datum, and redeemer.

    PLUTUS_FILE=test.plutus
    DATUM_FILE=test.datum
    REDEEMER_FILE=test.redeemer

We just use the simplest contract, `Close`, which is serialised in [example.contract](example.contract). We use JSON files for the contract and its current state:

    CONTRACT_FILE=example.contract
    STATE_FILE=example.state

We will put 3 ADA into the account for the wallet, as recorded in the contract's state:

    DATUM_LOVELACE=3000000
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
        "minSlot": 10,
        "boundValues": []
    }
    EOI

We will redeem the ADA within a particular range of slots:

    REDEEM_MIN_SLOT=1000
    REDEEM_MAX_SLOT=43500000


## Create the validator, datum, and redeemer.

We now create the Plutus script for the contract and compute its script address:

    ADDRESS_S=$(marlowe-cli validator $MAGIC --out-file $PLUTUS_FILE)

This default Marlowe validator has the following address:

    $ echo $ADDRESS_S
    
    addr_test1wqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6q2ng95l

The contract and its state form the datum, which we also compute:

    DATUM_HASH=$(
    marlowe-cli datum --contract-file $CONTRACT_FILE \
                      --state-file $STATE_FILE       \
                      --out-file $DATUM_FILE
    )

Since this is the `Close` contract, the redeemer needs no input aside from a slot range:

    marlowe-cli redeemer --out-file $REDEEMER_FILE


## Fund the contract.

Before running the contract, we need to put funds into it. Examine the UTxOs at the wallet address:

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_P
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    3ed9cbe11b6308c5ede3ca8c9eb3a7ba1d7fe00a958dceb029f6c6219180235f     0        980094849 lovelace + TxOutDatumNone

Select one of these UTxOs for use in funding the contract, naming it `TX_0`, and then build and submit the funding transaction:

    TX_0=3ed9cbe11b6308c5ede3ca8c9eb3a7ba1d7fe00a958dceb029f6c6219180235f
    
    marlowe-cli create $MAGIC                                  \
                       --socket-path $CARDANO_NODE_SOCKET_PATH \
                       --script-address $ADDRESS_S             \
                       --tx-out-datum-file $DATUM_FILE         \
                       --tx-out-value $DATUM_LOVELACE          \
                       --tx-in $TX_0#0                         \
                       --change-address $ADDRESS_P             \
                       --out-file tx.raw
    
    marlowe-cli submit $MAGIC                                  \
                       --socket-path $CARDANO_NODE_SOCKET_PATH \
                       --required-signer $PAYMENT_SKEY         \
                       --tx-body-file tx.raw


After the transaction is recorded on the blockchain, there are funds at the contract address with the data hash `DATUM_HASH`.

    $ echo $DATUM_HASH
    
    0c050b99438fcd2c65c54b062338f3692c212cbfb499cfe3ad6a9a07ce15dbc0
    
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    9c6d992735fd68ebf4e689ca75160007ffbdb584d4d908a1ab763d4d764eed13     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "0c050b99438fcd2c65c54b062338f3692c212cbfb499cfe3ad6a9a07ce15dbc0"

We name the funding transaction as `TX_1`.

    TX_1=9c6d992735fd68ebf4e689ca75160007ffbdb584d4d908a1ab763d4d764eed13


## Redeem the funds by running the contract.

We now use the previously computed redeemer and datum to remove the funds from the contract. This involves computing the fee, building the transaction, signing it, and submitting it.

    marlowe-cli close $MAGIC                                  \
                      --socket-path $CARDANO_NODE_SOCKET_PATH \
                      --tx-in-script-file $PLUTUS_FILE        \
                      --tx-in-redeemer-file $REDEEMER_FILE    \
                      --tx-in-datum-file $DATUM_FILE          \
                      --tx-in-marlowe $TX_1#1                 \
                      --tx-in $TX_1#0                         \
                      --tx-in-collateral $TX_1#0              \
                      --tx-out $ADDRESS_P+$DATUM_LOVELACE     \
                      --change-address $ADDRESS_P             \
                      --invalid-before $REDEEM_MIN_SLOT       \
                      --invalid-hereafter $REDEEM_MAX_SLOT    \
                      --out-file tx.raw
    
    marlowe-cli submit $MAGIC                                  \
                       --socket-path $CARDANO_NODE_SOCKET_PATH \
                       --required-signer $PAYMENT_SKEY         \
                       --tx-body-file tx.raw


After the transaction is recorded on the blockchain, we see that the funds were removed from the script address and are in the wallet.

    $ cardano-cli query utxo $MAGIC --address $ADDRESS_S
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    
    $ cardano-cli query utxo $MAGIC --address $ADDRESS_P
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    0dbfbe3221882b1f70a555dce4f1e10b4a5afab67d7be7231d91eac5a25f8aac     0        975882239 lovelace + TxOutDatumNone
    0dbfbe3221882b1f70a555dce4f1e10b4a5afab67d7be7231d91eac5a25f8aac     1        3000000 lovelace + TxOutDatumNone


Voilà! See <https://testnet.cardanoscan.io/transaction/0dbfbe3221882b1f70a555dce4f1e10b4a5afab67d7be7231d91eac5a25f8aac>.
