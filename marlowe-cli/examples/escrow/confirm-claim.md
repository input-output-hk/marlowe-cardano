# Example Escrow Contract: "Confirm Claim"

In this example execution of [an escrow contract](ReadMe.md), the buyer reports a problem, the seller disputes the problem, but the mediator confirms the problem.

![Flow chart for "confirm claim".](confirm-claim.svg)

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `PARTY_PREFIX` where they appear below.

## Preliminaries

### Select Network

```
if true
then # Use the public testnet.
  MAGIC=(--testnet-magic 1097911063)
  SLOT_LENGTH=1000
  SLOT_OFFSET=1594369216000
else # Use the private testnet.
  MAGIC=(--testnet-magic 1564)
  SLOT_LENGTH=1000
  SLOT_OFFSET=1638215277000
fi
```

### Select Parties

#### The Seller

The seller sells an item for a price.

```
SELLER_PREFIX="$TREASURY/francis-beaumont"
SELLER_NAME="Francis Beaumont"
SELLER_PAYMENT_SKEY="$SELLER_PREFIX".skey
SELLER_PAYMENT_VKEY="$SELLER_PREFIX".vkey
SELLER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$SELLER_PAYMENT_VKEY" \
)
SELLER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$SELLER_PAYMENT_VKEY"
)
```

The seller Francis Beaumont has the address `addr_test1vrtntkszteptml4e9ce9l3fsmgavwv4ywunvdnhxv6nw5ksq6737a` and public-key hash `d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$SELLER_PAYMENT_SKEY"  \
                       --change-address "$SELLER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
73c47c0b734bde5e7a8ffa179ee46ee56c3d1b189afb77f0f03f282427e8123e     0        2504289507 lovelace + TxOutDatumNone
73c47c0b734bde5e7a8ffa179ee46ee56c3d1b189afb77f0f03f282427e8123e     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FrancisBeaumont + TxOutDatumNone
```

We select the tokenless UTxO with the most funds to use in executing the contract.

```
TX_0_SELLER=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$SELLER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)
```

Francis Beaumont will spend the UTxO `73c47c0b734bde5e7a8ffa179ee46ee56c3d1b189afb77f0f03f282427e8123e#0`.

### The Buyer

```
BUYER_PREFIX="$TREASURY/thomas-kyd"
BUYER_NAME="Thomas Kyd"
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey
BUYER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                         \
                            --payment-verification-key-file "$BUYER_PAYMENT_VKEY" \
)
BUYER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$BUYER_PAYMENT_VKEY"
)
```

The buyer Thomas Kyd has the address `addr_test1vpkedn0l6slaj8fzkx08qyce9vpreuu3spmnlxkepx4757grxzyzf` and public-key hash `6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$BUYER_PAYMENT_SKEY"   \
                       --change-address "$BUYER_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3cc9959da87bbb75f837c6e77abcaeef0300f7a3ef2e16afad1ab2642ed3901f     0        1478475777 lovelace + TxOutDatumNone
```

We select the tokenless UTxO with the most funds to use in executing the contract.

```
TX_0_BUYER=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$BUYER_ADDRESS"                                                               \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)
```

Thomas Kyd will spend the UTxO `3cc9959da87bbb75f837c6e77abcaeef0300f7a3ef2e16afad1ab2642ed3901f#0`.

### The Mediator

```
MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey
MEDIATOR_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                            \
                            --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY" \
)
MEDIATOR_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY"
)
```

The mediator Christopher Marlowe has the address `addr_test1vqhqudxtwqcpjqesns79hqgqq2q0xx5q0hnzz5es9492yaqpxltpy` and public-key hash `2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                              \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                       --change-address "$MEDIATOR_ADDRESS"       \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f7a4daefb73b21a49071b01729d2f8a16ec89ed898beb0ba212facb08ead7b0c     0        1993129855 lovelace + TxOutDatumNone
f7a4daefb73b21a49071b01729d2f8a16ec89ed898beb0ba212facb08ead7b0c     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ChristopherMarlowe + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_MEDIATOR=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$MEDIATOR_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)
```

Christopher Marlowe will spend the UTxO `f7a4daefb73b21a49071b01729d2f8a16ec89ed898beb0ba212facb08ead7b0c#0`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 46721448. The current POSIX time implies that the tip of the blockchain should be slightly before slot 46721453. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
MINIMUM_SLOT="$TIP"
PAYMENT_DEADLINE=$(($TIP + 1 * 24 * 3600))
COMPLAINT_DEADLINE=$(($TIP + 2 * 24 * 3600))
DISPUTE_DEADLINE=$(($TIP + 3 * 24 * 3600))
MEDIATION_DEADLINE=$(($TIP + 4 * 24 * 3600))
```

* The current slot is 46721448.
* The buyer Thomas Kyd must pay before slot 46807848.
* They buyer Thomas Kyd has until slot 46894248 to complain.
* The seller Francis Beaumont has until slot 46980648 to dispute a complaint.
* The mediator Christopher Marlowe has until slot 47067048 to decide on a disputed complaint.

The contract also involves the price of the good exchanged and a minimum-ADA value.

```
MINIMUM_ADA=3000000
PRICE=256000000
```

The selling price is 256000000 lovelace.

We create the contract for the previously specified parameters.

```
marlowe-cli template escrow --minimum-ada "$MINIMUM_ADA"               \
                            --price "$PRICE"                           \
                            --seller "PK=$SELLER_PUBKEYHASH"           \
                            --buyer "PK=$BUYER_PUBKEYHASH"             \
                            --mediator "PK=$MEDIATOR_PUBKEYHASH"       \
                            --payment-deadline "$PAYMENT_DEADLINE"     \
                            --complaint-deadline "$COMPLAINT_DEADLINE" \
                            --dispute-deadline "$DISPUTE_DEADLINE"     \
                            --mediation-deadline "$MEDIATION_DEADLINE" \
                            --out-contract-file tx-1.contract          \
                            --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA.

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

```
marlowe-cli run initialize "${MAGIC[@]}"                 \
                           --slot-length "$SLOT_LENGTH"  \
                           --slot-offset "$SLOT_OFFSET"  \
                           --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.marlowe  \
                           --print-stats
```

```console
Validator size: 13848
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 37484307, exBudgetMemory = ExMemory 126000}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wz8xaj4mma6z39u5hpgh6zr2hsu535g4nuvhtde254ulavqqcrfj6`.

The mediator Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --tx-in "$TX_0_MEDIATOR"                   \
                        --change-address "$MEDIATOR_ADDRESS"       \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe            \
                        --out-file tx-1.raw                        \
                        --print-stats                              \
                        --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)
```

```console
Fee: Lovelace 212449
Size: 1054 / 16384 = 6%
Execution units:
  Memory: 0 / 12500000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `8dde57846df5eac598e8ef74fc856351a3382b5b96b3f68b0e883b7be0fa3778`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8dde57846df5eac598e8ef74fc856351a3382b5b96b3f68b0e883b7be0fa3778     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "a2aaa6df22bfc285e30381c35f7d47ebc2fdb238c128a486c71890a68e5389c4"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8dde57846df5eac598e8ef74fc856351a3382b5b96b3f68b0e883b7be0fa3778     0        1989917406 lovelace + TxOutDatumNone
```

## Transaction 2. Buyer Deposits Funds into Seller's Account.

First we compute the Marlowe input required to make the initial deposit by the buyer.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe               \
                        --deposit-account "PK=$SELLER_PUBKEYHASH" \
                        --deposit-party "PK=$BUYER_PUBKEYHASH"    \
                        --deposit-amount "$PRICE"                 \
                        --invalid-before "$TIP"                   \
                        --invalid-hereafter "$(($TIP+4*3600))"    \
                        --out-file tx-2.marlowe                   \
                        --print-stats
```

```console
Datum size: 771
```

Now the buyer Thomas Kyd submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_0_BUYER"          \
                        --tx-in "$TX_0_BUYER"                     \
                        --required-signer "$BUYER_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$BUYER_ADDRESS"         \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1205600
Size: 15861 / 16384 = 96%
Execution units:
  Memory: 3975476 / 12500000 = 31%
  Steps: 1494117513 / 10000000000 = 14%
```

The contract received the deposit of 256000000 lovelace from Thomas Kyd in the transaction `9fff3f78debe15f3eeab223e8054b1184442c6cd99656e3b03267744e870a2be`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9fff3f78debe15f3eeab223e8054b1184442c6cd99656e3b03267744e870a2be     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b8a941e9b63b8ec4dd7b9c714eaa56ca6059ce1a2cf3cc0831a892d118286c39"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9fff3f78debe15f3eeab223e8054b1184442c6cd99656e3b03267744e870a2be     0        1221270177 lovelace + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe            \
                        --choice-name "Report problem"         \
                        --choice-party "PK=$BUYER_PUBKEYHASH"  \
                        --choice-number 1                      \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-3.marlowe                \
                        --print-stats
```

```console
Datum size: 572
Payment 1
  Acccount: PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"
  Payee: Account (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")
  Ada: 256.000000
```

Now the buyer Thomas Kyd can submit a transaction to report that there is a problem:

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_2"#0              \
                        --tx-in "$TX_2"#0                         \
                        --required-signer "$BUYER_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$BUYER_ADDRESS"         \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1287054
Size: 15585 / 16384 = 95%
Execution units:
  Memory: 5177404 / 12500000 = 41%
  Steps: 1830402076 / 10000000000 = 18%
```

The reporting of a problem was recorded in the transaction `5d49d2852b23ec09dda3f6c0ab6c507719d7fc3b03cc0d0eb3880c5b1efe4356`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5d49d2852b23ec09dda3f6c0ab6c507719d7fc3b03cc0d0eb3880c5b1efe4356     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "25c941da75c8ad57e3bfccc17ae767d9bbe665cb3095b023b25c67598a1dde2b"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5d49d2852b23ec09dda3f6c0ab6c507719d7fc3b03cc0d0eb3880c5b1efe4356     0        1219983123 lovelace + TxOutDatumNone
```

## Transaction 4. The Seller Disputes that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe            \
                        --choice-name "Dispute problem"        \
                        --choice-party "PK=$SELLER_PUBKEYHASH" \
                        --choice-number 0                      \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-4.marlowe                \
                        --print-stats
```

```console
Datum size: 470
```

Now the seller Francis Beaumont can submit a transaction to dispute that there is a problem:

```
TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-3.marlowe            \
                        --tx-in-marlowe "$TX_3"#1                 \
                        --tx-in-collateral "$TX_0_SELLER"         \
                        --tx-in "$TX_0_SELLER"                    \
                        --required-signer "$SELLER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-4.marlowe           \
                        --change-address "$SELLER_ADDRESS"        \
                        --out-file tx-4.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1148848
Size: 15285 / 16384 = 93%
Execution units:
  Memory: 3653156 / 12500000 = 29%
  Steps: 1316437264 / 10000000000 = 13%
```

The dispute that this is a problem is recorded in the transaction `0da8c234a77c857adecc8e0ba48d8d8170a6991e430103bfdf47ce484fdba041`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0da8c234a77c857adecc8e0ba48d8d8170a6991e430103bfdf47ce484fdba041     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "e7d5a3825247261e89f8c49f414ce2b01301dbac35884a2ac7bca611b9099258"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0da8c234a77c857adecc8e0ba48d8d8170a6991e430103bfdf47ce484fdba041     0        2503140659 lovelace + TxOutDatumNone
```

## Transaction 5. The Mediator Confirms the Claim

Funds are released to the buyer and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe              \
                        --choice-name "Confirm claim"            \
                        --choice-party "PK=$MEDIATOR_PUBKEYHASH" \
                        --choice-number 1                        \
                        --invalid-before "$TIP"                  \
                        --invalid-hereafter "$(($TIP+4*3600))"   \
                        --out-file tx-5.marlowe                  \
                        --print-stats
```

```console
Datum size: 181
Payment 1
  Acccount: PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274"
  Payee: Party (PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274")
  Ada: 3.000000
Payment 2
  Acccount: PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79"
  Payee: Party (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")
  Ada: 256.000000
```

Now the mediator Christopher Marlowe can submit a transaction to release funds:

```
TX_5=$(
marlowe-cli run execute "${MAGIC[@]}"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --marlowe-in-file tx-4.marlowe             \
                        --tx-in-marlowe "$TX_4"#1                  \
                        --tx-in-collateral "$TX_1"#0               \
                        --tx-in "$TX_1"#0                          \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                        --marlowe-out-file tx-5.marlowe            \
                        --change-address "$MEDIATOR_ADDRESS"       \
                        --out-file tx-5.raw                        \
                        --print-stats                              \
                        --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)
```

```console
Fee: Lovelace 1139220
Size: 14714 / 16384 = 89%
Execution units:
  Memory: 3888228 / 12500000 = 31%
  Steps: 1343239352 / 10000000000 = 13%
```

The confirmation of the claim resulted in closing the contract, paying 256000000 lovelace to the buyer Thomas Kyd and 3000000 lovelace to the mediator Christopher Marlowe in the transaction `41888ecc4a770f2c37729d0b1d2b945004867f44154174dc2218a267a433c186`.  There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

There is no UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the buyer Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
41888ecc4a770f2c37729d0b1d2b945004867f44154174dc2218a267a433c186     2        256000000 lovelace + TxOutDatumNone
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
41888ecc4a770f2c37729d0b1d2b945004867f44154174dc2218a267a433c186     0        1988778186 lovelace + TxOutDatumNone
41888ecc4a770f2c37729d0b1d2b945004867f44154174dc2218a267a433c186     1        3000000 lovelace + TxOutDatumNone
```

