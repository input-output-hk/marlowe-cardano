# Example Escrow Contract: "Dismiss Claim"

In this example execution of [an escrow contract](ReadMe.md), the buyer reports a problem, the seller disputes the problem, and the mediator dismisses the buyer's claim.

![Flow chart for "dismiss claim".](dismiss-claim.svg)

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed
* xargs

Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `PARTY_PREFIX` where they appear below.

## Preliminaries

### Select Network

```
if false
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
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0407579ed4e0718cad128967f5987bacce8a6fd0f8f002ad26d1d20e460e3b7e     2        1000000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FrancisBeaumont + TxOutDatumNone
0ef2403cbf55679e07d38046eaed0593caad21a2634922a28c9e9c1defac9e70     0        21618792556 lovelace + TxOutDatumNone
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

Francis Beaumont will spend the UTxO `0ef2403cbf55679e07d38046eaed0593caad21a2634922a28c9e9c1defac9e70#0`.

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
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
37621a20446324ab966e7861d0ac5cde90f9d387b452d691ed2abbcf1d890c99     0        4017173779 lovelace + TxOutDatumNone
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

Thomas Kyd will spend the UTxO `37621a20446324ab966e7861d0ac5cde90f9d387b452d691ed2abbcf1d890c99#0`.

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
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0407579ed4e0718cad128967f5987bacce8a6fd0f8f002ad26d1d20e460e3b7e     5        1000000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ChristopherMarlowe + TxOutDatumNone
b618365334cf3263d37adf7c177c8e4091e390109038bff28e58dae5071d0766     0        8926107247 lovelace + TxOutDatumNone
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

Christopher Marlowe will spend the UTxO `b618365334cf3263d37adf7c177c8e4091e390109038bff28e58dae5071d0766#0`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 2672120. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2672121. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
MINIMUM_SLOT="$TIP"
PAYMENT_DEADLINE=$(($TIP + 1 * 24 * 3600))
COMPLAINT_DEADLINE=$(($TIP + 2 * 24 * 3600))
DISPUTE_DEADLINE=$(($TIP + 3 * 24 * 3600))
MEDIATION_DEADLINE=$(($TIP + 4 * 24 * 3600))
```

* The current slot is 2672120.
* The buyer Thomas Kyd must pay before slot 2758520.
* They buyer Thomas Kyd has until slot 2844920 to complain.
* The seller Francis Beaumont has until slot 2931320 to dispute a complaint.
* The mediator Christopher Marlowe has until slot 3017720 to decide on a disputed complaint.

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
CONTRACT_ADDRESS=$(jq -r '.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wq3n0x7khejv6r0k3rzjw3tfwjjxlmvc7e5zxa8z4jwc57ca4jg8m`.

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
Fee: Lovelace 212625
Size: 1058 / 32768 = 3%
Execution units:
  Memory: 0 / 10000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `968dfe9c1f701756d021cc73c935e2d53002d59409c85d6571a79376a1a03c3a`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
968dfe9c1f701756d021cc73c935e2d53002d59409c85d6571a79376a1a03c3a     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "e29a057aad7e7d3f5418c83f77204b3c94697558ceda44e8770dd1c1daaa0465"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
968dfe9c1f701756d021cc73c935e2d53002d59409c85d6571a79376a1a03c3a     0        8922894622 lovelace + TxOutDatumNone
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
Size: 15861 / 32768 = 48%
Execution units:
  Memory: 3975476 / 10000000 = 39%
  Steps: 1494117513 / 10000000000 = 14%
```

The contract received the deposit of 256000000 lovelace from Thomas Kyd in the transaction `dbac322f1ba1cbd89c9f6b9efe5833fd69b9a7edf371125027768416fe4956d2`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
dbac322f1ba1cbd89c9f6b9efe5833fd69b9a7edf371125027768416fe4956d2     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "c2e2e1b099cea853636712462e69dcab9cf96cdbeb11b25660a30a4f5d494c6c"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
dbac322f1ba1cbd89c9f6b9efe5833fd69b9a7edf371125027768416fe4956d2     0        3759968179 lovelace + TxOutDatumNone
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
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 1287054
Size: 15585 / 32768 = 47%
Execution units:
  Memory: 5177404 / 10000000 = 51%
  Steps: 1830402076 / 10000000000 = 18%
```

The reporting of a problem was recorded in the transaction `9b651466065698633505defcdaadd0c55849cd46bd5af2d605a1ebdf22783599`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9b651466065698633505defcdaadd0c55849cd46bd5af2d605a1ebdf22783599     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "0f88e107b8826e9231a159008ffbaa81d5e235853f743050b6cc2ca9c850ec8b"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9b651466065698633505defcdaadd0c55849cd46bd5af2d605a1ebdf22783599     0        3758681125 lovelace + TxOutDatumNone
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
Fee: Lovelace 1149024
Size: 15289 / 32768 = 46%
Execution units:
  Memory: 3653156 / 10000000 = 36%
  Steps: 1316437264 / 10000000000 = 13%
```

The dispute that this is a problem is recorded in the transaction `d9b491ac5f5128c09b83c477320b4aaccd37577bd971bbcd3e5ac7acbd69ce3c`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d9b491ac5f5128c09b83c477320b4aaccd37577bd971bbcd3e5ac7acbd69ce3c     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "0069958d480166055349c90f71c06572b79ff76514d0ce20159fcbe65bd51dc5"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d9b491ac5f5128c09b83c477320b4aaccd37577bd971bbcd3e5ac7acbd69ce3c     0        21617643532 lovelace + TxOutDatumNone
```

## Transaction 5. The Mediator Dismisses the Claim

Funds are released to the seller and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe              \
                        --choice-name "Dismiss claim"            \
                        --choice-party "PK=$MEDIATOR_PUBKEYHASH" \
                        --choice-number 0                        \
                        --invalid-before "$TIP"                  \
                        --invalid-hereafter "$(($TIP+4*3600))"   \
                        --out-file tx-5.marlowe                  \
                        --print-stats
```

```console
Datum size: 181
Payment 1
  Acccount: PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79"
  Payee: Account (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
  Ada: 256.000000
Payment 2
  Acccount: PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274"
  Payee: Party (PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274")
  Ada: 3.000000
Payment 3
  Acccount: PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"
  Payee: Party (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
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
Fee: Lovelace 1240920
Size: 14718 / 32768 = 44%
Execution units:
  Memory: 5160020 / 10000000 = 51%
  Steps: 1733561197 / 10000000000 = 17%
```

The dismissal of the claim resulted in closing the contract, paying 256000000 lovelace to the the seller Francis Beaumont and 3000000 lovelace to the mediator Christopher Marlowe in the transaction `49eff860937369f598c6694b69770fcc03b710c172b8ee6592e6a69916cead8b`.  There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
49eff860937369f598c6694b69770fcc03b710c172b8ee6592e6a69916cead8b     2        256000000 lovelace + TxOutDatumNone
```

There is no UTxO at the buyer Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
49eff860937369f598c6694b69770fcc03b710c172b8ee6592e6a69916cead8b     0        8921653702 lovelace + TxOutDatumNone
49eff860937369f598c6694b69770fcc03b710c172b8ee6592e6a69916cead8b     1        3000000 lovelace + TxOutDatumNone
```

## Clean Up Wallets

It's convenient to consolidate all of the UTxOs into single ones.

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" --out-file /dev/stdout                         \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| sed -e 's/^/--tx-in /'                                                                                        \
| xargs -n 9999 marlowe-cli transaction simple "${MAGIC[@]}"                                                    \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                        \
                                               --change-address "$SELLER_ADDRESS"                               \
                                               --out-file tx-6.raw                                              \
                                               --required-signer "$SELLER_PAYMENT_SKEY"                         \
                                               --submit=600                                                     \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| sed -e 's/^/--tx-in /'                                                                                        \
| xargs -n 9999 marlowe-cli transaction simple "${MAGIC[@]}"                                                    \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                        \
                                               --change-address "$BUYER_ADDRESS"                                \
                                               --out-file tx-7.raw                                              \
                                               --required-signer "$BUYER_PAYMENT_SKEY"                          \
                                               --submit=600                                                     \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" --out-file /dev/stdout                       \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| sed -e 's/^/--tx-in /'                                                                                        \
| xargs -n 9999 marlowe-cli transaction simple "${MAGIC[@]}"                                                    \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                        \
                                               --change-address "$MEDIATOR_ADDRESS"                             \
                                               --out-file tx-8.raw                                              \
                                               --required-signer "$MEDIATOR_PAYMENT_SKEY"                       \
                                               --submit=600                                                     \
> /dev/null
