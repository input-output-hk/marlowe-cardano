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
f10c0f05effd32483e4ed9ccad5843c795329b04874297b78b2c21f9413fede7     0        18144098880 lovelace + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_SELLER=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$SELLER_ADDRESS"                     \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
```

Francis Beaumont will spend the UTxO `f10c0f05effd32483e4ed9ccad5843c795329b04874297b78b2c21f9413fede7#0`.

### The Buyer

```
BUYER_PREFIX="$TREASURY/thomas-kyd"
BUYER_NAME="Thomas Kyd"
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey
BUYER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
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
d6b5025613d3c0013bb0391c9c6a297f3554db43e87c2cdb7fe3ffb453378e43     0        8441499115 lovelace + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_BUYER=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$BUYER_ADDRESS"                     \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
```

Thomas Kyd will spend the UTxO `d6b5025613d3c0013bb0391c9c6a297f3554db43e87c2cdb7fe3ffb453378e43#0`.

### The Mediator

```
MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey
MEDIATOR_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
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
20c8e13709505d250bd8c345c487fb7089e2c3a390648d4f21da05f765421064     0        8961343527 lovelace + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_MEDIATOR=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$MEDIATOR_ADDRESS"                     \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
```

Christopher Marlowe will spend the UTxO `20c8e13709505d250bd8c345c487fb7089e2c3a390648d4f21da05f765421064#0`.

### Validator Script and Address

The contract has a validator script and address. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

```
CONTRACT_ADDRESS=$(
marlowe-cli export-address "${MAGIC[@]}" \
            --slot-length "$SLOT_LENGTH" \
            --slot-offset "$SLOT_OFFSET" \
)
marlowe-cli export-validator "${MAGIC[@]}"                \
                             --slot-length "$SLOT_LENGTH" \
                             --slot-offset "$SLOT_OFFSET" \
                             --out-file escrow.plutus     \
                             --print-stats
```

```console
addr_test1wq3n0x7khejv6r0k3rzjw3tfwjjxlmvc7e5zxa8z4jwc57ca4jg8m
Validator size: 13848
Validator cost: ExBudget {exBudgetCPU = ExCPU 37484307, exBudgetMemory = ExMemory 126000}
```

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 1814040. The current POSIX time implies that the tip of the blockchain should be slightly before slot 1814042. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
MINIMUM_SLOT="$TIP"
PAYMENT_DEADLINE=$(($TIP + 1 * 24 * 3600))
COMPLAINT_DEADLINE=$(($TIP + 2 * 24 * 3600))
DISPUTE_DEADLINE=$(($TIP + 3 * 24 * 3600))
MEDIATION_DEADLINE=$(($TIP + 4 * 24 * 3600))
```

* The current slot is 1814040.
* The buyer Thomas Kyd must pay before slot 1900440.
* They buyer Thomas Kyd has until slot 1986840 to complain.
* The seller Francis Beaumont has until slot 2073240 to dispute a complaint.
* The mediator Christopher Marlowe has until slot 2159640 to decide on a disputed complaint.

The contract also involves the price of the good exchanged and a minimum-ADA value.

```
MINIMUM_ADA=3000000
PRICE=256000000
```

The selling price is 256000000 lovelace.

## Transaction 1. Create the Contract by Providing the Minimum ADA.

We create the contract for the previously specified parameters.

```
marlowe-cli contract-escrow --minimum-ada "$MINIMUM_ADA"               \
                            --price "$PRICE"                           \
                            --seller "PK=$SELLER_PUBKEYHASH"           \
                            --buyer "PK=$BUYER_PUBKEYHASH"             \
                            --mediator "PK=$MEDIATOR_PUBKEYHASH"       \
                            --payment-deadline "$PAYMENT_DEADLINE"     \
                            --complaint-deadline "$COMPLAINT_DEADLINE" \
                            --dispute-deadline "$DISPUTE_DEADLINE"     \
                            --mediation-deadline "$MEDIATION_DEADLINE" \
                            --out-file tx-1.marlowe
```

We extract the initial state and full contract from the `.marlowe`file that contains comprehensive information.

```
jq '.marloweState'    tx-1.marlowe > tx-1.state
jq '.marloweContract' tx-1.marlowe > tx-1.contract
```

For each transaction, we construct the output datum. Here is its size and hash:

```
marlowe-cli export-datum --contract-file tx-1.contract \
                         --state-file    tx-1.state    \
                         --out-file      tx-1.datum    \
                         --print-stats
```

```console
Datum size: 823
6e70eb24c55ac34f97af3ecedaca00dd5c9a4139efff25f86109556272f0a581
```

The mediator Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli transaction-create "${MAGIC[@]}"                              \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                               --tx-in "$TX_0_MEDIATOR"                   \
                               --change-address "$MEDIATOR_ADDRESS"       \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                               --script-address "$CONTRACT_ADDRESS"       \
                               --tx-out-datum-file tx-1.datum             \
                               --tx-out-marlowe "$MINIMUM_ADA"            \
                               --out-file tx-1.raw                        \
                               --print-stats                              \
                               --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 206861
Size: 1028 / 32768 = 3%
Execution units:
  Memory: 0 / 10000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `1bd52b01ceb486463b5d30982b8a137c96bbf57dc0b397ffb50f8b39ae5171fc`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1bd52b01ceb486463b5d30982b8a137c96bbf57dc0b397ffb50f8b39ae5171fc     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "6e70eb24c55ac34f97af3ecedaca00dd5c9a4139efff25f86109556272f0a581"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1bd52b01ceb486463b5d30982b8a137c96bbf57dc0b397ffb50f8b39ae5171fc     0        8958136666 lovelace + TxOutDatumNone
```

## Transaction 2. Buyer Deposits Funds into Seller's Account.

First we compute the Marlowe input required to make the initial deposit by the buyer.

```
marlowe-cli input-deposit --deposit-account "PK=$SELLER_PUBKEYHASH" \
                          --deposit-party "PK=$BUYER_PUBKEYHASH"    \
                          --deposit-amount "$PRICE"                 \
                          --out-file "tx-2.input"
```

Next we compute the transition caused by that input to the contract.

```
marlowe-cli compute --contract-file tx-1.contract          \
                    --state-file    tx-1.state             \
                    --input-file    tx-2.input             \
                    --out-file      tx-2.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats
```

```console
Datum size: 771
```

As in the first transaction, we compute the new state and contract.

```
jq '.state'    tx-2.marlowe > tx-2.state
jq '.contract' tx-2.marlowe > tx-2.contract
```

Because this transaction spends from the script address, it also needs a redeemer:

```
marlowe-cli export-redeemer --input-file tx-2.input    \
                            --out-file   tx-2.redeemer \
                            --print-stats
```

```console
Redeemer size: 85
```

As in the first transaction, we compute the datum and its hash:

```
marlowe-cli export-datum --contract-file tx-2.contract \
                         --state-file    tx-2.state    \
                         --out-file      tx-2.datum    \
                         --print-stats
```

```console
Datum size: 771
215aa65b5ff2c5fe59c01c9e61782278bf5570808765a59d80a82ba920bd1cfa
```

The value held at the contract address must match that required by its state.

```
CONTRACT_VALUE_2=$(jq '.accounts | [.[][1]] | add' tx-2.state)
```

Now the buyer Thomas Kyd submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$CONTRACT_ADDRESS"      \
                                --tx-in-marlowe "$TX_1"#1                 \
                                --tx-in-script-file escrow.plutus         \
                                --tx-in-datum-file tx-1.datum             \
                                --tx-in-redeemer-file tx-2.redeemer       \
                                --tx-in "$TX_0_BUYER"                     \
                                --tx-in-collateral "$TX_0_BUYER"          \
                                --required-signer "$BUYER_PAYMENT_SKEY"   \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"      \
                                --tx-out-datum-file tx-2.datum            \
                                --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA"    \
                                --change-address "$BUYER_ADDRESS"         \
                                --invalid-before "$TIP"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"    \
                                --out-file tx-2.raw                       \
                                --print-stats                             \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 1214701
Size: 15902 / 32768 = 48%
Execution units:
  Memory: 4058844 / 10000000 = 40%
  Steps: 1528605687 / 10000000000 = 15%
```

The contract received the deposit of 256000000 lovelace from Thomas Kyd in the transaction `af06a6dd5cd1b0e390307f768fc690e10c3256daef91fdba5ec0de04a83e85a6`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
af06a6dd5cd1b0e390307f768fc690e10c3256daef91fdba5ec0de04a83e85a6     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "215aa65b5ff2c5fe59c01c9e61782278bf5570808765a59d80a82ba920bd1cfa"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
af06a6dd5cd1b0e390307f768fc690e10c3256daef91fdba5ec0de04a83e85a6     0        8181284414 lovelace + TxOutDatumNone
af06a6dd5cd1b0e390307f768fc690e10c3256daef91fdba5ec0de04a83e85a6     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli input-choose --choice-name "Report problem"        \
                         --choice-party "PK=$BUYER_PUBKEYHASH" \
                         --choice-number 1                     \
                         --out-file tx-3.input
```

As in the second transaction we compute the contract's transition, its new state, the redeemer, the datum, and the value.

```
marlowe-cli compute --contract-file tx-2.contract          \
                    --state-file    tx-2.state             \
                    --input-file    tx-3.input             \
                    --out-file      tx-3.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats
```

```console
Payment 1
  Acccount: PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"
  Payee: Account (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")
  Ada: 256.000000
Datum size: 572
```

```
jq '.state'    tx-3.marlowe > tx-3.state
jq '.contract' tx-3.marlowe > tx-3.contract
marlowe-cli export-redeemer --input-file tx-3.input    \
                            --out-file   tx-3.redeemer \
                            --print-stats
```

```console
Redeemer size: 60
```

```
marlowe-cli export-datum --contract-file tx-3.contract \
                         --state-file    tx-3.state    \
                         --out-file      tx-3.datum    \
                         --print-stats
```

```console
Datum size: 572
c3ee0c56e41ce5014a9878ad907258e7afd55e7abd4c629c64599a755c2d73ed
```

Now the buyer Thomas Kyd can submit a transaction to report that there is a problem:

```
TX_3=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$CONTRACT_ADDRESS"      \
                                --tx-in-marlowe "$TX_2"#1                 \
                                --tx-in-script-file escrow.plutus         \
                                --tx-in-datum-file tx-2.datum             \
                                --tx-in-redeemer-file tx-3.redeemer       \
                                --tx-in "$TX_2"#0                         \
                                --tx-in-collateral "$TX_2"#0              \
                                --required-signer "$BUYER_PAYMENT_SKEY"   \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"      \
                                --tx-out-datum-file tx-3.datum            \
                                --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA"    \
                                --change-address "$BUYER_ADDRESS"         \
                                --invalid-before "$TIP"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"    \
                                --out-file tx-3.raw                       \
                                --print-stats                             \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 1296155
Size: 15626 / 32768 = 47%
Execution units:
  Memory: 5260772 / 10000000 = 52%
  Steps: 1864890250 / 10000000000 = 18%
```

The reporting of a problem was recorded in the transaction `83b1169a5d82458338f465ed1a35166998622dd01a76c7f70542f4853830143e`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
83b1169a5d82458338f465ed1a35166998622dd01a76c7f70542f4853830143e     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "c3ee0c56e41ce5014a9878ad907258e7afd55e7abd4c629c64599a755c2d73ed"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
83b1169a5d82458338f465ed1a35166998622dd01a76c7f70542f4853830143e     0        8176988259 lovelace + TxOutDatumNone
83b1169a5d82458338f465ed1a35166998622dd01a76c7f70542f4853830143e     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 4. The Seller Disputes that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli input-choose --choice-name "Dispute problem"        \
                         --choice-party "PK=$SELLER_PUBKEYHASH" \
                         --choice-number 0                      \
                         --out-file tx-4.input
```

As always we compute the contract's transition, its new state, the redeemer, the datum, and the value.

```
marlowe-cli compute --contract-file tx-3.contract          \
                    --state-file    tx-3.state             \
                    --input-file    tx-4.input             \
                    --out-file      tx-4.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats
```

```console
Datum size: 470
```

```
jq '.state'    tx-4.marlowe > tx-4.state
jq '.contract' tx-4.marlowe > tx-4.contract
marlowe-cli export-redeemer --input-file tx-4.input    \
                            --out-file   tx-4.redeemer \
                            --print-stats
```

```console
Redeemer size: 61
```

```
marlowe-cli export-datum --contract-file tx-4.contract \
                         --state-file    tx-4.state    \
                         --out-file      tx-4.datum    \
                         --print-stats
```

```console
Datum size: 470
bd83035f40fac457be61bfe1518fd678f817c7b3adf0b99959281954221f8ed1
```

Now the seller Francis Beaumont can submit a transaction to dispute that there is a problem:

```
TX_4=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$CONTRACT_ADDRESS"      \
                                --tx-in-marlowe "$TX_3"#1                 \
                                --tx-in-script-file escrow.plutus         \
                                --tx-in-datum-file tx-3.datum             \
                                --tx-in-redeemer-file tx-4.redeemer       \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"      \
                                --tx-out-datum-file tx-4.datum            \
                                --tx-in "$TX_0_SELLER"                    \
                                --tx-in-collateral "$TX_0_SELLER"         \
                                --required-signer "$SELLER_PAYMENT_SKEY"  \
                                --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA"   \
                                --change-address "$SELLER_ADDRESS"        \
                                --invalid-before "$TIP"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"    \
                                --out-file tx-4.raw                       \
                                --print-stats                             \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 1157949
Size: 15326 / 32768 = 46%
Execution units:
  Memory: 3736524 / 10000000 = 37%
  Steps: 1350925438 / 10000000000 = 13%
```

The dispute that this is a problem is recorded in the transaction `47f0117f8209ee29ee38580d8f452e708056d5e3f3d9056204491f8e11c8cb8a`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
47f0117f8209ee29ee38580d8f452e708056d5e3f3d9056204491f8e11c8cb8a     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "bd83035f40fac457be61bfe1518fd678f817c7b3adf0b99959281954221f8ed1"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
47f0117f8209ee29ee38580d8f452e708056d5e3f3d9056204491f8e11c8cb8a     0        18139940931 lovelace + TxOutDatumNone
47f0117f8209ee29ee38580d8f452e708056d5e3f3d9056204491f8e11c8cb8a     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 5. The Mediator Dismisses the Claim

Funds are released to the seller and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli input-choose --choice-name "Dismiss claim"            \
                         --choice-party "PK=$MEDIATOR_PUBKEYHASH" \
                         --choice-number 0                        \
                         --out-file tx-5.input
```

As previously we compute the contract's transition, its new state, and the redeemer. Because the contract is being closed, no new datum need be computed.

```
marlowe-cli compute --contract-file tx-4.contract          \
                    --state-file    tx-4.state             \
                    --input-file    tx-5.input             \
                    --out-file      tx-5.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats
```

```console
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
Datum size: 181
```

```
jq '.state'    tx-5.marlowe > tx-5.state
jq '.contract' tx-5.marlowe > tx-5.contract
marlowe-cli export-redeemer --input-file tx-5.input    \
                            --out-file   tx-5.redeemer \
                            --print-stats
```

```console
Redeemer size: 59
```

Now the mediator Christopher Marlowe can submit a transaction to release funds:

```
TX_5=$(
marlowe-cli transaction-close "${MAGIC[@]}"                              \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                              --tx-in-marlowe "$TX_4"#1                  \
                              --tx-in-script-file escrow.plutus          \
                              --tx-in-datum-file tx-4.datum              \
                              --tx-in-redeemer-file tx-5.redeemer        \
                              --tx-in "$TX_1"#0                          \
                              --tx-in-collateral "$TX_1"#0               \
                              --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                              --tx-out "$SELLER_ADDRESS+$PRICE"          \
                              --change-address "$MEDIATOR_ADDRESS"       \
                              --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA"  \
                              --invalid-before "$TIP"                    \
                              --invalid-hereafter "$(($TIP+4*3600))"     \
                              --out-file tx-5.raw                        \
                              --print-stats                              \
                              --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 1240920
Size: 14718 / 32768 = 44%
Execution units:
  Memory: 5160020 / 10000000 = 51%
  Steps: 1733561197 / 10000000000 = 17%
```


The dismissal of the claim resulted in closing the contract, paying 256000000 lovelace to the the seller Francis Beaumont and 3000000 lovelace to the mediator Christopher Marlowe in the transaction `9ac2be01ed87faff000f9e3f42f415813263f4692c2ddde4ee10ff9bd81c2188`.  There is no UTxO at the contract address:

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
9ac2be01ed87faff000f9e3f42f415813263f4692c2ddde4ee10ff9bd81c2188     1        256000000 lovelace + TxOutDatumNone
```

Here is the UTxO at the buyer Thomas Kyd's address:

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
9ac2be01ed87faff000f9e3f42f415813263f4692c2ddde4ee10ff9bd81c2188     0        8956895746 lovelace + TxOutDatumNone
9ac2be01ed87faff000f9e3f42f415813263f4692c2ddde4ee10ff9bd81c2188     2        3000000 lovelace + TxOutDatumNone
```

## Clean Up Wallets

It's convenient to consolidate all of the UTxOs into single ones.

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" --out-file /dev/stdout  \
| jq '. | to_entries[] | .key'                                                           \
| sed -e 's/"//g;s/^/--tx-in /'                                                          \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                             \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                               --change-address "$SELLER_ADDRESS"        \
                                               --out-file tx-6.raw                       \
                                               --required-signer "$SELLER_PAYMENT_SKEY"  \
                                               --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" --out-file /dev/stdout   \
| jq '. | to_entries[] | .key'                                                           \
| sed -e 's/"//g;s/^/--tx-in /'                                                          \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                             \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                               --change-address "$BUYER_ADDRESS"         \
                                               --out-file tx-7.raw                       \
                                               --required-signer "$BUYER_PAYMENT_SKEY"   \
                                               --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" --out-file /dev/stdout \
| jq '. | to_entries[] | .key'                                                            \
| sed -e 's/"//g;s/^/--tx-in /'                                                           \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                              \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                                               --change-address "$MEDIATOR_ADDRESS"       \
                                               --out-file tx-8.raw                        \
                                               --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                                               --submit=600                               \
> /dev/null
