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
9ea986d2c4c18a605963d8d2e9c52164afb7523212b9dc2ff8688a3f40289639     0        20392992606 lovelace + TxOutDatumNone
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

Francis Beaumont will spend the UTxO `9ea986d2c4c18a605963d8d2e9c52164afb7523212b9dc2ff8688a3f40289639#0`.

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
4db037e285102fd992ef43c0ca64589a5c2cd589bcd7c8f65f57cab719bf24f7     0        5325287683 lovelace + TxOutDatumNone
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

Thomas Kyd will spend the UTxO `4db037e285102fd992ef43c0ca64589a5c2cd589bcd7c8f65f57cab719bf24f7#0`.

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
63fb1d7fd884a67e5e950dd19cdcd40abb02d8ed2f65dee976cf9759c4cc4ba0     0        8934861844 lovelace + TxOutDatumNone
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

Christopher Marlowe will spend the UTxO `63fb1d7fd884a67e5e950dd19cdcd40abb02d8ed2f65dee976cf9759c4cc4ba0#0`.

### Validator Script and Address

The contract has a validator script and address. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

```
CONTRACT_ADDRESS=$(
marlowe-cli contract address "${MAGIC[@]}"        \
                     --slot-length "$SLOT_LENGTH" \
                     --slot-offset "$SLOT_OFFSET" \
)
marlowe-cli contract validator "${MAGIC[@]}"                \
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

The tip is at slot 2168846. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2168848. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
MINIMUM_SLOT="$TIP"
PAYMENT_DEADLINE=$(($TIP + 1 * 24 * 3600))
COMPLAINT_DEADLINE=$(($TIP + 2 * 24 * 3600))
DISPUTE_DEADLINE=$(($TIP + 3 * 24 * 3600))
MEDIATION_DEADLINE=$(($TIP + 4 * 24 * 3600))
```

* The current slot is 2168846.
* The buyer Thomas Kyd must pay before slot 2255246.
* They buyer Thomas Kyd has until slot 2341646 to complain.
* The seller Francis Beaumont has until slot 2428046 to dispute a complaint.
* The mediator Christopher Marlowe has until slot 2514446 to decide on a disputed complaint.

The contract also involves the price of the good exchanged and a minimum-ADA value.

```
MINIMUM_ADA=3000000
PRICE=256000000
```

The selling price is 256000000 lovelace.

## Transaction 1. Create the Contract by Providing the Minimum ADA.

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
                            --out-file tx-1.marlowe
```

We extract the initial state and full contract from the `.marlowe`file that contains comprehensive information.

```
jq '.marloweState'    tx-1.marlowe > tx-1.state
jq '.marloweContract' tx-1.marlowe > tx-1.contract
```

For each transaction, we construct the output datum. Here is its size and hash:

```
marlowe-cli contract datum --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.datum    \
                           --print-stats
```

```console
Datum size: 823
f57ea4696ebd8aa57a5c8ee805325472d67bfd368c0566112c4ac7f8e5849a37
```

The mediator Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli transaction create "${MAGIC[@]}"                              \
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

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `0172802573895b3be3f56ea18807b963fb9cf591198972fe2e342a3bf5f02ffd`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0172802573895b3be3f56ea18807b963fb9cf591198972fe2e342a3bf5f02ffd     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f57ea4696ebd8aa57a5c8ee805325472d67bfd368c0566112c4ac7f8e5849a37"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0172802573895b3be3f56ea18807b963fb9cf591198972fe2e342a3bf5f02ffd     0        8931654983 lovelace + TxOutDatumNone
```

## Transaction 2. Buyer Deposits Funds into Seller's Account.

First we compute the Marlowe input required to make the initial deposit by the buyer.

```
marlowe-cli input deposit --deposit-account "PK=$SELLER_PUBKEYHASH" \
                          --deposit-party "PK=$BUYER_PUBKEYHASH"    \
                          --deposit-amount "$PRICE"                 \
                          --out-file "tx-2.input"
```

Next we compute the transition caused by that input to the contract.

```
marlowe-cli run compute --contract-file tx-1.contract          \
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
marlowe-cli contract redeemer --input-file tx-2.input    \
                              --out-file   tx-2.redeemer \
                              --print-stats
```

```console
Redeemer size: 85
```

As in the first transaction, we compute the datum and its hash:

```
marlowe-cli contract datum --contract-file tx-2.contract \
                           --state-file    tx-2.state    \
                           --out-file      tx-2.datum    \
                           --print-stats
```

```console
Datum size: 771
972f735bf4ee6176f147824ee7319e0799294c951aedcfa05bb6a5c5086ce83f
```

The value held at the contract address must match that required by its state.

```
CONTRACT_VALUE_2=$(jq '.accounts | [.[][1]] | add' tx-2.state)
```

Now the buyer Thomas Kyd submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli transaction advance "${MAGIC[@]}"                             \
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

The contract received the deposit of 256000000 lovelace from Thomas Kyd in the transaction `78ef5699fe6d9aab5aac78016847a2c857e4e94d035be97b2d0de39563a0c4da`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
78ef5699fe6d9aab5aac78016847a2c857e4e94d035be97b2d0de39563a0c4da     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "972f735bf4ee6176f147824ee7319e0799294c951aedcfa05bb6a5c5086ce83f"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
78ef5699fe6d9aab5aac78016847a2c857e4e94d035be97b2d0de39563a0c4da     0        5065072982 lovelace + TxOutDatumNone
78ef5699fe6d9aab5aac78016847a2c857e4e94d035be97b2d0de39563a0c4da     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli input choose --choice-name "Report problem"        \
                         --choice-party "PK=$BUYER_PUBKEYHASH" \
                         --choice-number 1                     \
                         --out-file tx-3.input
```

As in the second transaction we compute the contract's transition, its new state, the redeemer, the datum, and the value.

```
marlowe-cli run compute --contract-file tx-2.contract          \
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
marlowe-cli contract redeemer --input-file tx-3.input    \
                              --out-file   tx-3.redeemer \
                              --print-stats
```

```console
Redeemer size: 60
```

```
marlowe-cli contract datum --contract-file tx-3.contract \
                           --state-file    tx-3.state    \
                           --out-file      tx-3.datum    \
                           --print-stats
```

```console
Datum size: 572
a84200b1064c589dee6634386d299794588d7081285facc6c0bcde1087369f93
```

Now the buyer Thomas Kyd can submit a transaction to report that there is a problem:

```
TX_3=$(
marlowe-cli transaction advance "${MAGIC[@]}"                             \
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

The reporting of a problem was recorded in the transaction `f46f6f40d75e3b7a21be662f08bf32a2a66122bd6261ef760f22e4c54a20fcbc`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f46f6f40d75e3b7a21be662f08bf32a2a66122bd6261ef760f22e4c54a20fcbc     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "a84200b1064c589dee6634386d299794588d7081285facc6c0bcde1087369f93"
```

Here is the UTxO at Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f46f6f40d75e3b7a21be662f08bf32a2a66122bd6261ef760f22e4c54a20fcbc     0        5060776827 lovelace + TxOutDatumNone
f46f6f40d75e3b7a21be662f08bf32a2a66122bd6261ef760f22e4c54a20fcbc     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 4. The Seller Disputes that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli input choose --choice-name "Dispute problem"        \
                         --choice-party "PK=$SELLER_PUBKEYHASH" \
                         --choice-number 0                      \
                         --out-file tx-4.input
```

As always we compute the contract's transition, its new state, the redeemer, the datum, and the value.

```
marlowe-cli run compute --contract-file tx-3.contract          \
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
marlowe-cli contract redeemer --input-file tx-4.input    \
                              --out-file   tx-4.redeemer \
                              --print-stats
```

```console
Redeemer size: 61
```

```
marlowe-cli contract datum --contract-file tx-4.contract \
                           --state-file    tx-4.state    \
                           --out-file      tx-4.datum    \
                           --print-stats
```

```console
Datum size: 470
c45057ca548688cfb3919dabba18133bffe5e8af1cd97fd93d56db3df3bd250f
```

Now the seller Francis Beaumont can submit a transaction to dispute that there is a problem:

```
TX_4=$(
marlowe-cli transaction advance "${MAGIC[@]}"                             \
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

The dispute that this is a problem is recorded in the transaction `d7c2f5b7eae876b41049c487db12ccd76cf72f5b6500214c63247422e147a556`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d7c2f5b7eae876b41049c487db12ccd76cf72f5b6500214c63247422e147a556     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "c45057ca548688cfb3919dabba18133bffe5e8af1cd97fd93d56db3df3bd250f"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d7c2f5b7eae876b41049c487db12ccd76cf72f5b6500214c63247422e147a556     0        20388834657 lovelace + TxOutDatumNone
d7c2f5b7eae876b41049c487db12ccd76cf72f5b6500214c63247422e147a556     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 5. The Mediator Confirms the Claim

Funds are released to the buyer and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli input choose --choice-name "Confirm claim"            \
                         --choice-party "PK=$MEDIATOR_PUBKEYHASH" \
                         --choice-number 1                        \
                         --out-file tx-5.input
```

As previously we compute the contract's transition, its new state, and the redeemer. Because the contract is being closed, no new datum need be computed.

```
marlowe-cli run compute --contract-file tx-4.contract          \
                        --state-file    tx-4.state             \
                        --input-file    tx-5.input             \
                        --out-file      tx-5.marlowe           \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --print-stats
```

```console
Payment 1
  Acccount: PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274"
  Payee: Party (PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274")
  Ada: 3.000000
Payment 2
  Acccount: PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79"
  Payee: Party (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")
  Ada: 256.000000
Datum size: 181
```

```
jq '.state'    tx-5.marlowe > tx-5.state
jq '.contract' tx-5.marlowe > tx-5.contract
marlowe-cli contract redeemer --input-file tx-5.input    \
                              --out-file   tx-5.redeemer \
                              --print-stats
```

```console
Redeemer size: 59
```

Now the mediator Christopher Marlowe can submit a transaction to release funds:

```
TX_5=$(
marlowe-cli transaction close "${MAGIC[@]}"                              \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                              --tx-in-marlowe "$TX_4"#1                  \
                              --tx-in-script-file escrow.plutus          \
                              --tx-in-datum-file tx-4.datum              \
                              --tx-in-redeemer-file tx-5.redeemer        \
                              --tx-in "$TX_1"#0                          \
                              --tx-in-collateral "$TX_1"#0               \
                              --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                              --tx-out "$BUYER_ADDRESS+$PRICE"           \
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
Fee: Lovelace 1139396
Size: 14718 / 32768 = 44%
Execution units:
  Memory: 3888228 / 10000000 = 38%
  Steps: 1343239352 / 10000000000 = 13%
```

The confirmation of the claim resulted in closing the contract, paying 256000000 lovelace to the buyer Thomas Kyd and 3000000 lovelace to the mediator Christopher Marlowe in the transaction `55b33216dbf5ced181dadd96a1e95fd52410f6895d5287d85de50ee454ea7078`.  There is no UTxO at the contract address:

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
55b33216dbf5ced181dadd96a1e95fd52410f6895d5287d85de50ee454ea7078     1        256000000 lovelace + TxOutDatumNone
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
55b33216dbf5ced181dadd96a1e95fd52410f6895d5287d85de50ee454ea7078     0        8930515587 lovelace + TxOutDatumNone
55b33216dbf5ced181dadd96a1e95fd52410f6895d5287d85de50ee454ea7078     2        3000000 lovelace + TxOutDatumNone
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
