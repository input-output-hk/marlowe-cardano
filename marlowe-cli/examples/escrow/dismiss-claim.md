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
* basenc
* tr

Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `MEDIATOR_PREFIX` where they appear below.

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
  SLOT_OFFSET=1644929640000
fi
```

### Role Currency

Set the role currency for the validator.

```
ROLE_CURRENCY=8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d
```

### Select Parties

#### The Seller

The seller sells an item for a price.

```
SELLER_PREFIX="$TREASURY/francis-beaumont"
SELLER_NAME="Francis Beaumont"
SELLER_ROLE=FB
SELLER_ROLE_HEX=$(echo -n "$SELLER_ROLE" | basenc --base16 | tr '[:upper:]' '[:lower:]')
SELLER_TOKEN="$ROLE_CURRENCY.$SELLER_ROLE"
SELLER_PAYMENT_SKEY="$SELLER_PREFIX".skey
SELLER_PAYMENT_VKEY="$SELLER_PREFIX".vkey
SELLER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$SELLER_PAYMENT_VKEY" \
)
```

The seller Francis Beaumont has the address `addr_test1vrtntkszteptml4e9ce9l3fsmgavwv4ywunvdnhxv6nw5ksq6737a` and role token named `FB`. They have the following UTxOs in their wallet:

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
d2eb7653cfc41c8810e66e44dc60cd657645e98640a1c145a5aa1f2304c85319     0        11422516927 lovelace + TxOutDatumNone
d2eb7653cfc41c8810e66e44dc60cd657645e98640a1c145a5aa1f2304c85319     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
```

We select the UTxO with the seller Francis Beaumont's role token.

```
TX_0_SELLER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$SELLER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_SELLER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                          \
                       --address "$SELLER_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                 \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$SELLER_ROLE_HEX"'" == 1) | .key' \
)
```

Francis Beaumont will spend the UTxOs `d2eb7653cfc41c8810e66e44dc60cd657645e98640a1c145a5aa1f2304c85319#0` and `d2eb7653cfc41c8810e66e44dc60cd657645e98640a1c145a5aa1f2304c85319#1`.

### The Buyer

```
BUYER_PREFIX="$TREASURY/thomas-middleton"
BUYER_NAME="Thomas Middleton"
BUYER_ROLE=TM
BUYER_ROLE_HEX=$(echo -n "$BUYER_ROLE" | basenc --base16 | tr '[:upper:]' '[:lower:]')
BUYER_TOKEN="$ROLE_CURRENCY.$BUYER_ROLE"
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey
BUYER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                         \
                            --payment-verification-key-file "$BUYER_PAYMENT_VKEY" \
)
```

The buyer Thomas Middleton has the address `addr_test1vzgrqnlp6elmettvuelx5vkn0uxhtu2ewqdhx297ukgjmjgpss5k0` and role token named `TM`. They have the following UTxOs in their wallet:

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
5a3c5a21676b3c3b0ce941dcb8c7cbe994da0dd6bcc2836d8b19f2abcc603780     0        10393734649 lovelace + TxOutDatumNone
5a3c5a21676b3c3b0ce941dcb8c7cbe994da0dd6bcc2836d8b19f2abcc603780     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
```

We select the UTxO with the buyer Thomas Middleton's role token.

```
TX_0_BUYER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$BUYER_ADDRESS"                                                               \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_BUYER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                         \
                       --address "$BUYER_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$BUYER_ROLE_HEX"'" == 1) | .key' \
)
```

Thomas Middleton will spend the UTxOs `5a3c5a21676b3c3b0ce941dcb8c7cbe994da0dd6bcc2836d8b19f2abcc603780#0` and `5a3c5a21676b3c3b0ce941dcb8c7cbe994da0dd6bcc2836d8b19f2abcc603780#1`.

### The Mediator

```
MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_ROLE=CM
MEDIATOR_ROLE_HEX=$(echo -n "$MEDIATOR_ROLE" | basenc --base16 | tr '[:upper:]' '[:lower:]')
MEDIATOR_TOKEN="$ROLE_CURRENCY.$MEDIATOR_ROLE"
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey
MEDIATOR_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                            \
                            --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY" \
)
```

The mediator Christopher Marlowe has the address `addr_test1vqhqudxtwqcpjqesns79hqgqq2q0xx5q0hnzz5es9492yaqpxltpy` and role token named `CM`. They have the following UTxOs in their wallet:

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
d5509eff05df06721261906af8c016fd62319a9a52e8f15be737103db3cd9eb2     0        21877206651 lovelace + TxOutDatumNone
d5509eff05df06721261906af8c016fd62319a9a52e8f15be737103db3cd9eb2     1        2000000 lovelace + 5000 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4265617247617264656e + TxOutDatumNone
d5509eff05df06721261906af8c016fd62319a9a52e8f15be737103db3cd9eb2     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
```

We select the UTxO with the mediator Christopher Marlowe's role token.

```
TX_0_MEDIATOR_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$MEDIATOR_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_MEDIATOR_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$MEDIATOR_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$MEDIATOR_ROLE_HEX"'" == 1) | .key' \
)
```

Christopher Marlowe will spend the UTxOs `d5509eff05df06721261906af8c016fd62319a9a52e8f15be737103db3cd9eb2#0` and `d5509eff05df06721261906af8c016fd62319a9a52e8f15be737103db3cd9eb2#2`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
```

The tip is at slot 2428280. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2428284. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))
```

* The current slot is 2428280.
* The buyer Thomas Middleton must pay before Wed, 16 Mar 2022 15:25:20 +0000.
* They buyer Thomas Middleton has until Thu, 17 Mar 2022 15:25:20 +0000 to complain.
* The seller Francis Beaumont has until Fri, 18 Mar 2022 15:25:20 +0000 to dispute a complaint.
* The mediator Christopher Marlowe has until Sat, 19 Mar 2022 15:25:20 +0000 to decide on a disputed complaint.

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
                            --seller "Role=$SELLER_ROLE"               \
                            --buyer "Role=$BUYER_ROLE"                 \
                            --mediator "Role=$MEDIATOR_ROLE"           \
                            --payment-deadline "$PAYMENT_DEADLINE"     \
                            --complaint-deadline "$COMPLAINT_DEADLINE" \
                            --dispute-deadline "$DISPUTE_DEADLINE"     \
                            --mediation-deadline "$MEDIATION_DEADLINE" \
                            --out-contract-file tx-1.contract          \
                            --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA.

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require.

```
marlowe-cli run initialize "${MAGIC[@]}"                     \
                           --slot-length "$SLOT_LENGTH"      \
                           --slot-offset "$SLOT_OFFSET"      \
                           --roles-currency "$ROLE_CURRENCY" \
                           --contract-file tx-1.contract     \
                           --state-file    tx-1.state        \
                           --out-file      tx-1.marlowe      \
                           --print-stats
```

```console
Validator size: 11961
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24384187, exBudgetMemory = ExMemory 82000}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wqhhy32uvsjtcd0s9lkve9dvdjx2l6q6dyshshm0rf57wjsv0d9mu`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wqkpm288nc55xq9ah0d0u63cfw95zjs983jvwl03dyrngmq8v9qpq`.

The mediator Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --tx-in "$TX_0_MEDIATOR_ADA"               \
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
Fee: Lovelace 197885
Size: 723 / 32768 = 2%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `ec5f6ee02a41576677fe75743f3eb22e5f257cb6ce12bfdc4a0a9caac9dc9b82`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ec5f6ee02a41576677fe75743f3eb22e5f257cb6ce12bfdc4a0a9caac9dc9b82     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "257eaf6eb024005bb8ebd8d2769a9be5258a1bedb0b2b05fc7820a50387bc83a"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ec5f6ee02a41576677fe75743f3eb22e5f257cb6ce12bfdc4a0a9caac9dc9b82     0        21874008766 lovelace + TxOutDatumNone
```

## Transaction 2. Buyer Deposits Funds into Seller's Account.

First we compute the Marlowe input required to make the initial deposit by the buyer.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$SELLER_ROLE" \
                        --deposit-party "Role=$BUYER_ROLE"    \
                        --deposit-amount "$PRICE"             \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 463
```

Now the buyer Thomas Middleton submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                                         \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-1.marlowe                        \
                        --tx-in-marlowe "$TX_1"#1                             \
                        --tx-in-collateral "$TX_0_BUYER_ADA"                  \
                        --tx-in "$TX_0_BUYER_ADA"                             \
                        --tx-in "$TX_0_BUYER_TOKEN"                           \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-2.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-2.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                              \
)
```

```console
Fee: Lovelace 1195686
Size: 13395 / 32768 = 40%
Execution units:
  Memory: 5136504 / 30000000 = 17%
  Steps: 1870740930 / 10000000000 = 18%
```

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `608b65ecf9d552c1342d8866177a61950232338cd0b3be9a003ef80b89059684`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
608b65ecf9d552c1342d8866177a61950232338cd0b3be9a003ef80b89059684     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "6db010f5af56bd043a1107f01e2bf7454024745705249879d7a676f7545527fd"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
608b65ecf9d552c1342d8866177a61950232338cd0b3be9a003ef80b89059684     0        10135538963 lovelace + TxOutDatumNone
608b65ecf9d552c1342d8866177a61950232338cd0b3be9a003ef80b89059684     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --choice-name "Report problem"        \
                        --choice-party "Role=$BUYER_ROLE"     \
                        --choice-number 1                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats
```

```console
Datum size: 341
Payment 1
  Acccount: "FB"
  Payee: Account "TM"
  Ada: 256.000000
```

Now the buyer Thomas Middleton can submit a transaction to report that there is a problem:

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                                         \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-2.marlowe                        \
                        --tx-in-marlowe "$TX_2"#1                             \
                        --tx-in-collateral "$TX_2"#0                          \
                        --tx-in "$TX_2"#0                                     \
                        --tx-in "$TX_2"#2                                     \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-3.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-3.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 1270067
Size: 13250 / 32768 = 40%
Execution units:
  Memory: 6175502 / 30000000 = 20%
  Steps: 2159377546 / 10000000000 = 21%
```

The reporting of a problem was recorded in the transaction `d2afe144c62d7f8fbf53c90e0d4e860069aa1ecd0fc9ba6b29317ce9974e696d`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d2afe144c62d7f8fbf53c90e0d4e860069aa1ecd0fc9ba6b29317ce9974e696d     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "01b1c3b68b8c48f613f0553bdd8935a474954198bb62d0d10994d095829c3a7c"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d2afe144c62d7f8fbf53c90e0d4e860069aa1ecd0fc9ba6b29317ce9974e696d     0        10134268896 lovelace + TxOutDatumNone
d2afe144c62d7f8fbf53c90e0d4e860069aa1ecd0fc9ba6b29317ce9974e696d     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
```

## Transaction 4. The Seller Disputes that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe            \
                        --choice-name "Dispute problem"        \
                        --choice-party "Role=$SELLER_ROLE" \
                        --choice-number 0                      \
                        --invalid-before "$NOW"                \
                        --invalid-hereafter "$((NOW+4*HOUR))"  \
                        --out-file tx-4.marlowe                \
                        --print-stats
```

```console
Datum size: 262
```

Now the seller Francis Beaumont can submit a transaction to dispute that there is a problem:

```
TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                                           \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                        --marlowe-in-file tx-3.marlowe                          \
                        --tx-in-marlowe "$TX_3"#1                               \
                        --tx-in-collateral "$TX_0_SELLER_ADA"                   \
                        --tx-in "$TX_0_SELLER_ADA"                              \
                        --tx-in "$TX_0_SELLER_TOKEN"                            \
                        --required-signer "$SELLER_PAYMENT_SKEY"                \
                        --marlowe-out-file tx-4.marlowe                         \
                        --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA+1 $SELLER_TOKEN" \
                        --change-address "$SELLER_ADDRESS"                      \
                        --out-file tx-4.raw                                     \
                        --print-stats                                           \
                        --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                \
)
```

```console
Fee: Lovelace 1152060
Size: 13050 / 32768 = 39%
Execution units:
  Memory: 4850616 / 30000000 = 16%
  Steps: 1704994684 / 10000000000 = 17%
```

The dispute that this is a problem is recorded in the transaction `58edeea8348d80581a68bc3a0d30f60c439d43dcca5df36eabf26b74127b8c28`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
58edeea8348d80581a68bc3a0d30f60c439d43dcca5df36eabf26b74127b8c28     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4b459c7eeb618c5709eb18040205a58965876236297efd854dec1967e003e74c"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
58edeea8348d80581a68bc3a0d30f60c439d43dcca5df36eabf26b74127b8c28     0        11420364867 lovelace + TxOutDatumNone
58edeea8348d80581a68bc3a0d30f60c439d43dcca5df36eabf26b74127b8c28     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
```

## Transaction 5. The Mediator Dismisses the Claim

Funds are released to the seller and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe           \
                        --choice-name "Dismiss claim"         \
                        --choice-party "Role=$MEDIATOR_ROLE"  \
                        --choice-number 0                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-5.marlowe               \
                        --print-stats
```

```console
Datum size: 104
Payment 1
  Acccount: "TM"
  Payee: Account "FB"
  Ada: 256.000000
Payment 2
  Acccount: "CM"
  Payee: Party "CM"
  Ada: 3.000000
Payment 3
  Acccount: "FB"
  Payee: Party "FB"
  Ada: 256.000000
```

Now the mediator Christopher Marlowe can submit a transaction to release funds:

```
TX_5=$(
marlowe-cli run execute "${MAGIC[@]}"                                               \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                        --marlowe-in-file tx-4.marlowe                              \
                        --tx-in-marlowe "$TX_4"#1                                   \
                        --tx-in-collateral "$TX_1"#0                                \
                        --tx-in "$TX_1"#0                                           \
                        --tx-in "$TX_0_MEDIATOR_TOKEN"                              \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY"                  \
                        --marlowe-out-file tx-5.marlowe                             \
                        --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA+1 $MEDIATOR_TOKEN" \
                        --change-address "$MEDIATOR_ADDRESS"                        \
                        --out-file tx-5.raw                                         \
                        --print-stats                                               \
                        --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                    \
)
```

```console
Fee: Lovelace 1191158
Size: 12784 / 32768 = 39%
Execution units:
  Memory: 5520750 / 30000000 = 18%
  Steps: 1873306884 / 10000000000 = 18%
```

The dismissal of the claim resulted in closing the contract, paying 256000000 lovelace to the role address for the benefit of the seller Francis Beaumont and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `f5fc46a1d720096c1354f239063f85991c8ac13c2eee57a56fc4b9a2555102d9`.  There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f5fc46a1d720096c1354f239063f85991c8ac13c2eee57a56fc4b9a2555102d9     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
f5fc46a1d720096c1354f239063f85991c8ac13c2eee57a56fc4b9a2555102d9     2        256000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f5fc46a1d720096c1354f239063f85991c8ac13c2eee57a56fc4b9a2555102d9     0        21871817608 lovelace + TxOutDatumNone
f5fc46a1d720096c1354f239063f85991c8ac13c2eee57a56fc4b9a2555102d9     3        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
```

## Transactions 6 and 7. Seller and Mediator Withdraw Funds.

The seller Francis Beaumont submits a transaction to withdraw the payment from the role address.

```
TX_6=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                           \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-5.marlowe                             \
                         --role-name "$SELLER_ROLE"                              \
                         --tx-in "$TX_4"#0                                       \
                         --tx-in "$TX_4"#2                                       \
                         --tx-in-collateral "$TX_4"#0                            \
                         --required-signer "$SELLER_PAYMENT_SKEY"                \
                         --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA+1 $SELLER_TOKEN" \
                         --change-address "$SELLER_ADDRESS"                      \
                         --out-file tx-6.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)
```

```console
Fee: Lovelace 422397
Size: 2889 / 32768 = 8%
Execution units:
  Memory: 1407010 / 30000000 = 4%
  Steps: 541567360 / 10000000000 = 5%
```

The mediator Christopher Marlowe submits a transaction to withdraw the minimum ADA from the role address.

```
TX_7=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                               \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                         --marlowe-file tx-5.marlowe                                 \
                         --role-name "$MEDIATOR_ROLE"                                \
                         --tx-in "$TX_5"#0                                           \
                         --tx-in "$TX_5"#3                                           \
                         --tx-in-collateral "$TX_5"#0                                \
                         --required-signer "$MEDIATOR_PAYMENT_SKEY"                  \
                         --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA+1 $MEDIATOR_TOKEN" \
                         --change-address "$MEDIATOR_ADDRESS"                        \
                         --out-file tx-7.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)
```

```console
Fee: Lovelace 426739
Size: 2889 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
```

There is no UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
dad71c91fa59f4e85dae291a0210251772d8b03ee9b6b9bd822e1cb6a9d4bd12     0        11419942470 lovelace + TxOutDatumNone
dad71c91fa59f4e85dae291a0210251772d8b03ee9b6b9bd822e1cb6a9d4bd12     1        256000000 lovelace + TxOutDatumNone
dad71c91fa59f4e85dae291a0210251772d8b03ee9b6b9bd822e1cb6a9d4bd12     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
da47cc88aa1ed92a184bfa5823ee46d3a0577afd1933e4de49c4fd0b86a7f827     0        21871390869 lovelace + TxOutDatumNone
da47cc88aa1ed92a184bfa5823ee46d3a0577afd1933e4de49c4fd0b86a7f827     1        3000000 lovelace + TxOutDatumNone
da47cc88aa1ed92a184bfa5823ee46d3a0577afd1933e4de49c4fd0b86a7f827     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
```

## Summary of Transactions

![Summary of transactions.](dismiss-claim-txs.svg)

## Clean Up

Send the funds back to the buyer, so that the test can be run again.

```
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX_6"#1                         \
                               --required-signer "$SELLER_PAYMENT_SKEY"  \
                               --change-address "$BUYER_ADDRESS"         \
                               --out-file tx-8.raw                       \
                               --submit=600                              \
> /dev/null
