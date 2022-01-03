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

Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `MEDIATOR_PREFIX` where they appear below.

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
56c38f8941152570a4420a40a9ad99132e794d1e2806d2adaaa6d85d13e9e694     0        3003616304 lovelace + TxOutDatumNone
56c38f8941152570a4420a40a9ad99132e794d1e2806d2adaaa6d85d13e9e694     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
56c38f8941152570a4420a40a9ad99132e794d1e2806d2adaaa6d85d13e9e694     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FrancisBeaumont + TxOutDatumNone
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
cardano-cli query utxo "${MAGIC[@]}"                                                                      \
                       --address "$SELLER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                             \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$SELLER_ROLE"'" == 1) | .key' \
)
```

Francis Beaumont will spend the UTxOs `56c38f8941152570a4420a40a9ad99132e794d1e2806d2adaaa6d85d13e9e694#0` and `56c38f8941152570a4420a40a9ad99132e794d1e2806d2adaaa6d85d13e9e694#1`.

### The Buyer

```
BUYER_PREFIX="$TREASURY/thomas-middleton"
BUYER_NAME="Thomas Middleton"
BUYER_ROLE=TM
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
92d52f47f45aadc93ebf247403e416892ed3e7b6b7a5f239dc98f143c406d977     0        2954355169 lovelace + TxOutDatumNone
92d52f47f45aadc93ebf247403e416892ed3e7b6b7a5f239dc98f143c406d977     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
92d52f47f45aadc93ebf247403e416892ed3e7b6b7a5f239dc98f143c406d977     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
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
cardano-cli query utxo "${MAGIC[@]}"                                                                     \
                       --address "$BUYER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                            \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$BUYER_ROLE"'" == 1) | .key' \
)
```

Thomas Middleton will spend the UTxOs `92d52f47f45aadc93ebf247403e416892ed3e7b6b7a5f239dc98f143c406d977#0` and `92d52f47f45aadc93ebf247403e416892ed3e7b6b7a5f239dc98f143c406d977#1`.

### The Mediator

```
MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_ROLE=CM
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
6bacba798627179750ae5d6f947adfcd6c62a7d899bf264c4fa1660ce5c4ef73     0        1965693621 lovelace + TxOutDatumNone
6bacba798627179750ae5d6f947adfcd6c62a7d899bf264c4fa1660ce5c4ef73     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
6bacba798627179750ae5d6f947adfcd6c62a7d899bf264c4fa1660ce5c4ef73     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ChristopherMarlowe + TxOutDatumNone
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
cardano-cli query utxo "${MAGIC[@]}"                                                                        \
                       --address "$MEDIATOR_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                               \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$MEDIATOR_ROLE"'" == 1) | .key' \
)
```

Christopher Marlowe will spend the UTxOs `6bacba798627179750ae5d6f947adfcd6c62a7d899bf264c4fa1660ce5c4ef73#0` and `6bacba798627179750ae5d6f947adfcd6c62a7d899bf264c4fa1660ce5c4ef73#1`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 46873317. The current POSIX time implies that the tip of the blockchain should be slightly before slot 46873319. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((TIP+1*24*3600))
COMPLAINT_DEADLINE=$((TIP+2*24*3600))
DISPUTE_DEADLINE=$((TIP+3*24*3600))
MEDIATION_DEADLINE=$((TIP+4*24*3600))
```

* The current slot is 46873317.
* The buyer Thomas Middleton must pay before slot 46959717.
* They buyer Thomas Middleton has until slot 47046117 to complain.
* The seller Francis Beaumont has until slot 47132517 to dispute a complaint.
* The mediator Christopher Marlowe has until slot 47218917 to decide on a disputed complaint.

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

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

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
Validator size: 14578
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 45939839, exBudgetMemory = ExMemory 154400}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wrne597ectcpfjrl3azk4ag9adc47wy8ft4ccxat0q2zurq2fsdjf`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpt3m3hnfzystzp6x3n5v4q0jsj5hca4z5drs30us07pe8sslxxqd`.

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
Fee: Lovelace 197005
Size: 703 / 16384 = 4%
Execution units:
  Memory: 0 / 12500000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `b1a63c3258e3710ef93795153df253f6d4b34862f258696cb8950f64499607b4`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b1a63c3258e3710ef93795153df253f6d4b34862f258696cb8950f64499607b4     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "bbec0127a601e5b1735a59fb50d058bf84617ec1a3a49514717f17a1b107d364"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b1a63c3258e3710ef93795153df253f6d4b34862f258696cb8950f64499607b4     0        1962496616 lovelace + TxOutDatumNone
```

## Transaction 2. Buyer Deposits Funds into Seller's Account.

First we compute the Marlowe input required to make the initial deposit by the buyer.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$SELLER_ROLE" \
                        --deposit-party "Role=$BUYER_ROLE"    \
                        --deposit-amount "$PRICE"             \
                        --invalid-before "$TIP"               \
                        --invalid-hereafter "$((TIP+4*3600))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 447
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
Fee: Lovelace 1317104
Size: 15976 / 16384 = 97%
Execution units:
  Memory: 5237756 / 12500000 = 41%
  Steps: 1898639171 / 10000000000 = 18%
```

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `3f2cc8ec9b98ea4dbaebab7d1bf37915812de0d1193e7553cf18c8f91a15daf7`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3f2cc8ec9b98ea4dbaebab7d1bf37915812de0d1193e7553cf18c8f91a15daf7     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "299bc3a064e4e9a0899a18220d4ef91c0560451b4a52aba941ea280d5aead2a2"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3f2cc8ec9b98ea4dbaebab7d1bf37915812de0d1193e7553cf18c8f91a15daf7     0        2696038065 lovelace + TxOutDatumNone
3f2cc8ec9b98ea4dbaebab7d1bf37915812de0d1193e7553cf18c8f91a15daf7     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --choice-name "Report problem"        \
                        --choice-party "Role=$BUYER_ROLE"     \
                        --choice-number 1                     \
                        --invalid-before "$TIP"               \
                        --invalid-hereafter "$((TIP+4*3600))" \
                        --out-file tx-3.marlowe               \
                        --print-stats
```

```console
Datum size: 329
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
Fee: Lovelace 1410489
Size: 15835 / 16384 = 96%
Execution units:
  Memory: 6514554 / 12500000 = 52%
  Steps: 2258106595 / 10000000000 = 22%
```

The reporting of a problem was recorded in the transaction `038159fcd6db522d042968e9d393b255f9660bb93430af49f23f4d49447976e1`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
038159fcd6db522d042968e9d393b255f9660bb93430af49f23f4d49447976e1     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "442afc900e57fd98be2d635298701d418eb06f23a91b97f0f0a1018683be72e7"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
038159fcd6db522d042968e9d393b255f9660bb93430af49f23f4d49447976e1     0        2694627576 lovelace + TxOutDatumNone
038159fcd6db522d042968e9d393b255f9660bb93430af49f23f4d49447976e1     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
```

## Transaction 4. The Seller Disputes that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe            \
                        --choice-name "Dispute problem"        \
                        --choice-party "Role=$SELLER_ROLE" \
                        --choice-number 0                      \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$((TIP+4*3600))"  \
                        --out-file tx-4.marlowe                \
                        --print-stats
```

```console
Datum size: 254
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
Fee: Lovelace 1276685
Size: 15643 / 16384 = 95%
Execution units:
  Memory: 4985668 / 12500000 = 39%
  Steps: 1742995705 / 10000000000 = 17%
```

The dispute that this is a problem is recorded in the transaction `62a7e1e4ddd43e99085849183036488cee1376e470f1402aad55f58c6dcf950f`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
62a7e1e4ddd43e99085849183036488cee1376e470f1402aad55f58c6dcf950f     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f20adee381730836823f62e78a8b3ea6bad742c7f8e240861356551b1de7a751"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
62a7e1e4ddd43e99085849183036488cee1376e470f1402aad55f58c6dcf950f     0        3001339619 lovelace + TxOutDatumNone
62a7e1e4ddd43e99085849183036488cee1376e470f1402aad55f58c6dcf950f     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
```

## Transaction 5. The Mediator Dismisses the Claim

Funds are released to the seller and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe           \
                        --choice-name "Dismiss claim"         \
                        --choice-party "Role=$MEDIATOR_ROLE"  \
                        --choice-number 0                     \
                        --invalid-before "$TIP"               \
                        --invalid-hereafter "$((TIP+4*3600))" \
                        --out-file tx-5.marlowe               \
                        --print-stats
```

```console
Datum size: 100
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
Fee: Lovelace 1377465
Size: 15389 / 16384 = 93%
Execution units:
  Memory: 6425156 / 12500000 = 51%
  Steps: 2143802768 / 10000000000 = 21%
```

The dismissal of the claim resulted in closing the contract, paying 256000000 lovelace to the role address for the benefit of the seller Francis Beaumont and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `f8f580ca1adb1cc5dea7687f603c7685c46846e996c8e579a7daa734549d169e`.  There is no UTxO at the contract address:

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
f8f580ca1adb1cc5dea7687f603c7685c46846e996c8e579a7daa734549d169e     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
f8f580ca1adb1cc5dea7687f603c7685c46846e996c8e579a7daa734549d169e     2        256000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f8f580ca1adb1cc5dea7687f603c7685c46846e996c8e579a7daa734549d169e     0        1960119151 lovelace + TxOutDatumNone
f8f580ca1adb1cc5dea7687f603c7685c46846e996c8e579a7daa734549d169e     3        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
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
Fee: Lovelace 450617
Size: 3288 / 16384 = 20%
Execution units:
  Memory: 1541710 / 12500000 = 12%
  Steps: 581669615 / 10000000000 = 5%
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
Fee: Lovelace 455805
Size: 3288 / 16384 = 20%
Execution units:
  Memory: 1607210 / 12500000 = 12%
  Steps: 601218138 / 10000000000 = 6%
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
5ffc43e74f86c2e63847557288410f662ca5a1a971a34f61b4af7723e748f146     0        3000889002 lovelace + TxOutDatumNone
5ffc43e74f86c2e63847557288410f662ca5a1a971a34f61b4af7723e748f146     1        256000000 lovelace + TxOutDatumNone
5ffc43e74f86c2e63847557288410f662ca5a1a971a34f61b4af7723e748f146     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b93b551188448223a12c51389bf8946a511297ba7d787277d92760073b4a5642     0        1959663346 lovelace + TxOutDatumNone
b93b551188448223a12c51389bf8946a511297ba7d787277d92760073b4a5642     1        3000000 lovelace + TxOutDatumNone
b93b551188448223a12c51389bf8946a511297ba7d787277d92760073b4a5642     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
```

# Clean Up
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
