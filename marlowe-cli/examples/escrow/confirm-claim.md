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
36515d01eb8b21c937d6bff82b2c31082511b9998b38cb382d579da7e1037a6b     0        11425138069 lovelace + TxOutDatumNone
36515d01eb8b21c937d6bff82b2c31082511b9998b38cb382d579da7e1037a6b     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
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

Francis Beaumont will spend the UTxOs `36515d01eb8b21c937d6bff82b2c31082511b9998b38cb382d579da7e1037a6b#0` and `36515d01eb8b21c937d6bff82b2c31082511b9998b38cb382d579da7e1037a6b#1`.

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
c9ce4813545829269bf68cd90a2d21aa5552108ba1de4e3ef2307b99b6841281     0        10399890799 lovelace + TxOutDatumNone
c9ce4813545829269bf68cd90a2d21aa5552108ba1de4e3ef2307b99b6841281     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `c9ce4813545829269bf68cd90a2d21aa5552108ba1de4e3ef2307b99b6841281#0` and `c9ce4813545829269bf68cd90a2d21aa5552108ba1de4e3ef2307b99b6841281#1`.

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
4d0d051e197e8ee846dd90a5078de33ca1c99aebfbb40787de9fd8b42943aada     0        21879934417 lovelace + TxOutDatumNone
4d0d051e197e8ee846dd90a5078de33ca1c99aebfbb40787de9fd8b42943aada     1        2000000 lovelace + 5000 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4265617247617264656e + TxOutDatumNone
4d0d051e197e8ee846dd90a5078de33ca1c99aebfbb40787de9fd8b42943aada     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
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

Christopher Marlowe will spend the UTxOs `4d0d051e197e8ee846dd90a5078de33ca1c99aebfbb40787de9fd8b42943aada#0` and `4d0d051e197e8ee846dd90a5078de33ca1c99aebfbb40787de9fd8b42943aada#2`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
```

The tip is at slot 2427789. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2427793. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))
```

* The current slot is 2427789.
* The buyer Thomas Middleton must pay before Wed, 16 Mar 2022 15:17:09 +0000.
* They buyer Thomas Middleton has until Thu, 17 Mar 2022 15:17:09 +0000 to complain.
* The seller Francis Beaumont has until Fri, 18 Mar 2022 15:17:09 +0000 to dispute a complaint.
* The mediator Christopher Marlowe has until Sat, 19 Mar 2022 15:17:09 +0000 to decide on a disputed complaint.

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

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `c29c43bd5f4c676e48760fd1445f8d7774d6316bca16a82fa53ee8e60b090199`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c29c43bd5f4c676e48760fd1445f8d7774d6316bca16a82fa53ee8e60b090199     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "78756afb4eff8a2ac4e7f9277c704b02bfc7b92d08428128c28c7368acd76522"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c29c43bd5f4c676e48760fd1445f8d7774d6316bca16a82fa53ee8e60b090199     0        21876736532 lovelace + TxOutDatumNone
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
Fee: Lovelace 1200060
Size: 13395 / 32768 = 40%
Execution units:
  Memory: 5191704 / 30000000 = 17%
  Steps: 1887222834 / 10000000000 = 18%
```

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `3d4f198cf5cf90eecca924f44ad55738c0523473ecd42c6261fe6520d775eba5`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3d4f198cf5cf90eecca924f44ad55738c0523473ecd42c6261fe6520d775eba5     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "d686ff6029dc09a71de319b60429136d54ae59da3aae1ccc59b445539489a50f"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3d4f198cf5cf90eecca924f44ad55738c0523473ecd42c6261fe6520d775eba5     0        10141690739 lovelace + TxOutDatumNone
3d4f198cf5cf90eecca924f44ad55738c0523473ecd42c6261fe6520d775eba5     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
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

The reporting of a problem was recorded in the transaction `8ba763854d1e397ad8f2c9a4eaa81856e92ec0973c575f7a8a473ba1cef2fe82`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8ba763854d1e397ad8f2c9a4eaa81856e92ec0973c575f7a8a473ba1cef2fe82     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "55ee00c1501f7f1672825cd2681dbe9635d184d754504de2d23703753cc07eec"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8ba763854d1e397ad8f2c9a4eaa81856e92ec0973c575f7a8a473ba1cef2fe82     0        10140420672 lovelace + TxOutDatumNone
8ba763854d1e397ad8f2c9a4eaa81856e92ec0973c575f7a8a473ba1cef2fe82     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
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
Fee: Lovelace 1147687
Size: 13050 / 32768 = 39%
Execution units:
  Memory: 4795416 / 30000000 = 15%
  Steps: 1688512780 / 10000000000 = 16%
```

The dispute that this is a problem is recorded in the transaction `abac4cb165bd328e13e9eb8f7fcad83d8c8f2a29fcf262acb8d97976fb697b82`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
abac4cb165bd328e13e9eb8f7fcad83d8c8f2a29fcf262acb8d97976fb697b82     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "194b7d8b30d8ce60a9c48016de840dda5d8effb8ec464b72b0c79066119d158b"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
abac4cb165bd328e13e9eb8f7fcad83d8c8f2a29fcf262acb8d97976fb697b82     0        11422990382 lovelace + TxOutDatumNone
abac4cb165bd328e13e9eb8f7fcad83d8c8f2a29fcf262acb8d97976fb697b82     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
```

## Transaction 5. The Mediator Confirms the Claim

Funds are released to the buyer and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe           \
                        --choice-name "Confirm claim"         \
                        --choice-party "Role=$MEDIATOR_ROLE"  \
                        --choice-number 1                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-5.marlowe               \
                        --print-stats
```

```console
Datum size: 104
Payment 1
  Acccount: "CM"
  Payee: Party "CM"
  Ada: 3.000000
Payment 2
  Acccount: "TM"
  Payee: Party "TM"
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
Fee: Lovelace 1100994
Size: 12784 / 32768 = 39%
Execution units:
  Memory: 4392754 / 30000000 = 14%
  Steps: 1525473633 / 10000000000 = 15%
```

The confirmation of the claim resulted in closing the contract, paying 256000000 lovelace to the role address for the benefit of the buyer Thomas Middleton and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `7725ef2f0fb5214e2a28f8c17a431e54c72aa510315e0435f47419f5ded8c516`.  There is no UTxO at the contract address:

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
7725ef2f0fb5214e2a28f8c17a431e54c72aa510315e0435f47419f5ded8c516     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
7725ef2f0fb5214e2a28f8c17a431e54c72aa510315e0435f47419f5ded8c516     2        256000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7725ef2f0fb5214e2a28f8c17a431e54c72aa510315e0435f47419f5ded8c516     0        21874635538 lovelace + TxOutDatumNone
7725ef2f0fb5214e2a28f8c17a431e54c72aa510315e0435f47419f5ded8c516     3        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
```

## Transactions 6 and 7. Buyer and Mediator Withdraw Funds.

The buyer Thomas Middleton submits a transaction to withdraw the repayment from the role address.

```
TX_6=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                         \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                         --marlowe-file tx-5.marlowe                           \
                         --role-name "$BUYER_ROLE"                             \
                         --tx-in "$TX_3"#0                                     \
                         --tx-in "$TX_3"#2                                     \
                         --tx-in-collateral "$TX_3"#0                          \
                         --required-signer "$BUYER_PAYMENT_SKEY"               \
                         --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                         --change-address "$BUYER_ADDRESS"                     \
                         --out-file tx-6.raw                                   \
                         --print-stats                                         \
                         --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                               \
)
```

```console
Fee: Lovelace 426739
Size: 2889 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
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

Here are the UTxOs at the buyer Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b316f7c822601e03e89457e176f2ac37dd8c5e60f9a4996e501e1e9400e3b401     0        10139993933 lovelace + TxOutDatumNone
b316f7c822601e03e89457e176f2ac37dd8c5e60f9a4996e501e1e9400e3b401     1        256000000 lovelace + TxOutDatumNone
b316f7c822601e03e89457e176f2ac37dd8c5e60f9a4996e501e1e9400e3b401     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f3538850159066fa8362ede4b0b2370e9ab72f062049df7aad5ccc4862d91a1f     0        21874208799 lovelace + TxOutDatumNone
f3538850159066fa8362ede4b0b2370e9ab72f062049df7aad5ccc4862d91a1f     1        3000000 lovelace + TxOutDatumNone
f3538850159066fa8362ede4b0b2370e9ab72f062049df7aad5ccc4862d91a1f     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
```

