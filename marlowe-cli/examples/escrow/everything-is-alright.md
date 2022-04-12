# Example Escrow Contract: "Everything is alright"

In this example execution of [an escrow contract](ReadMe.md), the buyer does not report a problem.

![Flow chart for "everything is alright".](everything-is-alright.svg)

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the bystander and party roles, or they will be created automatically: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `MEDIATOR_PREFIX` where they appear below.

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

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
```

The tip is at slot 4851683. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4851702. Tests may fail if this is not the case.

### Participants

#### The Seller

The seller sells an item for a price.

```
SELLER_PREFIX="$TREASURY/francis-beaumont"
SELLER_NAME="Francis Beaumont"
SELLER_ROLE=FB
SELLER_PAYMENT_SKEY="$SELLER_PREFIX".skey
SELLER_PAYMENT_VKEY="$SELLER_PREFIX".vkey
```

Create the seller's keys, if necessary.

```
if [[ ! -e "$SELLER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$SELLER_PAYMENT_SKEY"      \
                              --verification-key-file "$SELLER_PAYMENT_VKEY"
fi
SELLER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$SELLER_PAYMENT_VKEY")
```

Fund the seller's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        "$SELLER_ADDRESS"
```

```console
TxId "77cbb1f87aeec184614b6bef3c91530cef1125fb28d5155451c1d5fc27ce0389"
```

### The Buyer

```
BUYER_PREFIX="$TREASURY/thomas-middleton"
BUYER_NAME="Thomas Middleton"
BUYER_ROLE=TM
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey
```

Create the buyer's keys, if necessary.

```
if [[ ! -e "$BUYER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$BUYER_PAYMENT_SKEY"      \
                              --verification-key-file "$BUYER_PAYMENT_VKEY"
fi
BUYER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$BUYER_PAYMENT_VKEY")
```

Fund the buyer's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 350000000                      \
                        "$BUYER_ADDRESS"
```

```console
TxId "a1ef7d543a2bedb52e17b6a61d1f425b880eedd0fa2f7de771b80ccde3602c2d"
```

### The Mediator

```
MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_ROLE=CM
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey
```

Create the mediator's keys, if necessary.

```
if [[ ! -e "$MEDIATOR_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$MEDIATOR_PAYMENT_SKEY"      \
                              --verification-key-file "$MEDIATOR_PAYMENT_VKEY"
fi
MEDIATOR_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY")
```

Fund the mediator's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        "$MEDIATOR_ADDRESS"
```

```console
TxId "e45ae79f4fa0d021e346e0bc174f07b34d82919881f461a07dddfc8796a8b71c"
```

### Role Tokens

The mediator mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"     \
                      --required-signer "$MEDIATOR_PAYMENT_SKEY"    \
                      --change-address  "$MEDIATOR_ADDRESS"         \
                      --expires "$MINT_EXPIRES"                     \
                      --out-file /dev/null                          \
                      --submit=600                                  \
                      "$MEDIATOR_ROLE" "$SELLER_ROLE" "$BUYER_ROLE" \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                \
)
MEDIATOR_TOKEN="$ROLE_CURRENCY.$MEDIATOR_ROLE"
SELLER_TOKEN="$ROLE_CURRENCY.$SELLER_ROLE"
BUYER_TOKEN="$ROLE_CURRENCY.$BUYER_ROLE"
```

Find the transaction output with the seller's role token.

```
TX_MINT_SELLER=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$SELLER_TOKEN"              \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the seller their role token.

```
marlowe-cli transaction simple "${MAGIC[@]}"                                      \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"          \
                               --tx-in "$TX_MINT_SELLER"                          \
                               --tx-out "$SELLER_ADDRESS+2000000+1 $SELLER_TOKEN" \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY"         \
                               --change-address "$MEDIATOR_ADDRESS"               \
                               --out-file /dev/null                               \
                               --submit 600
```

```console
TxId "68d136034903e722494b6e1545d1312e2912be9e948dc5858224b7d69406063f"
```

Find the transaction output with the buyer's role token.

```
TX_MINT_BUYER=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BUYER_TOKEN"               \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the buyer their role token.

```
marlowe-cli transaction simple "${MAGIC[@]}"                                    \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"        \
                               --tx-in "$TX_MINT_BUYER"                         \
                               --tx-out "$BUYER_ADDRESS+2000000+1 $BUYER_TOKEN" \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY"       \
                               --change-address "$MEDIATOR_ADDRESS"             \
                               --out-file /dev/null                             \
                               --submit 600
```

```console
TxId "52677e874edee4f4d8c1e39f41de357154a9a45c38ce28ef53e3da31ae9b0e72"
```

### Available UTxOs

The mediator Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz` and role token named `CM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$MEDIATOR_PAYMENT_SKEY"\
                       --change-address "$MEDIATOR_ADDRESS"      \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     0        329759734 lovelace + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.434d + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     2        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.434d + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     3        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.4642 + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     4        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.544d + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     5        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.434d + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     6        2000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.434d + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     7        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.434d + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     8        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.434d + TxOutDatumNone
fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583     9        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.434d + TxOutDatumNone
```

We select the UTxO with the mediator Christopher Marlowe's role token.

```
TX_0_MEDIATOR_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 120000000                 \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_MEDIATOR_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$MEDIATOR_TOKEN"              \
                        "$MEDIATOR_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Christopher Marlowe will spend the UTxOs `fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583#0` and `fd267b69380c0b91a9b2c7df248a44d59ccb7161fcc0f0150491b2e645faa583#6`.

The seller Francis Beaumont has the address `addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q` and role token named `FB`. They have the following UTxOs in their wallet:

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
8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc     0        361621787 lovelace + TxOutDatumNone
8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.4642 + TxOutDatumNone
8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.4642 + TxOutDatumNone
8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc     3        2000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.4642 + TxOutDatumNone
8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc     4        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.4642 + TxOutDatumNone
8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc     5        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.4642 + TxOutDatumNone
8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc     6        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.4642 + TxOutDatumNone
```

We select the UTxO with the lender Francis Beaumont's role token.

```
TX_0_SELLER_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$SELLER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_SELLER_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$SELLER_TOKEN"              \
                        "$SELLER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Francis Beaumont will spend the UTxOs `8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc#0` and `8471c57b8533112cf86a7f7ed2d0c6a53c8cd63fa975e4e8d6efae2b7b3179cc#3`.

The buyer Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `TM`. They have the following UTxOs in their wallet:

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
62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4     0        449592459 lovelace + TxOutDatumNone
62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.544d + TxOutDatumNone
62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.544d + TxOutDatumNone
62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4     3        2000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.544d + TxOutDatumNone
62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4     4        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.544d + TxOutDatumNone
62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4     5        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.544d + TxOutDatumNone
62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4     6        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.544d + TxOutDatumNone
```

We select the UTxO with the lender Thomas Middleton's role token.

```
TX_0_BUYER_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BUYER_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_BUYER_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BUYER_TOKEN"               \
                        "$BUYER_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Middleton will spend the UTxOs `62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4#0` and `62d93cb82655fdd7a469cd703ee14a2f74ea263e850a794d5462b8ecc62709f4#3`.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))
```

* The current slot is 4851683.
* The buyer Thomas Middleton must pay before Wed, 13 Apr 2022 16:35:23 +0000.
* They buyer Thomas Middleton has until Thu, 14 Apr 2022 16:35:23 +0000 to complain.
* The seller Francis Beaumont has until Fri, 15 Apr 2022 16:35:23 +0000 to dispute a complaint.
* The mediator Christopher Marlowe has until Sat, 16 Apr 2022 16:35:23 +0000 to decide on a disputed complaint.

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
Validator size: 12379
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wrc0c860yypg6qjsmfdq803hxm5e7atel846hrpdppftjqqpal390`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wz9a77fe4ame4qy2e4423pjs9gqppz4tdsguewcqdhrznxg6y6mgj`.

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
Fee: Lovelace 197709
Size: 719 / 32768 = 2%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `bdd37bd27c47cfb978451d393add78c30d16275069abd04019ec8361666cb923`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bdd37bd27c47cfb978451d393add78c30d16275069abd04019ec8361666cb923     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "53b4248602c8196d8afb41f58dd212c3eb2827adabaf451eb47b2b126f15bc9b"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bdd37bd27c47cfb978451d393add78c30d16275069abd04019ec8361666cb923     0        326562025 lovelace + TxOutDatumNone
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
Fee: Lovelace 1281415
Size: 13809 / 32768 = 42%
Execution units:
  Memory: 5986656 / 30000000 = 19%
  Steps: 2126759426 / 10000000000 = 21%
```

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `b4ab8e80e839ef1e552b1c74fc76a5bc7289ad2e391a836395333f74253b48dd`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b4ab8e80e839ef1e552b1c74fc76a5bc7289ad2e391a836395333f74253b48dd     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e80ec9cd929c30b5c4ac5c003754c2c10029cd671ebc46dc72bcd84b3ab23c2"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b4ab8e80e839ef1e552b1c74fc76a5bc7289ad2e391a836395333f74253b48dd     0        191311044 lovelace + TxOutDatumNone
b4ab8e80e839ef1e552b1c74fc76a5bc7289ad2e391a836395333f74253b48dd     2        3000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.544d + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that Everything is Alright

Funds are released to the seller and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --choice-name "Everything is alright" \
                        --choice-party "Role=$BUYER_ROLE"     \
                        --choice-number 0                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats
```

```console
Datum size: 57
Payment 1
  Acccount: "CM"
  Payee: Party "CM"
  Ada: 3.000000
Payment 2
  Acccount: "FB"
  Payee: Party "FB"
  Ada: 256.000000
```

Now the buyer Thomas Middleton can submit a transaction to release funds:

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
Fee: Lovelace 1180504
Size: 13407 / 32768 = 40%
Execution units:
  Memory: 4994034 / 30000000 = 16%
  Steps: 1766862123 / 10000000000 = 17%
```

The closing of the contract paid 256000000 lovelace to the role address for the benefit of the seller Francis Beaumont and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `ed934398065a85c1e27a0eb7384241d559c6e81d0ee837535975f1dae0684259`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ed934398065a85c1e27a0eb7384241d559c6e81d0ee837535975f1dae0684259     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
ed934398065a85c1e27a0eb7384241d559c6e81d0ee837535975f1dae0684259     2        256000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
```

Here is the UTxO at the buyer Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ed934398065a85c1e27a0eb7384241d559c6e81d0ee837535975f1dae0684259     0        190130540 lovelace + TxOutDatumNone
ed934398065a85c1e27a0eb7384241d559c6e81d0ee837535975f1dae0684259     3        3000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.544d + TxOutDatumNone
```

## Transactions 4 and 5. Seller and Mediator Withdraw Funds.

The seller Francis Beaumont submits a transaction to withdraw the payment from the role address.

```
TX_4=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                           \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-3.marlowe                             \
                         --role-name "$SELLER_ROLE"                              \
                         --tx-in "$TX_0_SELLER_ADA"                              \
                         --tx-in "$TX_0_SELLER_TOKEN"                            \
                         --tx-in-collateral "$TX_0_SELLER_ADA"                   \
                         --required-signer "$SELLER_PAYMENT_SKEY"                \
                         --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA+1 $SELLER_TOKEN" \
                         --change-address "$SELLER_ADDRESS"                      \
                         --out-file tx-4.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)
```

```console
Fee: Lovelace 422221
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1407010 / 30000000 = 4%
  Steps: 541567360 / 10000000000 = 5%
```

The mediator Christopher Marlowe submits a transaction to withdraw the minimum ADA from the role address.

```
TX_5=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                               \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                         --marlowe-file tx-3.marlowe                                 \
                         --role-name "$MEDIATOR_ROLE"                                \
                         --tx-in "$TX_1"#0                                           \
                         --tx-in "$TX_0_MEDIATOR_TOKEN"                              \
                         --tx-in-collateral "$TX_1"#0                                \
                         --required-signer "$MEDIATOR_PAYMENT_SKEY"                  \
                         --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA+1 $MEDIATOR_TOKEN" \
                         --change-address "$MEDIATOR_ADDRESS"                        \
                         --out-file tx-5.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)
```

```console
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
```

There is no UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
58224eee8aa2db665921e2a3bcbe621f63668d128d0704251f4b971dd8f904f1     0        360199566 lovelace + TxOutDatumNone
58224eee8aa2db665921e2a3bcbe621f63668d128d0704251f4b971dd8f904f1     1        256000000 lovelace + TxOutDatumNone
58224eee8aa2db665921e2a3bcbe621f63668d128d0704251f4b971dd8f904f1     2        3000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.4642 + TxOutDatumNone
```

Here are the UTxOs at the buyer Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ed934398065a85c1e27a0eb7384241d559c6e81d0ee837535975f1dae0684259     0        190130540 lovelace + TxOutDatumNone
ed934398065a85c1e27a0eb7384241d559c6e81d0ee837535975f1dae0684259     3        3000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.544d + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b0cf613e0b3e78dbf50d9b74b461a271ea4120c4958b86cd941ac53a0246e2e6     0        325135462 lovelace + TxOutDatumNone
b0cf613e0b3e78dbf50d9b74b461a271ea4120c4958b86cd941ac53a0246e2e6     1        3000000 lovelace + TxOutDatumNone
b0cf613e0b3e78dbf50d9b74b461a271ea4120c4958b86cd941ac53a0246e2e6     2        3000000 lovelace + 1 7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0.434d + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple "${MAGIC[@]}"                                        \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"            \
                               --tx-in "$TX_4"#0                                    \
                               --tx-in "$TX_4"#2                                    \
                               --tx-out "$MEDIATOR_ADDRESS+1400000+1 $SELLER_TOKEN" \
                               --required-signer "$SELLER_PAYMENT_SKEY"             \
                               --change-address "$FAUCET_ADDRESS"                   \
                               --out-file /dev/null                                 \
                               --submit 600
```

```console
TxId "6a03d2f8a937224c949e93428366d7083abdd4faf623076c9ce254b1c9aa1ad1"
```

marlowe-cli transaction simple "${MAGIC[@]}"                                       \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"           \
                               --tx-in "$TX_3"#0                                   \
                               --tx-in "$TX_3"#3                                   \
                               --tx-out "$MEDIATOR_ADDRESS+1400000+1 $BUYER_TOKEN" \
                               --required-signer "$BUYER_PAYMENT_SKEY"             \
                               --change-address "$FAUCET_ADDRESS"                  \
                               --out-file /dev/null                                \
                               --submit 600
```

```console
TxId "16afa32123679cafe336ea692e39075b9ac73e20407ddeb70ef6775dbff42425"
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"     \
                      --required-signer "$MEDIATOR_PAYMENT_SKEY"    \
                      --change-address  "$MEDIATOR_ADDRESS"         \
                      --count -1                                    \
                      --expires "$MINT_EXPIRES"                     \
                      --out-file /dev/null                          \
                      --submit=600                                  \
                      "$MEDIATOR_ROLE" "$SELLER_ROLE" "$BUYER_ROLE"
```

```console
PolicyID "7231df33cd154c6ae7ef0d83ae158321eb108baf65941ed81f916da0"
TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple "${MAGIC[@]}"                              \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                               --tx-in "$TX"                              \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"         \
                               --out-file /dev/null                       \
                               --submit 600
```

```console
TxId "40104b051f80d22baf559bdc61db7dec9ed721d224a971bea99420f48a3da4e1"
