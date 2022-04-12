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

The tip is at slot 4852082. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4852085. Tests may fail if this is not the case.

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
TxId "d65ba4128c9a9d350090b58cee32a042199d1ad348d4ac370c8a4ef12687c9be"
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
TxId "ee2efa9c7aa9d1d45fbff41757082b128e44f36e2272efafb44595aa4d3937c3"
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
TxId "53a1365505503d7b46db2d0abb6964be0613f52692bec55887d290025d5e38b2"
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
TxId "91238dba57b520e60f81c5eb2e7d7abad1ea569a7f2f3e692e067ce9b23fe50c"
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
TxId "a58f92d759571cef8afc4bbfa63eb1a6aff1495795f66abd2b56c0aeee4e3a9e"
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
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     0        157146684 lovelace + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.434d + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     2        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.434d + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     3        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.4642 + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     4        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.544d + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     5        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.434d + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     6        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.434d + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     7        2000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.434d + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     8        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.434d + TxOutDatumNone
923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9     9        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.434d + TxOutDatumNone
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

Christopher Marlowe will spend the UTxOs `923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9#0` and `923dcdc5967b9843ea7770498a1f802504d1778790ee21c2eaf89e90255d16a9#7`.

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
9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd     0        305772547 lovelace + TxOutDatumNone
9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.4642 + TxOutDatumNone
9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.4642 + TxOutDatumNone
9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd     3        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.4642 + TxOutDatumNone
9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd     4        2000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.4642 + TxOutDatumNone
9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd     5        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.4642 + TxOutDatumNone
9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd     6        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.4642 + TxOutDatumNone
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

Francis Beaumont will spend the UTxOs `9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd#0` and `9a014fd5c3bbaf301308d5fb93482752f141c61312fc8d6c99527ebb076d8dbd#4`.

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
0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255     0        349778575 lovelace + TxOutDatumNone
0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.544d + TxOutDatumNone
0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.544d + TxOutDatumNone
0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255     3        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.544d + TxOutDatumNone
0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255     4        2000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.544d + TxOutDatumNone
0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255     5        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.544d + TxOutDatumNone
0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255     6        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.544d + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255#0` and `0f9fc138565576b117d975e6684f2a339917d2d37684625dc204cc4d88114255#4`.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))
```

* The current slot is 4852082.
* The buyer Thomas Middleton must pay before Wed, 13 Apr 2022 16:42:02 +0000.
* They buyer Thomas Middleton has until Thu, 14 Apr 2022 16:42:02 +0000 to complain.
* The seller Francis Beaumont has until Fri, 15 Apr 2022 16:42:02 +0000 to dispute a complaint.
* The mediator Christopher Marlowe has until Sat, 16 Apr 2022 16:42:02 +0000 to decide on a disputed complaint.

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

The Marlowe contract resides at address `addr_test1wr79kelsj5xhmtfnwnq2jgss8mzlngz0t7cgjtagjt6pyvcv7xzvg`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wr4qxjpmee6zwxz4ntkkmjqpwqs77dvxf829kw6hhvcc5xq5ajxes`.

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

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `6b80920a43a354d8a254c29847ca88c1657641a3db5d66a86b7e503796bc27ec`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6b80920a43a354d8a254c29847ca88c1657641a3db5d66a86b7e503796bc27ec     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "2e53fc40513b2bf65f162967ea4dcaf763c13f18a46cc9a5c001cfecb1a03658"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6b80920a43a354d8a254c29847ca88c1657641a3db5d66a86b7e503796bc27ec     0        153948975 lovelace + TxOutDatumNone
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

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `a0b3837ce2d8faeab28c1a51351b4e9aeea98cfa3dc8bd0f69487fb7d89efd16`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a0b3837ce2d8faeab28c1a51351b4e9aeea98cfa3dc8bd0f69487fb7d89efd16     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5c8c194dd9f61d4b3dc88edc234f1fa4bfe96ca2b7338ba06d48f73228d65ce5"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a0b3837ce2d8faeab28c1a51351b4e9aeea98cfa3dc8bd0f69487fb7d89efd16     0        91497160 lovelace + TxOutDatumNone
a0b3837ce2d8faeab28c1a51351b4e9aeea98cfa3dc8bd0f69487fb7d89efd16     2        3000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.544d + TxOutDatumNone
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
Fee: Lovelace 1361222
Size: 13664 / 32768 = 41%
Execution units:
  Memory: 7091694 / 30000000 = 23%
  Steps: 2437803602 / 10000000000 = 24%
```

The reporting of a problem was recorded in the transaction `5e5ec6589cb4ced4bc2e314fee50a69b8e9031b3a16aecc1b0d4a5b4e606895c`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5e5ec6589cb4ced4bc2e314fee50a69b8e9031b3a16aecc1b0d4a5b4e606895c     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "6f3d569b5fc7256628296d609800a893cd962fb05e7a26a5b053f23a40596a1c"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5e5ec6589cb4ced4bc2e314fee50a69b8e9031b3a16aecc1b0d4a5b4e606895c     0        90135938 lovelace + TxOutDatumNone
5e5ec6589cb4ced4bc2e314fee50a69b8e9031b3a16aecc1b0d4a5b4e606895c     2        3000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.544d + TxOutDatumNone
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
Fee: Lovelace 1237789
Size: 13464 / 32768 = 41%
Execution units:
  Memory: 5700768 / 30000000 = 19%
  Steps: 1961013180 / 10000000000 = 19%
```

The dispute that this is a problem is recorded in the transaction `2b349dc2a9b8d3707e280a76768a441a18b3ab43b93ac4af2d13b114d85b2e2e`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2b349dc2a9b8d3707e280a76768a441a18b3ab43b93ac4af2d13b114d85b2e2e     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "d13a4f829608df0013e7d48b4c3ec31ada2003cd058e0e0894fa65e8b1200281"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2b349dc2a9b8d3707e280a76768a441a18b3ab43b93ac4af2d13b114d85b2e2e     0        303534758 lovelace + TxOutDatumNone
2b349dc2a9b8d3707e280a76768a441a18b3ab43b93ac4af2d13b114d85b2e2e     2        3000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.4642 + TxOutDatumNone
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
Fee: Lovelace 1143911
Size: 13198 / 32768 = 40%
Execution units:
  Memory: 4703770 / 30000000 = 15%
  Steps: 1619167575 / 10000000000 = 16%
```

The confirmation of the claim resulted in closing the contract, paying 256000000 lovelace to the role address for the benefit of the buyer Thomas Middleton and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `2653f964a61b160d2f21122c96d4a9c770c3e2a213c497a6b023b1e1968db3fc`.  There is no UTxO at the contract address:

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
2653f964a61b160d2f21122c96d4a9c770c3e2a213c497a6b023b1e1968db3fc     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
2653f964a61b160d2f21122c96d4a9c770c3e2a213c497a6b023b1e1968db3fc     2        256000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2653f964a61b160d2f21122c96d4a9c770c3e2a213c497a6b023b1e1968db3fc     0        151805064 lovelace + TxOutDatumNone
2653f964a61b160d2f21122c96d4a9c770c3e2a213c497a6b023b1e1968db3fc     3        3000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.434d + TxOutDatumNone
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
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
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
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
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
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2b349dc2a9b8d3707e280a76768a441a18b3ab43b93ac4af2d13b114d85b2e2e     0        303534758 lovelace + TxOutDatumNone
2b349dc2a9b8d3707e280a76768a441a18b3ab43b93ac4af2d13b114d85b2e2e     2        3000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.4642 + TxOutDatumNone
```

Here are the UTxOs at the buyer Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5104c8f6ac000fa2d5c292ce71f7e7318acf6a09fb11f64b88375b17f9df8e1f     0        89709375 lovelace + TxOutDatumNone
5104c8f6ac000fa2d5c292ce71f7e7318acf6a09fb11f64b88375b17f9df8e1f     1        256000000 lovelace + TxOutDatumNone
5104c8f6ac000fa2d5c292ce71f7e7318acf6a09fb11f64b88375b17f9df8e1f     2        3000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.544d + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e86d1b0945f7b2f1c878fa11470f873aeeae31eaa93491d8645f73eedebf8e3a     0        151378501 lovelace + TxOutDatumNone
e86d1b0945f7b2f1c878fa11470f873aeeae31eaa93491d8645f73eedebf8e3a     1        3000000 lovelace + TxOutDatumNone
e86d1b0945f7b2f1c878fa11470f873aeeae31eaa93491d8645f73eedebf8e3a     2        3000000 lovelace + 1 a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad.434d + TxOutDatumNone
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
TxId "5fd55d77947a0df054b836b2a353e355b7c53f0d8ebe0d6f371307f45c996cfb"
```

marlowe-cli transaction simple "${MAGIC[@]}"                                       \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"           \
                               --tx-in "$TX_6"#0                                   \
                               --tx-in "$TX_6"#2                                   \
                               --tx-out "$MEDIATOR_ADDRESS+1400000+1 $BUYER_TOKEN" \
                               --required-signer "$BUYER_PAYMENT_SKEY"             \
                               --change-address "$FAUCET_ADDRESS"                  \
                               --out-file /dev/null                                \
                               --submit 600
```

```console
TxId "393a14664bddd8ffaffee79e01f6bdcaa3eb3a88b5934e358dbf3cf24f443656"
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
PolicyID "a0b724655f3517534a7b834bcf594c42d5809ac4a77d42ac6ab8ebad"
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
TxId "46834b533a5af2974539d87fc4420eba45a88063bb3e139701380e7ca721f914"
