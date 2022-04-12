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

The tip is at slot 4853022. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4853024. Tests may fail if this is not the case.

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
TxId "7495b5f7212077ec30484b8030b5b9cb2017762d1453bba134575599180dce8f"
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
TxId "2f66060a1927a0793443414ab3ca435944ee38822c61632cc1dc99d272144149"
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
TxId "f097f0ff1c0b1e72828da901931e5cdc91669dda36358ecbcc6e3356b83896c9"
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
TxId "1993603a92ec719f869c6cf276747eaff5f4d8207c3235d69f5dd559924e06b1"
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
TxId "2d35d2a06a3e8191e834aa8a5be829c82b5d4486638badd4a0454c9309260c3e"
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
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     0        157146684 lovelace + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.434d + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     2        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.434d + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     3        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.4642 + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     4        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.544d + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     5        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.434d + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     6        2000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.434d + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     7        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.434d + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     8        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.434d + TxOutDatumNone
e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227     9        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.434d + TxOutDatumNone
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

Christopher Marlowe will spend the UTxOs `e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227#0` and `e2cccdfa96d3ea5ae8492406ef133bf1d9c327de1e4d5b12ac8dc760f4003227#6`.

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
71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1     0        49778575 lovelace + TxOutDatumNone
71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.4642 + TxOutDatumNone
71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.4642 + TxOutDatumNone
71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1     3        2000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.4642 + TxOutDatumNone
71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1     4        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.4642 + TxOutDatumNone
71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1     5        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.4642 + TxOutDatumNone
71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1     6        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.4642 + TxOutDatumNone
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

Francis Beaumont will spend the UTxOs `71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1#0` and `71056d73920a5b6614c4e21c71ed988c0ee54d5c2c525d8c84b60fcdc792b6b1#3`.

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
a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0     0        605772547 lovelace + TxOutDatumNone
a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.544d + TxOutDatumNone
a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.544d + TxOutDatumNone
a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0     3        2000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.544d + TxOutDatumNone
a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0     4        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.544d + TxOutDatumNone
a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0     5        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.544d + TxOutDatumNone
a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0     6        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.544d + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0#0` and `a5ba36667a4e98e952526a9ec62e643bb791058c5509a1709f0e16e628e7fba0#3`.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))
```

* The current slot is 4853022.
* The buyer Thomas Middleton must pay before Wed, 13 Apr 2022 16:57:42 +0000.
* They buyer Thomas Middleton has until Thu, 14 Apr 2022 16:57:42 +0000 to complain.
* The seller Francis Beaumont has until Fri, 15 Apr 2022 16:57:42 +0000 to dispute a complaint.
* The mediator Christopher Marlowe has until Sat, 16 Apr 2022 16:57:42 +0000 to decide on a disputed complaint.

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

The Marlowe contract resides at address `addr_test1wr9yp6pnmncpex40jtvmdnflsvamaw22l5ftt360f5jwcfsrjqeug`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wrz68rewm94ttt7llg6eke49f9ymejc59z0fnhf5wz0flhgxsw99k`.

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

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `5cbc7e22a3011513d3828417cd6fee91b9ffabd2193968b10211e564b8001bf4`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5cbc7e22a3011513d3828417cd6fee91b9ffabd2193968b10211e564b8001bf4     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "440066dea6d85c61247ab7f7fea1a00c857a25af6a2980e394d38db7ad795e6e"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5cbc7e22a3011513d3828417cd6fee91b9ffabd2193968b10211e564b8001bf4     0        153948975 lovelace + TxOutDatumNone
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
Fee: Lovelace 1285788
Size: 13809 / 32768 = 42%
Execution units:
  Memory: 6041856 / 30000000 = 20%
  Steps: 2143241330 / 10000000000 = 21%
```

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `b28134e84358ada53f27d7b8cbd99b9a9eab8ff2bfc2669259dc850a05d40a62`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b28134e84358ada53f27d7b8cbd99b9a9eab8ff2bfc2669259dc850a05d40a62     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "092893995581ce95470851239318236453796b0a93a4f8539ee9f40f42768a0a"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b28134e84358ada53f27d7b8cbd99b9a9eab8ff2bfc2669259dc850a05d40a62     0        347486759 lovelace + TxOutDatumNone
b28134e84358ada53f27d7b8cbd99b9a9eab8ff2bfc2669259dc850a05d40a62     2        3000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.544d + TxOutDatumNone
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

The reporting of a problem was recorded in the transaction `4715a3603f74e8ce2f29c1adffe40dd87d73d08a9c7b6be7cef86ece1e41c5aa`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
4715a3603f74e8ce2f29c1adffe40dd87d73d08a9c7b6be7cef86ece1e41c5aa     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "39bc9207442fc62a18b14cf7d847aceef0375de2a5f1c39bdf8ef01e0b3d4fc9"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
4715a3603f74e8ce2f29c1adffe40dd87d73d08a9c7b6be7cef86ece1e41c5aa     0        346125537 lovelace + TxOutDatumNone
4715a3603f74e8ce2f29c1adffe40dd87d73d08a9c7b6be7cef86ece1e41c5aa     2        3000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.544d + TxOutDatumNone
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

The dispute that this is a problem is recorded in the transaction `25aa0deb2b4ffff5d28b2243583165297b3f826ec02e42458ee80d37e1e35766`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
25aa0deb2b4ffff5d28b2243583165297b3f826ec02e42458ee80d37e1e35766     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b63fbcd7881c848838889e94d1a59d303b78f3ee67137ee19193aa598deb2900"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
25aa0deb2b4ffff5d28b2243583165297b3f826ec02e42458ee80d37e1e35766     0        47540786 lovelace + TxOutDatumNone
25aa0deb2b4ffff5d28b2243583165297b3f826ec02e42458ee80d37e1e35766     2        3000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.4642 + TxOutDatumNone
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
Fee: Lovelace 1228239
Size: 13198 / 32768 = 40%
Execution units:
  Memory: 5758362 / 30000000 = 19%
  Steps: 1944800234 / 10000000000 = 19%
```

The dismissal of the claim resulted in closing the contract, paying 256000000 lovelace to the role address for the benefit of the seller Francis Beaumont and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `f330eecb56c89062f8244af119ab49ebc13c668ac937b9e1cf664072269203a4`.  There is no UTxO at the contract address:

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
f330eecb56c89062f8244af119ab49ebc13c668ac937b9e1cf664072269203a4     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
f330eecb56c89062f8244af119ab49ebc13c668ac937b9e1cf664072269203a4     2        256000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f330eecb56c89062f8244af119ab49ebc13c668ac937b9e1cf664072269203a4     0        151720736 lovelace + TxOutDatumNone
f330eecb56c89062f8244af119ab49ebc13c668ac937b9e1cf664072269203a4     3        3000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.434d + TxOutDatumNone
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
Fee: Lovelace 422221
Size: 2885 / 32768 = 8%
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
1a8d753e30a3752677e9450e7faca164d664db8285aae63840acf3f855e4270d     0        47118565 lovelace + TxOutDatumNone
1a8d753e30a3752677e9450e7faca164d664db8285aae63840acf3f855e4270d     1        256000000 lovelace + TxOutDatumNone
1a8d753e30a3752677e9450e7faca164d664db8285aae63840acf3f855e4270d     2        3000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.4642 + TxOutDatumNone
```

Here are the UTxOs at the buyer Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
4715a3603f74e8ce2f29c1adffe40dd87d73d08a9c7b6be7cef86ece1e41c5aa     0        346125537 lovelace + TxOutDatumNone
4715a3603f74e8ce2f29c1adffe40dd87d73d08a9c7b6be7cef86ece1e41c5aa     2        3000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.544d + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8015bab2732238869ffa6a5661fa837c0e05dee9750cc9c153bfc54d45bf348b     0        151294173 lovelace + TxOutDatumNone
8015bab2732238869ffa6a5661fa837c0e05dee9750cc9c153bfc54d45bf348b     1        3000000 lovelace + TxOutDatumNone
8015bab2732238869ffa6a5661fa837c0e05dee9750cc9c153bfc54d45bf348b     2        3000000 lovelace + 1 6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9.434d + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple "${MAGIC[@]}"                                        \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"            \
                               --tx-in "$TX_6"#0                                    \
                               --tx-in "$TX_6"#2                                    \
                               --tx-out "$MEDIATOR_ADDRESS+1400000+1 $SELLER_TOKEN" \
                               --required-signer "$SELLER_PAYMENT_SKEY"             \
                               --change-address "$FAUCET_ADDRESS"                   \
                               --out-file /dev/null                                 \
                               --submit 600
```

```console
TxId "f3f313b92e36a36239fcdf729aa0931c62cbf7e4fc36033df1aa267f2c666d06"
```

marlowe-cli transaction simple "${MAGIC[@]}"                                       \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"           \
                               --tx-in "$TX_3"#0                                   \
                               --tx-in "$TX_3"#2                                   \
                               --tx-out "$MEDIATOR_ADDRESS+1400000+1 $BUYER_TOKEN" \
                               --required-signer "$BUYER_PAYMENT_SKEY"             \
                               --change-address "$FAUCET_ADDRESS"                  \
                               --out-file /dev/null                                \
                               --submit 600
```

```console
TxId "8f79971c11ede73383b5bc9aab1feeace3707be02fc1ff917405e7921f84e4b3"
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
PolicyID "6637983d5fda592fb7ba561b79cdbc1324589ad1eb2b9d8420228dc9"
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
TxId "b833953774cf179eb8c406e4e2e408da325c22d12c5babb87eb3151e33e4bc99"
