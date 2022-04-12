# Test of a Contract for Differences

In this example execution of a contract for differences, the party and counterparty each make a deposit to cover margin, an oracle reports prices, and the price change is settled among the parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the oracle and party roles, or they will be created automatically: to do this, set the environment variables `PARTY_PREFIX`, `COUNTERPARTY_PREFIX`, and `ORACLE_PREFIX` where they appear below.

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
MINUTE="$((60*1000))"
```

The tip is at slot 4853320. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4853320. Tests may fail if this is not the case.

### Participants

#### The Party

The party sells an item for a price.

```
PARTY_PREFIX="$TREASURY/francis-beaumont"
PARTY_NAME="Francis Beaumont"
PARTY_ROLE=FB
PARTY_PAYMENT_SKEY="$PARTY_PREFIX".skey
PARTY_PAYMENT_VKEY="$PARTY_PREFIX".vkey
```

Create the party's keys, if necessary.

```
if [[ ! -e "$PARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_PAYMENT_VKEY"
fi
PARTY_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_PAYMENT_VKEY")
```

Fund the party's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        "$PARTY_ADDRESS"
```

```console
TxId "d381ce3f18fd483f1d5ba47620192feaddb7077c88392e4d5039fd34e3d23970"
```

### The Counterparty

```
COUNTERPARTY_PREFIX="$TREASURY/thomas-middleton"
COUNTERPARTY_NAME="Thomas Middleton"
COUNTERPARTY_ROLE=TM
COUNTERPARTY_PAYMENT_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_PAYMENT_VKEY="$COUNTERPARTY_PREFIX".vkey
```

Create the counterparty's keys, if necessary.

```
if [[ ! -e "$COUNTERPARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$COUNTERPARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$COUNTERPARTY_PAYMENT_VKEY"
fi
COUNTERPARTY_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY")
```

Fund the counterparty's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        "$COUNTERPARTY_ADDRESS"
```

```console
TxId "9598867629f971597f4a3282c60c587781939ea814e4dda4ee917fd07001bcae"
```

### The Oracle

```
ORACLE_PREFIX="$TREASURY/christopher-marlowe"
ORACLE_NAME="Christopher Marlowe"
ORACLE_ROLE=CM
ORACLE_PAYMENT_SKEY="$ORACLE_PREFIX".skey
ORACLE_PAYMENT_VKEY="$ORACLE_PREFIX".vkey
```

Create the oracle's keys, if necessary.

```
if [[ ! -e "$ORACLE_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$ORACLE_PAYMENT_SKEY"      \
                              --verification-key-file "$ORACLE_PAYMENT_VKEY"
fi
ORACLE_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$ORACLE_PAYMENT_VKEY")
```

Fund the oracle's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        "$ORACLE_ADDRESS"
```

```console
TxId "95a470c19366ac6978a39cedb1b01e5df98f79c8e3a9c09ebad1d7d18c3279d4"
```

### Role Tokens

The oracle mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                      --required-signer "$ORACLE_PAYMENT_SKEY"          \
                      --change-address  "$ORACLE_ADDRESS"               \
                      --expires "$MINT_EXPIRES"                         \
                      --out-file /dev/null                              \
                      --submit=600                                      \
                      "$ORACLE_ROLE" "$PARTY_ROLE" "$COUNTERPARTY_ROLE" \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                    \
)
ORACLE_TOKEN="$ROLE_CURRENCY.$ORACLE_ROLE"
PARTY_TOKEN="$ROLE_CURRENCY.$PARTY_ROLE"
COUNTERPARTY_TOKEN="$ROLE_CURRENCY.$COUNTERPARTY_ROLE"
```

Find the transaction output with the party's role token.

```
TX_MINT_PARTY=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$PARTY_TOKEN"               \
                        "$ORACLE_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the party their role token.

```
marlowe-cli transaction simple "${MAGIC[@]}"                                    \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"        \
                               --tx-in "$TX_MINT_PARTY"                         \
                               --tx-out "$PARTY_ADDRESS+2000000+1 $PARTY_TOKEN" \
                               --required-signer "$ORACLE_PAYMENT_SKEY"         \
                               --change-address "$ORACLE_ADDRESS"               \
                               --out-file /dev/null                             \
                               --submit 600
```

```console
TxId "e66dbe1b2a669ea41a9675bcf8c5019f8314b5da00ef6366b947c7f05850e473"
```

Find the transaction output with the counterparty's role token.

```
TX_MINT_COUNTERPARTY=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$COUNTERPARTY_TOKEN"        \
                        "$ORACLE_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the counterparty their role token.

```
marlowe-cli transaction simple "${MAGIC[@]}"                                                  \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                      \
                               --tx-in "$TX_MINT_COUNTERPARTY"                                \
                               --tx-out "$COUNTERPARTY_ADDRESS+2000000+1 $COUNTERPARTY_TOKEN" \
                               --required-signer "$ORACLE_PAYMENT_SKEY"                       \
                               --change-address "$ORACLE_ADDRESS"                             \
                               --out-file /dev/null                                           \
                               --submit 600
```

```console
TxId "5f8ecba1cc9d4318bd338e6322a5f428df0f2e666913c23db9ca2e66f49b42ad"
```

### Available UTxOs

The oracle Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz` and role token named `CM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$ORACLE_PAYMENT_SKEY"  \
                       --change-address "$ORACLE_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     0        157146684 lovelace + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.434d + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     2        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.434d + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     3        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.4642 + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     4        2000000 lovelace + 1 4e24af19c3b1d518b1cba28ee5c32c6d638731966ad00fd0cfa5a929.544d + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     5        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.434d + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     6        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.434d + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     7        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.434d + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     8        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.434d + TxOutDatumNone
123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269     9        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.434d + TxOutDatumNone
```

We select the UTxO with the oracle Christopher Marlowe's role token.

```
TX_0_ORACLE_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$ORACLE_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_ORACLE_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$ORACLE_TOKEN"              \
                        "$ORACLE_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Christopher Marlowe will spend the UTxOs `123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269#0` and `123c436e4452c74f0c6a34326fcd3d5a29ff76b5068d3a2c62ebc4cc308e2269#9`.

The party Francis Beaumont has the address `addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q` and role token named `FB`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_PAYMENT_SKEY"   \
                       --change-address "$PARTY_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397     0        305772547 lovelace + TxOutDatumNone
7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.4642 + TxOutDatumNone
7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.4642 + TxOutDatumNone
7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397     3        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.4642 + TxOutDatumNone
7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397     4        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.4642 + TxOutDatumNone
7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397     5        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.4642 + TxOutDatumNone
7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397     6        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.4642 + TxOutDatumNone
```

We select the UTxO with the lender Francis Beaumont's role token.

```
TX_0_PARTY_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_PARTY_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$PARTY_TOKEN"               \
                        "$PARTY_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Francis Beaumont will spend the UTxOs `7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397#0` and `7c1970a1974158b966bfa4ad93e5ad00c14e6aaa72cee593f5c89467b1097397#6`.

The counterparty Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `TM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                       --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                       --change-address "$COUNTERPARTY_ADDRESS"       \
                       --out-file /dev/null                           \
                       --submit=600                                   \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008     0        49778575 lovelace + TxOutDatumNone
410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.544d + TxOutDatumNone
410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008     2        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.544d + TxOutDatumNone
410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008     3        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.544d + TxOutDatumNone
410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008     4        2000000 lovelace + 1 a2f32f2ffcf0be35654d5b1e5ea525bd83ece8fc4bb4631664c86f25.544d + TxOutDatumNone
410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008     5        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.544d + TxOutDatumNone
410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008     6        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.544d + TxOutDatumNone
```

We select the UTxO with the lender Thomas Middleton's role token.

```
TX_0_COUNTERPARTY_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_COUNTERPARTY_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$COUNTERPARTY_TOKEN"        \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Middleton will spend the UTxOs `410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008#0` and `410dcd642455989eada1102234af14db52f3bc7c2aefb3caf294be7328f71008#6`.

## The Contract

The contract has several deadlines. These are set to unrealistic values so that the example can be run quickly.

```
PARTY_MARGIN_DEADLINE="$((NOW+12*HOUR))"
COUNTERPARTY_MARGIN_DEADLINE="$((NOW+18*HOUR))"
FIRST_PRICE_TIME="$((NOW-20*MINUTE))"
FIRST_PRICE_DEADLINE="$((NOW+24*HOUR))"
SECOND_PRICE_TIME="$((NOW-19*MINUTE))"
SECOND_PRICE_DEADLINE="$((NOW+48*HOUR))"
```

The contract also involves the margin deposit amount and a minimum-ADA value.

```
MINIMUM_ADA=2000000
PARTY_MARGIN=7000000
COUNTERPARTY_MARGIN=8000000
```

The oracle will report a pair of prices.

```
FIRST_PRICE=1000000
SECOND_PRICE=1500000
```

* The current slot is 4853320.
* The party Francis Beaumont must pay the margin deposit of 7000000 lovelace before Wed, 13 Apr 2022 05:02:40 +0000.
* The counterparty Thomas Middleton must pay the margin deposit of 8000000 lovelace before Wed, 13 Apr 2022 11:02:40 +0000.
* The oracle Christopher Marlowe report the first price of 1000000 lovelace between Tue, 12 Apr 2022 16:42:40 +0000 and Wed, 13 Apr 2022 17:02:40 +0000.
* The oracle Christopher Marlowe report the second price of 1500000 lovelace between Tue, 12 Apr 2022 16:43:40 +0000 and Thu, 14 Apr 2022 17:02:40 +0000.

We create the contract for the previously specified parameters. User Marlowe Playground to create contracts and export them to JSON files like the following.

```
cat > tx-1.contract << EOI
{
   "when" : [
      {
         "case" : {
            "party" : { "role_token" : "$PARTY_ROLE" },
            "of_token" : { "currency_symbol" : "", "token_name" : "" },
            "deposits" : $PARTY_MARGIN,
            "into_account" : { "role_token" : "$PARTY_ROLE" }
         },
         "then" : {
            "when" : [
               {
                  "case" : {
                     "party" : { "role_token" : "$COUNTERPARTY_ROLE" },
                     "deposits" : $COUNTERPARTY_MARGIN,
                     "of_token" : { "currency_symbol" : "", "token_name" : "" },
                     "into_account" : { "role_token" : "$COUNTERPARTY_ROLE" }
                  },
                  "then" : {
                     "when" : [],
                     "timeout" : $FIRST_PRICE_TIME,
                     "timeout_continuation" : {
                        "when" : [
                           {
                              "case" : {
                                 "choose_between" : [ { "from" : 0, "to" : 1000000000 } ],
                                 "for_choice" : {
                                    "choice_name" : "Price in first window",
                                    "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                 }
                              },
                              "then" : {
                                 "when" : [],
                                 "timeout" : $SECOND_PRICE_TIME,
                                 "timeout_continuation" : {
                                    "when" : [
                                       {
                                          "case" : {
                                             "choose_between" : [ { "from" : 0, "to" : 1000000000 } ],
                                             "for_choice" : {
                                                "choice_name" : "Price in second window",
                                                "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                             }
                                          },
                                          "then" : {
                                             "if" : {
                                                "value" : {
                                                   "value_of_choice" : {
                                                      "choice_name" : "Price in first window",
                                                      "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                   }
                                                },
                                                "gt" : {
                                                   "value_of_choice" : {
                                                      "choice_name" : "Price in second window",
                                                      "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                   }
                                                }
                                             },
                                             "then" : {
                                                "let" : "Decrease in price",
                                                "be" : {
                                                   "value" : {
                                                      "value_of_choice" : {
                                                         "choice_name" : "Price in first window",
                                                         "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                      }
                                                   },
                                                   "minus" : {
                                                      "value_of_choice" : {
                                                         "choice_name" : "Price in second window",
                                                         "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                      }
                                                   }
                                                },
                                                "then" : {
                                                   "from_account" : { "role_token" : "$COUNTERPARTY_ROLE" },
                                                   "pay" : {
                                                      "if" : {
                                                         "value" : { "use_value" : "Decrease in price" },
                                                         "lt" : $COUNTERPARTY_MARGIN
                                                      },
                                                      "then" : {
                                                         "use_value" : "Decrease in price"
                                                      },
                                                      "else" : $COUNTERPARTY_MARGIN
                                                   },
                                                   "token" : { "currency_symbol" : "", "token_name" : "" },
                                                   "to" : { "account" : { "role_token" : "$PARTY_ROLE" } },
                                                   "then" : "close"
                                                }
                                             },
                                             "else" : {
                                                "if" : {
                                                   "value" : {
                                                      "value_of_choice" : {
                                                         "choice_name" : "Price in first window",
                                                         "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                      }
                                                   },
                                                   "lt" : {
                                                      "value_of_choice" : {
                                                         "choice_name" : "Price in second window",
                                                         "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                      }
                                                   }
                                                },
                                                "then" : {
                                                   "let" : "Increase in price",
                                                   "be" : {
                                                      "value" : {
                                                         "value_of_choice" : {
                                                            "choice_name" : "Price in second window",
                                                            "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                         }
                                                      },
                                                      "minus" : {
                                                         "value_of_choice" : {
                                                            "choice_name" : "Price in first window",
                                                            "choice_owner" : { "role_token" : "$ORACLE_ROLE" }
                                                         }
                                                      }
                                                   },
                                                   "then" : {
                                                      "pay" : {
                                                         "if" : {
                                                            "value" : { "use_value" : "Increase in price" },
                                                            "lt" : $PARTY_MARGIN
                                                         },
                                                         "then" : {
                                                            "use_value" : "Increase in price"
                                                         },
                                                         "else" : $PARTY_MARGIN
                                                      },
                                                      "from_account" : { "role_token" : "$PARTY_ROLE" },
                                                      "token" : { "currency_symbol" : "", "token_name" : "" },
                                                      "to" : { "account" : { "role_token" : "$COUNTERPARTY_ROLE" } },
                                                      "then" : "close"
                                                   }
                                                },
                                                "else" : "close"
                                             }
                                          }
                                       }
                                    ],
                                    "timeout" : $SECOND_PRICE_DEADLINE,
                                    "timeout_continuation" : "close"
                                 }
                              }
                           }
                        ],
                        "timeout" : $FIRST_PRICE_DEADLINE,
                        "timeout_continuation" : "close"
                     }
                  }
               }
            ],
            "timeout" : $COUNTERPARTY_MARGIN_DEADLINE,
            "timeout_continuation" : "close"
         }
      }
   ],
   "timeout" : $PARTY_MARGIN_DEADLINE,
   "timeout_continuation" : "close"
}
EOI
```

Also create the initial state for the contract, which simply has the oracle starting the contract with the minimum required ADA.

```
cat > tx-1.state << EOI
{
    "accounts": [
        [
            [
                { "role_token": "$ORACLE_ROLE" },
                { "currency_symbol": "", "token_name": "" }
            ],
            $MINIMUM_ADA
        ]
    ],
    "choices": [],
    "boundValues": [],
    "minTime": $((NOW-30*MINUTE))
}
EOI
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

The Marlowe contract resides at address `addr_test1wqs6a7qmwlj5fkw4vz5g427zfgnvl0z6xtpnute7jaau3kgncu5up`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wp23jcs97zaez0r2r3yjkzddvhu78dnkqrslw2hvukzarrq77u27k`.

The oracle Christopher Marlowe submits the transaction along with the minimum ADA 2000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_ORACLE_ADA"                \
                        --change-address "$ORACLE_ADDRESS"        \
                        --required-signer "$ORACLE_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-1.marlowe           \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 217333
Size: 1165 / 32768 = 3%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 2000000 lovelace from the oracle Christopher Marlowe in the transaction `0b3523d0f70e1a660149fb1bf91edce85e701936932e5e40c3ee8243352106fb`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0b3523d0f70e1a660149fb1bf91edce85e701936932e5e40c3ee8243352106fb     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "3cd068c923ea2c90db4493bcdc53c6d6e16691079f5fada7d75843921fe5233e"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0b3523d0f70e1a660149fb1bf91edce85e701936932e5e40c3ee8243352106fb     0        154929351 lovelace + TxOutDatumNone
```

## Transaction 2. Party Deposits Margin Funds into Their Account.

First we compute the Marlowe input required to make the margin deposit by the party.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$PARTY_ROLE"  \
                        --deposit-party "Role=$PARTY_ROLE"    \
                        --deposit-amount "$PARTY_MARGIN"      \
                        --invalid-before "$((NOW-18*MINUTE))" \
                        --invalid-hereafter "$((NOW+3*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 901
```

Now the party Francis Beaumont submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                                         \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-1.marlowe                        \
                        --tx-in-marlowe "$TX_1"#1                             \
                        --tx-in-collateral "$TX_0_PARTY_ADA"                  \
                        --tx-in "$TX_0_PARTY_ADA"                             \
                        --tx-in "$TX_0_PARTY_TOKEN"                           \
                        --required-signer "$PARTY_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-2.marlowe                       \
                        --tx-out "$PARTY_ADDRESS+$MINIMUM_ADA+1 $PARTY_TOKEN" \
                        --change-address "$PARTY_ADDRESS"                     \
                        --out-file tx-2.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                              \
)
```

```console
Fee: Lovelace 1390537
Size: 14693 / 32768 = 44%
Execution units:
  Memory: 6778248 / 30000000 = 22%
  Steps: 2467277051 / 10000000000 = 24%
```

The contract received the margin deposit of 7000000 lovelace in the transaction `e106962edfabb151727e44323b7d165cdb82d289b6ec84fbf0fa6c271aca531b`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e106962edfabb151727e44323b7d165cdb82d289b6ec84fbf0fa6c271aca531b     1        9000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "36118d6a3a2de675e6a04f2857a4f1369a74d902809d066c84337b3bbbefa8df"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e106962edfabb151727e44323b7d165cdb82d289b6ec84fbf0fa6c271aca531b     0        297382010 lovelace + TxOutDatumNone
e106962edfabb151727e44323b7d165cdb82d289b6ec84fbf0fa6c271aca531b     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.4642 + TxOutDatumNone
```

## Transaction 3. Counterparty Deposits Margin Funds into Their Account.

First we compute the Marlowe input required to make the margin deposit by the counterparty.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                 \
                        --deposit-account "Role=$COUNTERPARTY_ROLE" \
                        --deposit-party "Role=$COUNTERPARTY_ROLE"   \
                        --deposit-amount "$COUNTERPARTY_MARGIN"     \
                        --invalid-before "$((NOW-18*MINUTE))"       \
                        --invalid-hereafter "$((NOW+3*HOUR))"       \
                        --out-file tx-3.marlowe                     \
                        --print-stats
```

```console
Datum size: 854
```

Now the counterparty Thomas Middleton submits the transaction along with their deposit:

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                                                       \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                           \
                        --marlowe-in-file tx-2.marlowe                                      \
                        --tx-in-marlowe "$TX_2"#1                                           \
                        --tx-in-collateral "$TX_0_COUNTERPARTY_ADA"                         \
                        --tx-in "$TX_0_COUNTERPARTY_ADA"                                    \
                        --tx-in "$TX_0_COUNTERPARTY_TOKEN"                                  \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY"                      \
                        --marlowe-out-file tx-3.marlowe                                     \
                        --tx-out "$COUNTERPARTY_ADDRESS+$MINIMUM_ADA+1 $COUNTERPARTY_TOKEN" \
                        --change-address "$COUNTERPARTY_ADDRESS"                            \
                        --out-file tx-3.raw                                                 \
                        --print-stats                                                       \
                        --submit=600                                                        \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                            \
)
```

```console
Fee: Lovelace 1403433
Size: 14613 / 32768 = 44%
Execution units:
  Memory: 7002686 / 30000000 = 23%
  Steps: 2515343620 / 10000000000 = 25%
```

The contract received the margin deposit of 8000000 lovelace in the transaction `5f756fdadf789d32c568175be12250d1bde5c758e2e04395deccdf69a2be05f9`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5f756fdadf789d32c568175be12250d1bde5c758e2e04395deccdf69a2be05f9     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "d1408d772c219c233a50ce562dff73b355494ab92f158cf86cf12b7cbf63f5fd"
```

Here is the UTxO at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5f756fdadf789d32c568175be12250d1bde5c758e2e04395deccdf69a2be05f9     0        40375142 lovelace + TxOutDatumNone
5f756fdadf789d32c568175be12250d1bde5c758e2e04395deccdf69a2be05f9     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.544d + TxOutDatumNone
```

## Transaction 4. Oracle Reports the First Price.

First we compute the Marlowe input required for the oracle to report the price.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe           \
                        --choice-name "Price in first window" \
                        --choice-party "Role=$ORACLE_ROLE"    \
                        --choice-number "$FIRST_PRICE"        \
                        --invalid-before "$((NOW-18*MINUTE))" \
                        --invalid-hereafter "$((NOW+3*HOUR))" \
                        --out-file tx-4.marlowe               \
                        --print-stats
```

```console
Datum size: 807
```

Now the oracle Christopher Marlowe submits the transaction containing their price report:

```
TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                                           \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                        --marlowe-in-file tx-3.marlowe                          \
                        --tx-in-marlowe "$TX_3"#1                               \
                        --tx-in-collateral "$TX_1"#0                            \
                        --tx-in "$TX_1"#0                                       \
                        --tx-in "$TX_0_ORACLE_TOKEN"                            \
                        --required-signer "$ORACLE_PAYMENT_SKEY"                \
                        --marlowe-out-file tx-4.marlowe                         \
                        --tx-out "$ORACLE_ADDRESS+$MINIMUM_ADA+1 $ORACLE_TOKEN" \
                        --change-address "$ORACLE_ADDRESS"                      \
                        --out-file tx-4.raw                                     \
                        --print-stats                                           \
                        --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                \
)
```

```console
Fee: Lovelace 1387820
Size: 14532 / 32768 = 44%
Execution units:
  Memory: 6876264 / 30000000 = 22%
  Steps: 2449397708 / 10000000000 = 24%
```

The contract received the price report in the transaction `465300101b16dd297839a4dbd1e3284d9d3bfc0cd9c42b1f8da6d606087968f1`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
465300101b16dd297839a4dbd1e3284d9d3bfc0cd9c42b1f8da6d606087968f1     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e7554c44f47866f785afb5fc3e65a2b207fd78a513ccda6106b96887dd08624"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
465300101b16dd297839a4dbd1e3284d9d3bfc0cd9c42b1f8da6d606087968f1     0        153541531 lovelace + TxOutDatumNone
465300101b16dd297839a4dbd1e3284d9d3bfc0cd9c42b1f8da6d606087968f1     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.434d + TxOutDatumNone
```

## Transaction 5. Oracle Reports the Second Price.

First we compute the Marlowe input required for the oracle to report the price.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe            \
                        --choice-name "Price in second window" \
                        --choice-party "Role=$ORACLE_ROLE"     \
                        --choice-number "$SECOND_PRICE"        \
                        --invalid-before "$((NOW-18*MINUTE))"  \
                        --invalid-hereafter "$((NOW+3*HOUR))"  \
                        --out-file tx-5.marlowe                \
                        --print-stats
```

```console
Datum size: 127
Payment 1
  Acccount: "FB"
  Payee: Account "TM"
  Ada: 0.500000
Payment 2
  Acccount: "CM"
  Payee: Party "CM"
  Ada: 2.000000
Payment 3
  Acccount: "FB"
  Payee: Party "FB"
  Ada: 6.500000
Payment 4
  Acccount: "TM"
  Payee: Party "TM"
  Ada: 8.500000
```

Now the oracle Christopher Marlowe submits the transaction containing their price report:

```
TX_5=$(
marlowe-cli run execute "${MAGIC[@]}"                                           \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                        --marlowe-in-file tx-4.marlowe                          \
                        --tx-in-marlowe "$TX_4"#1                               \
                        --tx-in-collateral "$TX_4"#0                            \
                        --tx-in "$TX_4"#0                                       \
                        --tx-in "$TX_4"#2                                       \
                        --required-signer "$ORACLE_PAYMENT_SKEY"                \
                        --marlowe-out-file tx-5.marlowe                         \
                        --tx-out "$ORACLE_ADDRESS+$MINIMUM_ADA+1 $ORACLE_TOKEN" \
                        --change-address "$ORACLE_ADDRESS"                      \
                        --out-file tx-5.raw                                     \
                        --print-stats                                           \
                        --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                \
)
```

```console
Fee: Lovelace 1584102
Size: 13830 / 32768 = 42%
Execution units:
  Memory: 9777684 / 30000000 = 32%
  Steps: 3278225203 / 10000000000 = 32%
```

The contract received the price report in the transaction `229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d` and settled the payments. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     0        151957429 lovelace + TxOutDatumNone
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     4        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.434d + TxOutDatumNone
```

This settles the difference contract, so payments are made to each participant. Here are the UTxO for payments at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     2        6500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

## Transaction 6. Party Withdraws Their Remaining Margin Deposit.

The party Francis Beaumont submits a transaction to withdraw their margin deposit minus the price difference.

```
TX_6=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                         \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                         --marlowe-file tx-5.marlowe                           \
                         --role-name "$PARTY_ROLE"                             \
                         --tx-in "$TX_2"#0                                     \
                         --tx-in "$TX_2"#2                                     \
                         --tx-in-collateral "$TX_2"#0                          \
                         --required-signer "$PARTY_PAYMENT_SKEY"               \
                         --tx-out "$PARTY_ADDRESS+$MINIMUM_ADA+1 $PARTY_TOKEN" \
                         --change-address "$PARTY_ADDRESS"                     \
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

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f970159785fe4a01a24453183d015b9f01a57ed371a4164fa88f171372c7537f     0        296955447 lovelace + TxOutDatumNone
f970159785fe4a01a24453183d015b9f01a57ed371a4164fa88f171372c7537f     1        6500000 lovelace + TxOutDatumNone
f970159785fe4a01a24453183d015b9f01a57ed371a4164fa88f171372c7537f     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.4642 + TxOutDatumNone
```

## Transaction 7. Counterparty Withdraws Their Margin Deposit and Profit.

The counterparty Thomas Middleton submits a transaction to withdraw their margin deposit plus the price difference.

```
TX_7=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                                       \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                           \
                         --marlowe-file tx-5.marlowe                                         \
                         --role-name "$COUNTERPARTY_ROLE"                                    \
                         --tx-in "$TX_3"#0                                                   \
                         --tx-in "$TX_3"#2                                                   \
                         --tx-in-collateral "$TX_3"#0                                        \
                         --required-signer "$COUNTERPARTY_PAYMENT_SKEY"                      \
                         --tx-out "$COUNTERPARTY_ADDRESS+$MINIMUM_ADA+1 $COUNTERPARTY_TOKEN" \
                         --change-address "$COUNTERPARTY_ADDRESS"                            \
                         --out-file tx-7.raw                                                 \
                         --print-stats                                                       \
                         --submit=600                                                        \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                             \
)
```

```console
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
```

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
229577f7447ab9be2af361074e0977fcadcc4a929e618c0c233d0f903217964d     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e4ececccf15a64d5fc93b0d4946a1118c1ac2864c7d29c34544a95f26e7b7cc4     0        39948579 lovelace + TxOutDatumNone
e4ececccf15a64d5fc93b0d4946a1118c1ac2864c7d29c34544a95f26e7b7cc4     1        8500000 lovelace + TxOutDatumNone
e4ececccf15a64d5fc93b0d4946a1118c1ac2864c7d29c34544a95f26e7b7cc4     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.544d + TxOutDatumNone
```

## Transaction 8. Oracle Withdraws Their Deposit.

The oracle Christopher Marlowe submits a transaction to withdraw their minimum ADA deposit.

```
TX_8=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                           \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-5.marlowe                             \
                         --role-name "$ORACLE_ROLE"                              \
                         --tx-in "$TX_5"#0                                       \
                         --tx-in "$TX_5"#4                                       \
                         --tx-in-collateral "$TX_5"#0                            \
                         --required-signer "$ORACLE_PAYMENT_SKEY"                \
                         --tx-out "$ORACLE_ADDRESS+$MINIMUM_ADA+1 $ORACLE_TOKEN" \
                         --change-address "$ORACLE_ADDRESS"                      \
                         --out-file tx-7.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)
```

```console
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
```

There are no UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f970159785fe4a01a24453183d015b9f01a57ed371a4164fa88f171372c7537f     0        296955447 lovelace + TxOutDatumNone
f970159785fe4a01a24453183d015b9f01a57ed371a4164fa88f171372c7537f     1        6500000 lovelace + TxOutDatumNone
f970159785fe4a01a24453183d015b9f01a57ed371a4164fa88f171372c7537f     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.4642 + TxOutDatumNone
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e4ececccf15a64d5fc93b0d4946a1118c1ac2864c7d29c34544a95f26e7b7cc4     0        39948579 lovelace + TxOutDatumNone
e4ececccf15a64d5fc93b0d4946a1118c1ac2864c7d29c34544a95f26e7b7cc4     1        8500000 lovelace + TxOutDatumNone
e4ececccf15a64d5fc93b0d4946a1118c1ac2864c7d29c34544a95f26e7b7cc4     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.544d + TxOutDatumNone
```

Here are the UTxOs at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d4007a0462c63fb8124d7cef992fcdc414cba9cac2703032803c3427f7b1e871     0        151530866 lovelace + TxOutDatumNone
d4007a0462c63fb8124d7cef992fcdc414cba9cac2703032803c3427f7b1e871     1        2000000 lovelace + TxOutDatumNone
d4007a0462c63fb8124d7cef992fcdc414cba9cac2703032803c3427f7b1e871     2        2000000 lovelace + 1 cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8.434d + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple "${MAGIC[@]}"                                     \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                               --tx-in "$TX_6"#0                                 \
                               --tx-in "$TX_6"#2                                 \
                               --tx-out "$ORACLE_ADDRESS+1400000+1 $PARTY_TOKEN" \
                               --required-signer "$PARTY_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                \
                               --out-file /dev/null                              \
                               --submit 600
```

```console
TxId "79ed2ac744371d6735f0aa2ede2b541e32c9751189c1f293d027bd600cef4365"
```

marlowe-cli transaction simple "${MAGIC[@]}"                                            \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                \
                               --tx-in "$TX_7"#0                                        \
                               --tx-in "$TX_7"#2                                        \
                               --tx-out "$ORACLE_ADDRESS+1400000+1 $COUNTERPARTY_TOKEN" \
                               --required-signer "$COUNTERPARTY_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                       \
                               --out-file /dev/null                                     \
                               --submit 600
```

```console
TxId "0156d4070763a8787195550e96c6255ca8e2799ffa99b2d386e8b1fef6533d17"
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                      --required-signer "$ORACLE_PAYMENT_SKEY"          \
                      --change-address  "$ORACLE_ADDRESS"               \
                      --count -1                                        \
                      --expires "$MINT_EXPIRES"                         \
                      --out-file /dev/null                              \
                      --submit=600                                      \
                      "$ORACLE_ROLE" "$PARTY_ROLE" "$COUNTERPARTY_ROLE"
```

```console
PolicyID "cf53c1321095b2172d44d30ec173db211c3ce9d3ddcf583212b785b8"
