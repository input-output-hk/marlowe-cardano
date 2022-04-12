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

The tip is at slot 4870526. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4870528. Tests may fail if this is not the case.

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
TxId "103df793a34f53b6fd6a69843b2a12121b5655b36faa51ad6a78e7b39061bc9c"
```

#### The Counterparty

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
TxId "327d229c8cf03c60f86b7ee8e69233a33c21f8a636e5a1ad5a06a4e99a8d62d0"
```

#### The Oracle

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
TxId "70257de8baccdd34bc1b8f3c4144807c7f5944280d04cfe0ba692fa5ee702a0b"
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
TxId "69b13f7d769a5ca6d36cf78047a3909d155986292fa2ef4a0a53565ba03db1e2"
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
TxId "a8bd4a471653c1200ae87e48e353b4c00f604faf6d0274d8b403d69480ceac30"
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
4b041611ab264e3470ef9ba0d52c88b3ae41a6d392ded2c0a4c28b4cb9c877ef     0        93295228 lovelace + TxOutDatumNone
4b041611ab264e3470ef9ba0d52c88b3ae41a6d392ded2c0a4c28b4cb9c877ef     1        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.434d + TxOutDatumNone
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

Christopher Marlowe will spend the UTxOs `4b041611ab264e3470ef9ba0d52c88b3ae41a6d392ded2c0a4c28b4cb9c877ef#0` and `4b041611ab264e3470ef9ba0d52c88b3ae41a6d392ded2c0a4c28b4cb9c877ef#1`.

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
f3891a6051ad41e380f819f10df1ffd159baa2d4d8a2b125ea7ff56f7186c8c4     0        49824995 lovelace + TxOutDatumNone
f3891a6051ad41e380f819f10df1ffd159baa2d4d8a2b125ea7ff56f7186c8c4     1        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.4642 + TxOutDatumNone
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

Francis Beaumont will spend the UTxOs `f3891a6051ad41e380f819f10df1ffd159baa2d4d8a2b125ea7ff56f7186c8c4#0` and `f3891a6051ad41e380f819f10df1ffd159baa2d4d8a2b125ea7ff56f7186c8c4#1`.

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
7628ec00921791a98cbceca9d82c1622c2237e5b35bea434ca59da9939fede56     0        49824995 lovelace + TxOutDatumNone
7628ec00921791a98cbceca9d82c1622c2237e5b35bea434ca59da9939fede56     1        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.544d + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `7628ec00921791a98cbceca9d82c1622c2237e5b35bea434ca59da9939fede56#0` and `7628ec00921791a98cbceca9d82c1622c2237e5b35bea434ca59da9939fede56#1`.

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

* The current slot is 4870526.
* The party Francis Beaumont must pay the margin deposit of 7000000 lovelace before Wed, 13 Apr 2022 09:49:26 +0000.
* The counterparty Thomas Middleton must pay the margin deposit of 8000000 lovelace before Wed, 13 Apr 2022 15:49:26 +0000.
* The oracle Christopher Marlowe report the first price of 1000000 lovelace between Tue, 12 Apr 2022 21:29:26 +0000 and Wed, 13 Apr 2022 21:49:26 +0000.
* The oracle Christopher Marlowe report the second price of 1500000 lovelace between Tue, 12 Apr 2022 21:30:26 +0000 and Thu, 14 Apr 2022 21:49:26 +0000.

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

The Marlowe contract resides at address `addr_test1wz8hxrpserkykxhz767tg5x78v8zayl2rd9q95h0lsfu4hqfeh799`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wp8c8jvd0fh99dh5ey9zdgxn8dcn3u5wldgnzvn7xjcxwpselwscq`.

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

The contract received the minimum ADA of 2000000 lovelace from the oracle Christopher Marlowe in the transaction `d400cb82d87aaa0d57de077078345752648b81222ba0b67f2efd6fe9d0d28bbd`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d400cb82d87aaa0d57de077078345752648b81222ba0b67f2efd6fe9d0d28bbd     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9172fed0456babcad1c713481c39efb766cf567c35f5ead2256eb900b3c41185"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d400cb82d87aaa0d57de077078345752648b81222ba0b67f2efd6fe9d0d28bbd     0        91077895 lovelace + TxOutDatumNone
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

The contract received the margin deposit of 7000000 lovelace in the transaction `d18491d49bc15b176ed6096efc046987bc44da81dd85cdcf8d271b081f0fd241`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d18491d49bc15b176ed6096efc046987bc44da81dd85cdcf8d271b081f0fd241     1        9000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "d669355b92cc01e75338c4ca497336160a2220a29eca9ac341fdb143f7706daf"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d18491d49bc15b176ed6096efc046987bc44da81dd85cdcf8d271b081f0fd241     0        41434458 lovelace + TxOutDatumNone
d18491d49bc15b176ed6096efc046987bc44da81dd85cdcf8d271b081f0fd241     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.4642 + TxOutDatumNone
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

The contract received the margin deposit of 8000000 lovelace in the transaction `9210751e0124a582fedfdae0e41bc9f93414413afb0ccdd1c1dbc7585e682529`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9210751e0124a582fedfdae0e41bc9f93414413afb0ccdd1c1dbc7585e682529     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "99f931439b36db35dd1d67fb9681f1ec4dd275c0e3a10ac159a1ee1a1b92ff4f"
```

Here is the UTxO at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9210751e0124a582fedfdae0e41bc9f93414413afb0ccdd1c1dbc7585e682529     0        40421562 lovelace + TxOutDatumNone
9210751e0124a582fedfdae0e41bc9f93414413afb0ccdd1c1dbc7585e682529     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.544d + TxOutDatumNone
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
Fee: Lovelace 1381983
Size: 14532 / 32768 = 44%
Execution units:
  Memory: 6802860 / 30000000 = 22%
  Steps: 2427197116 / 10000000000 = 24%
```

The contract received the price report in the transaction `64b95f48561e184ac17c815241b2f6436267a0c79316a8949a4e7f18374b6372`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
64b95f48561e184ac17c815241b2f6436267a0c79316a8949a4e7f18374b6372     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "21ecf3fd057b43416ff7442b8b1d4d311906060c83ee3216b9fe184695aea2e1"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
64b95f48561e184ac17c815241b2f6436267a0c79316a8949a4e7f18374b6372     0        89695912 lovelace + TxOutDatumNone
64b95f48561e184ac17c815241b2f6436267a0c79316a8949a4e7f18374b6372     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.434d + TxOutDatumNone
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

The contract received the price report in the transaction `2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18` and settled the payments. There is no UTxO at the contract address:

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
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     0        88111810 lovelace + TxOutDatumNone
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     4        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.434d + TxOutDatumNone
```

This settles the difference contract, so payments are made to each participant. Here are the UTxO for payments at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     2        6500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
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
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b545ed9baa1d7b7defdd75be99b2dc9800fd1a93018edb3d3b9b9becf8427adf     0        41007895 lovelace + TxOutDatumNone
b545ed9baa1d7b7defdd75be99b2dc9800fd1a93018edb3d3b9b9becf8427adf     1        6500000 lovelace + TxOutDatumNone
b545ed9baa1d7b7defdd75be99b2dc9800fd1a93018edb3d3b9b9becf8427adf     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.4642 + TxOutDatumNone
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
2e8abe5f9faf98c4fc4051884e47ce1b7d81ac78af9234a1dbf24d467f2d9c18     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f14379cfc1af76f6ac89fadc035614ccabe01ed0731fcfb873aaa5f16763926f     0        39994999 lovelace + TxOutDatumNone
f14379cfc1af76f6ac89fadc035614ccabe01ed0731fcfb873aaa5f16763926f     1        8500000 lovelace + TxOutDatumNone
f14379cfc1af76f6ac89fadc035614ccabe01ed0731fcfb873aaa5f16763926f     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.544d + TxOutDatumNone
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
b545ed9baa1d7b7defdd75be99b2dc9800fd1a93018edb3d3b9b9becf8427adf     0        41007895 lovelace + TxOutDatumNone
b545ed9baa1d7b7defdd75be99b2dc9800fd1a93018edb3d3b9b9becf8427adf     1        6500000 lovelace + TxOutDatumNone
b545ed9baa1d7b7defdd75be99b2dc9800fd1a93018edb3d3b9b9becf8427adf     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.4642 + TxOutDatumNone
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f14379cfc1af76f6ac89fadc035614ccabe01ed0731fcfb873aaa5f16763926f     0        39994999 lovelace + TxOutDatumNone
f14379cfc1af76f6ac89fadc035614ccabe01ed0731fcfb873aaa5f16763926f     1        8500000 lovelace + TxOutDatumNone
f14379cfc1af76f6ac89fadc035614ccabe01ed0731fcfb873aaa5f16763926f     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.544d + TxOutDatumNone
```

Here are the UTxOs at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
20c56adf9b8495db92126df5e213a6ca72436173e1f064f136df4a42ad6fdb76     0        87685247 lovelace + TxOutDatumNone
20c56adf9b8495db92126df5e213a6ca72436173e1f064f136df4a42ad6fdb76     1        2000000 lovelace + TxOutDatumNone
20c56adf9b8495db92126df5e213a6ca72436173e1f064f136df4a42ad6fdb76     2        2000000 lovelace + 1 8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37.434d + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple "${MAGIC[@]}"                                     \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                               --tx-in "$TX_6"#0                                 \
                               --tx-in "$TX_6"#1                                 \
                               --tx-in "$TX_6"#2                                 \
                               --tx-out "$ORACLE_ADDRESS+1400000+1 $PARTY_TOKEN" \
                               --required-signer "$PARTY_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                \
                               --out-file /dev/null                              \
                               --submit 600
```

```console
TxId "bc392f2566be897e09df5e2c317bfea0610b60b84acfc0388069ff253888314d"
```

marlowe-cli transaction simple "${MAGIC[@]}"                                            \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                \
                               --tx-in "$TX_7"#0                                        \
                               --tx-in "$TX_7"#1                                        \
                               --tx-in "$TX_7"#2                                        \
                               --tx-out "$ORACLE_ADDRESS+1400000+1 $COUNTERPARTY_TOKEN" \
                               --required-signer "$COUNTERPARTY_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                       \
                               --out-file /dev/null                                     \
                               --submit 600
```

```console
TxId "7ab57a84f695198a6493e33233690851bc504d8af8cde9bddfab58c8460d2d81"
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
PolicyID "8f841510b60b3efb489318b8614b64d2d6e2d618e354e0576c5e5f37"
TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$ORACLE_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$ORACLE_PAYMENT_SKEY"  \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600
```

```console
TxId "dfd457f505cf2f09a0a349b01578164df6334324db07d41599984802ea3e129d"
