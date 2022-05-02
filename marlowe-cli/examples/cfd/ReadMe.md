# Test of a Contract for Differences

In this example execution of a contract for differences, the party and counterparty each make a deposit to cover margin, an oracle reports prices, and the price change is settled among the parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.
See below for how to set `MAGIC` to select the network.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the oracle and party roles, or they will be created automatically: to do this, set the environment variables `PARTY_PREFIX`, `COUNTERPARTY_PREFIX`, and `ORACLE_PREFIX` where they appear below.

## Preliminaries

### Select Network

```
if [[ -z "$MAGIC" ]]
then
  MAGIC=(--testnet-magic 1567)
fi
SLOT_LENGTH=$(marlowe-cli util slotting "${MAGIC[@]}" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotLength)
SLOT_OFFSET=$(marlowe-cli util slotting "${MAGIC[@]}" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotZeroTime)
```

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"
```

The tip is at slot 31610. The current POSIX time implies that the tip of the blockchain should be slightly before slot 31612. Tests may fail if this is not the case.

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
TxId "8177286c71971212bd28237ed63b6376501b07dfdf0f88af7996e438c1bb8024"
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
TxId "308279728cdfc276e2282eee40dc6c21848a9cf0a93b0a42b615639752852f37"
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
TxId "7a0ca1029e3433f9ac1ac6b13a590ee284d03f298498bf889e3310ccd0783250"
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
TxId "994b776f4d62ab66d17cfbe943cc2859250504b991330c12527bb40f61548525"
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
TxId "4b3bc4633f0b24b760e0c5f504e64ae16aa87aa0e62361251730bfb199dfcee4"
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
43101c1ee613d7e30ddcf0e60c429df9a779290e33ec366dee6884af63c93ae2     0        93295228 lovelace + TxOutDatumNone
43101c1ee613d7e30ddcf0e60c429df9a779290e33ec366dee6884af63c93ae2     1        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.434d + TxOutDatumNone
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

Christopher Marlowe will spend the UTxOs `43101c1ee613d7e30ddcf0e60c429df9a779290e33ec366dee6884af63c93ae2#0` and `43101c1ee613d7e30ddcf0e60c429df9a779290e33ec366dee6884af63c93ae2#1`.

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
dbf3f263b7460e16a5eab06a58f32219d9ed683603446a0559f2145046047167     0        305818967 lovelace + TxOutDatumNone
dbf3f263b7460e16a5eab06a58f32219d9ed683603446a0559f2145046047167     1        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.4642 + TxOutDatumNone
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

Francis Beaumont will spend the UTxOs `dbf3f263b7460e16a5eab06a58f32219d9ed683603446a0559f2145046047167#0` and `dbf3f263b7460e16a5eab06a58f32219d9ed683603446a0559f2145046047167#1`.

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
dbff628765d4611cf7bb9d3ca8818c04a744ab56ad98bfcf6ae9cabc2ff1b108     0        49824995 lovelace + TxOutDatumNone
dbff628765d4611cf7bb9d3ca8818c04a744ab56ad98bfcf6ae9cabc2ff1b108     1        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.544d + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `dbff628765d4611cf7bb9d3ca8818c04a744ab56ad98bfcf6ae9cabc2ff1b108#0` and `dbff628765d4611cf7bb9d3ca8818c04a744ab56ad98bfcf6ae9cabc2ff1b108#1`.

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

* The current slot is 31610.
* The party Francis Beaumont must pay the margin deposit of 7000000 lovelace before Fri, 15 Apr 2022 12:07:21 +0000.
* The counterparty Thomas Middleton must pay the margin deposit of 8000000 lovelace before Fri, 15 Apr 2022 18:07:21 +0000.
* The oracle Christopher Marlowe report the first price of 1000000 lovelace between Thu, 14 Apr 2022 23:47:21 +0000 and Sat, 16 Apr 2022 00:07:21 +0000.
* The oracle Christopher Marlowe report the second price of 1500000 lovelace between Thu, 14 Apr 2022 23:48:21 +0000 and Sun, 17 Apr 2022 00:07:21 +0000.

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
marlowe-cli run initialize "${MAGIC[@]}"                             \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --roles-currency "$ROLE_CURRENCY"         \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --print-stats
```

```console
Validator size: 12633
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24920101, exBudgetMemory = ExMemory 83800}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wqd55ncuuzut5rwkfy62tuakf36yve3cafyd34knfxv3dsgaew3yl`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpk809w7k8fypemv0slvjqjf5evh0u94maw72jj0ccfw4eg2qsmrk`.

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

The contract received the minimum ADA of 2000000 lovelace from the oracle Christopher Marlowe in the transaction `6c0331f34d5be83d8ec519515f61399f09ebfec2fa65202cbbe35ab84362f453`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6c0331f34d5be83d8ec519515f61399f09ebfec2fa65202cbbe35ab84362f453     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "2dead97ee7e0742c384400d5c7dc811d6c8b6f3ce3cf42d6fe4fefc8001ee2ce"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6c0331f34d5be83d8ec519515f61399f09ebfec2fa65202cbbe35ab84362f453     0        91077895 lovelace + TxOutDatumNone
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
Fee: Lovelace 1401727
Size: 14943 / 32768 = 45%
Execution units:
  Memory: 6780648 / 30000000 = 22%
  Steps: 2467991603 / 10000000000 = 24%
```

The contract received the margin deposit of 7000000 lovelace in the transaction `bbff62707790540c54d358b5fef1cca4e43c7be923137364c40facfea5c508c5`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bbff62707790540c54d358b5fef1cca4e43c7be923137364c40facfea5c508c5     1        9000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8dd6992a966723c57308382b304fffc8f874a3a8e12fbf93101665c84d46eadf"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bbff62707790540c54d358b5fef1cca4e43c7be923137364c40facfea5c508c5     0        297417240 lovelace + TxOutDatumNone
bbff62707790540c54d358b5fef1cca4e43c7be923137364c40facfea5c508c5     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.4642 + TxOutDatumNone
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
Fee: Lovelace 1418996
Size: 14863 / 32768 = 45%
Execution units:
  Memory: 7060286 / 30000000 = 23%
  Steps: 2532540076 / 10000000000 = 25%
```

The contract received the margin deposit of 8000000 lovelace in the transaction `10e91f688f9ace5b2db61ec5d5e72167557393a63041c559a67a80993452d2c5`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
10e91f688f9ace5b2db61ec5d5e72167557393a63041c559a67a80993452d2c5     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "2098f5238170c5c63ed1da5dff7d4c7f0017cd9d873c8bc1255b5d12a0875e7e"
```

Here is the UTxO at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
10e91f688f9ace5b2db61ec5d5e72167557393a63041c559a67a80993452d2c5     0        40405999 lovelace + TxOutDatumNone
10e91f688f9ace5b2db61ec5d5e72167557393a63041c559a67a80993452d2c5     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.544d + TxOutDatumNone
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
Fee: Lovelace 1399010
Size: 14782 / 32768 = 45%
Execution units:
  Memory: 6878664 / 30000000 = 22%
  Steps: 2450112260 / 10000000000 = 24%
```

The contract received the price report in the transaction `a6c262f383d9478d4f7660348563e73314f28fb1cf869a9353cbabe59349c16d`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a6c262f383d9478d4f7660348563e73314f28fb1cf869a9353cbabe59349c16d     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "83b43993d815aa8a84aefeccb6f46f4d6139a9303eb03e460930163cdea878bb"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a6c262f383d9478d4f7660348563e73314f28fb1cf869a9353cbabe59349c16d     0        89678885 lovelace + TxOutDatumNone
a6c262f383d9478d4f7660348563e73314f28fb1cf869a9353cbabe59349c16d     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.434d + TxOutDatumNone
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
Fee: Lovelace 1595213
Size: 14080 / 32768 = 42%
Execution units:
  Memory: 9779084 / 30000000 = 32%
  Steps: 3278642025 / 10000000000 = 32%
```

The contract received the price report in the transaction `f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d` and settled the payments. There is no UTxO at the contract address:

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
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     0        88083672 lovelace + TxOutDatumNone
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     4        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.434d + TxOutDatumNone
```

This settles the difference contract, so payments are made to each participant. Here are the UTxO for payments at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     2        6500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
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
Fee: Lovelace 422045
Size: 2881 / 32768 = 8%
Execution units:
  Memory: 1407010 / 30000000 = 4%
  Steps: 541567360 / 10000000000 = 5%
```

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
173edf9f6a19f39dde9c6d53fc8f971d3de4ca1b26a62c276ddc24802ea7a516     0        296995195 lovelace + TxOutDatumNone
173edf9f6a19f39dde9c6d53fc8f971d3de4ca1b26a62c276ddc24802ea7a516     1        6500000 lovelace + TxOutDatumNone
173edf9f6a19f39dde9c6d53fc8f971d3de4ca1b26a62c276ddc24802ea7a516     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.4642 + TxOutDatumNone
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
Fee: Lovelace 422045
Size: 2881 / 32768 = 8%
Execution units:
  Memory: 1407010 / 30000000 = 4%
  Steps: 541567360 / 10000000000 = 5%
```

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f45fbdf7a0af60e2c25133b4854800ed3175e09f9574243299e634df53489d5d     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
15613d9f31733f08439b5d2ef16d3fedc854ae8a3584f7d79b08b140cfc8c875     0        39983954 lovelace + TxOutDatumNone
15613d9f31733f08439b5d2ef16d3fedc854ae8a3584f7d79b08b140cfc8c875     1        8500000 lovelace + TxOutDatumNone
15613d9f31733f08439b5d2ef16d3fedc854ae8a3584f7d79b08b140cfc8c875     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.544d + TxOutDatumNone
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
Fee: Lovelace 426387
Size: 2881 / 32768 = 8%
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
173edf9f6a19f39dde9c6d53fc8f971d3de4ca1b26a62c276ddc24802ea7a516     0        296995195 lovelace + TxOutDatumNone
173edf9f6a19f39dde9c6d53fc8f971d3de4ca1b26a62c276ddc24802ea7a516     1        6500000 lovelace + TxOutDatumNone
173edf9f6a19f39dde9c6d53fc8f971d3de4ca1b26a62c276ddc24802ea7a516     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.4642 + TxOutDatumNone
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
15613d9f31733f08439b5d2ef16d3fedc854ae8a3584f7d79b08b140cfc8c875     0        39983954 lovelace + TxOutDatumNone
15613d9f31733f08439b5d2ef16d3fedc854ae8a3584f7d79b08b140cfc8c875     1        8500000 lovelace + TxOutDatumNone
15613d9f31733f08439b5d2ef16d3fedc854ae8a3584f7d79b08b140cfc8c875     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.544d + TxOutDatumNone
```

Here are the UTxOs at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
00c74db37a7268d24a455d6a254a13f97ae240c704762cb6af547e06b88bfcea     0        87657285 lovelace + TxOutDatumNone
00c74db37a7268d24a455d6a254a13f97ae240c704762cb6af547e06b88bfcea     1        2000000 lovelace + TxOutDatumNone
00c74db37a7268d24a455d6a254a13f97ae240c704762cb6af547e06b88bfcea     2        2000000 lovelace + 1 2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b.434d + TxOutDatumNone
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
TxId "12843949161ed803f57c9dad402ac2262af6b081a0d6484b834f8a790ffab53f"
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
TxId "37e7becd93049ccf4b8f0faddbb2da642b89e0320aa7dd47e2741e2c3e85dca1"
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
PolicyID "2c0447a0b07aee4708fa09845d3080ed78a4d414c49bbc03405dbf4b"
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
TxId "035dd3d107f5c28eb06cba8229ddb1d6fd2adedb2f3bf2be5ab2cbc02000d2dc"
