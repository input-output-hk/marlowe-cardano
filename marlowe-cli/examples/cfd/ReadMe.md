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

```
: "${FAUCET_ADDRESS:?FAUCET_ADDRESS not set}"
: "${FAUCET_SKEY_FILE:?FAUCET_SKEY_FILE not set}"
```

### Select Network

```
: "${MAGIC:=2}"
```

MAGIC=2

```
SLOT_LENGTH=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotLength)
SLOT_OFFSET=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotZeroTime)
```

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip --testnet-magic "$MAGIC" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"
```

The tip is at slot 3418024. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3418026. Tests may fail if this is not the case.

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
PARTY_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_PAYMENT_VKEY")
```

Fund the party's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"          \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$PARTY_ADDRESS"
```

```console
TxId "71a2f9fdfcdbd8274f180997cad77fec41cbab6d778f341a2d860deeaffb4314"
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
COUNTERPARTY_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY")
```

Fund the counterparty's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"          \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$COUNTERPARTY_ADDRESS"
```

```console
TxId "c2c9fb544a1ac8679dfb7b4da4b6204c70db94404a14ef9b9071b0754afa6732"
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
ORACLE_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$ORACLE_PAYMENT_VKEY")
```

Fund the oracle's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"          \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$ORACLE_ADDRESS"
```

```console
TxId "1989a7ae15277e65d1990e909b1aa80b7c21da989e6a35a25605e00273ac0326"
```

### Role Tokens

The oracle mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                  \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                      --required-signer "$ORACLE_PAYMENT_SKEY"          \
                      --change-address  "$ORACLE_ADDRESS"               \
                      --expires "$MINT_EXPIRES"                         \
                      --out-file /dev/null                              \
                      --submit=600                                      \
                      "$ORACLE_ROLE" "$PARTY_ROLE" "$COUNTERPARTY_ROLE" \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                       \
)
ORACLE_TOKEN="$ROLE_CURRENCY.$ORACLE_ROLE"
PARTY_TOKEN="$ROLE_CURRENCY.$PARTY_ROLE"
COUNTERPARTY_TOKEN="$ROLE_CURRENCY.$COUNTERPARTY_ROLE"
```

Find the transaction output with the party's role token.

```
TX_MINT_PARTY=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --asset-only "$PARTY_TOKEN"                     \
                        "$ORACLE_ADDRESS"                               \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
```

Send the party their role token.

```
marlowe-cli transaction simple --testnet-magic "$MAGIC"                 \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"        \
                               --tx-in "$TX_MINT_PARTY"                         \
                               --tx-out "$PARTY_ADDRESS+2000000+1 $PARTY_TOKEN" \
                               --required-signer "$ORACLE_PAYMENT_SKEY"         \
                               --change-address "$ORACLE_ADDRESS"               \
                               --out-file /dev/null                             \
                               --submit 600
```

```console
TxId "e72999661409b888c27755cf9fe1f64c94d0031e95028d04188c664b2411a716"
```

Find the transaction output with the counterparty's role token.

```
TX_MINT_COUNTERPARTY=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --asset-only "$COUNTERPARTY_TOKEN"              \
                        "$ORACLE_ADDRESS"                               \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
```

Send the counterparty their role token.

```
marlowe-cli transaction simple --testnet-magic "$MAGIC"                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                      \
                               --tx-in "$TX_MINT_COUNTERPARTY"                                \
                               --tx-out "$COUNTERPARTY_ADDRESS+2000000+1 $COUNTERPARTY_TOKEN" \
                               --required-signer "$ORACLE_PAYMENT_SKEY"                       \
                               --change-address "$ORACLE_ADDRESS"                             \
                               --out-file /dev/null                                           \
                               --submit 600
```

```console
TxId "278ff2d0fdf8bfc6b6298bb231bfdcb32de6b6e2d874f76e6d09b677e69295d7"
```

### Available UTxOs

The oracle Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz` and role token named `CM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"          \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$ORACLE_PAYMENT_SKEY"  \
                       --change-address "$ORACLE_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
086eeb59a3fb6cf999b129f07d68004f68ac4f5e0e6efa97d0817aeb13a046ad     0        244596968 lovelace + TxOutDatumNone
086eeb59a3fb6cf999b129f07d68004f68ac4f5e0e6efa97d0817aeb13a046ad     1        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.434d + TxOutDatumNone
```

We select the UTxO with the oracle Christopher Marlowe's role token.

```
TX_0_ORACLE_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --lovelace-only 20000000                        \
                        "$ORACLE_ADDRESS"                               \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
TX_0_ORACLE_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --asset-only "$ORACLE_TOKEN"                    \
                        "$ORACLE_ADDRESS"                               \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
```

Christopher Marlowe will spend the UTxOs `086eeb59a3fb6cf999b129f07d68004f68ac4f5e0e6efa97d0817aeb13a046ad#0` and `086eeb59a3fb6cf999b129f07d68004f68ac4f5e0e6efa97d0817aeb13a046ad#1`.

The party Francis Beaumont has the address `addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q` and role token named `FB`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"          \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_PAYMENT_SKEY"   \
                       --change-address "$PARTY_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ba6a7b563701ed5b44bd1daddbff9b86412a6b439bee78ec60e4e1d74cac457c     0        193127175 lovelace + TxOutDatumNone
ba6a7b563701ed5b44bd1daddbff9b86412a6b439bee78ec60e4e1d74cac457c     1        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.4642 + TxOutDatumNone
```

We select the UTxO with the lender Francis Beaumont's role token.

```
TX_0_PARTY_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --lovelace-only 20000000                        \
                        "$PARTY_ADDRESS"                                \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
TX_0_PARTY_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --asset-only "$PARTY_TOKEN"                     \
                        "$PARTY_ADDRESS"                                \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
```

Francis Beaumont will spend the UTxOs `ba6a7b563701ed5b44bd1daddbff9b86412a6b439bee78ec60e4e1d74cac457c#0` and `ba6a7b563701ed5b44bd1daddbff9b86412a6b439bee78ec60e4e1d74cac457c#1`.

The counterparty Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `TM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"               \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                       --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                       --change-address "$COUNTERPARTY_ADDRESS"       \
                       --out-file /dev/null                           \
                       --submit=600                                   \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
64d1a7d2456be7ee6a2680713811a50552e482582d9440b7caf85c861acb5f57     0        49824907 lovelace + TxOutDatumNone
64d1a7d2456be7ee6a2680713811a50552e482582d9440b7caf85c861acb5f57     1        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.544d + TxOutDatumNone
```

We select the UTxO with the lender Thomas Middleton's role token.

```
TX_0_COUNTERPARTY_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --lovelace-only 20000000                        \
                        "$COUNTERPARTY_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
TX_0_COUNTERPARTY_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"       \
                        --asset-only "$COUNTERPARTY_TOKEN"              \
                        "$COUNTERPARTY_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'  \
)
```

Thomas Middleton will spend the UTxOs `64d1a7d2456be7ee6a2680713811a50552e482582d9440b7caf85c861acb5f57#0` and `64d1a7d2456be7ee6a2680713811a50552e482582d9440b7caf85c861acb5f57#1`.

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

* The current slot is 3418024.
* The party Francis Beaumont must pay the margin deposit of 7000000 lovelace before Sun, 18 Sep 2022 01:27:04 +0000.
* The counterparty Thomas Middleton must pay the margin deposit of 8000000 lovelace before Sun, 18 Sep 2022 07:27:04 +0000.
* The oracle Christopher Marlowe report the first price of 1000000 lovelace between Sat, 17 Sep 2022 13:07:04 +0000 and Sun, 18 Sep 2022 13:27:04 +0000.
* The oracle Christopher Marlowe report the second price of 1500000 lovelace between Sat, 17 Sep 2022 13:08:04 +0000 and Mon, 19 Sep 2022 13:27:04 +0000.

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
marlowe-cli run initialize --testnet-magic "$MAGIC"          \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --roles-currency "$ROLE_CURRENCY"         \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --print-stats
```

```console
Validator size: 12668
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 18653100, exBudgetMemory = ExMemory 81200}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.tx.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wrv0vwr4megau50ujjwsktvajmsu6dzza2rnalufd3husaqs9v6rv`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.tx.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpkmnxz4aylglk57j9mf90r5dj0kmde7n6frfgatam4fw8qyrah58`.

The oracle Christopher Marlowe submits the transaction along with the minimum ADA 2000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"          \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_ORACLE_ADA"                \
                        --change-address "$ORACLE_ADDRESS"        \
                        --required-signer "$ORACLE_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-1.marlowe           \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                     \
)
```

```console
Fee: Lovelace 219137
Size: 1204 / 16384 = 7%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 2000000 lovelace from the oracle Christopher Marlowe in the transaction `adf62f449538dae97e2ea33526fca12d9ade7766e1b567eda9fad683055537a5`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
adf62f449538dae97e2ea33526fca12d9ade7766e1b567eda9fad683055537a5     1        2000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "b0761b910d294e17c853ccfc7a485daff91d0ee1b77f7407d28204ccdcee3226"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
adf62f449538dae97e2ea33526fca12d9ade7766e1b567eda9fad683055537a5     0        242377831 lovelace + TxOutDatumNone
```

## Transaction 2. Party Deposits Margin Funds into Their Account.

First we compute the Marlowe input required to make the margin deposit by the party.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe   \
                        --deposit-account "$PARTY_ROLE"       \
                        --deposit-party "$PARTY_ROLE"         \
                        --deposit-amount "$PARTY_MARGIN"      \
                        --invalid-before "$((NOW-18*MINUTE))" \
                        --invalid-hereafter "$((NOW+3*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 937
```

Now the party Francis Beaumont submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                      \
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
| sed -e 's/^TxId "\(.*\)"$/\1/'                                 \
)
```

```console
Fee: Lovelace 1378257
Size: 15059 / 16384 = 91%
Execution units:
  Memory: 6974606 / 14000000 = 49%
  Steps: 1915239970 / 10000000000 = 19%
```

The contract received the margin deposit of 7000000 lovelace in the transaction `b9227a36f8fc67bd279b3e57a43cc143944715d49de0cde0ddf53da3b458eb3a`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b9227a36f8fc67bd279b3e57a43cc143944715d49de0cde0ddf53da3b458eb3a     1        9000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "4bbc9f5eb0a5699729618fb5395b83b3aa8abb1b116a6110ae13c032561012c8"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b9227a36f8fc67bd279b3e57a43cc143944715d49de0cde0ddf53da3b458eb3a     0        184748918 lovelace + TxOutDatumNone
b9227a36f8fc67bd279b3e57a43cc143944715d49de0cde0ddf53da3b458eb3a     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.4642 + TxOutDatumNone
```

## Transaction 3. Counterparty Deposits Margin Funds into Their Account.

First we compute the Marlowe input required to make the margin deposit by the counterparty.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe         \
                        --deposit-account "$COUNTERPARTY_ROLE"      \
                        --deposit-party "$COUNTERPARTY_ROLE"        \
                        --deposit-amount "$COUNTERPARTY_MARGIN"     \
                        --invalid-before "$((NOW-18*MINUTE))"       \
                        --invalid-hereafter "$((NOW+3*HOUR))"       \
                        --out-file tx-3.marlowe                     \
                        --print-stats
```

```console
Datum size: 890
```

Now the counterparty Thomas Middleton submits the transaction along with their deposit:

```
TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                    \
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
| sed -e 's/^TxId "\(.*\)"$/\1/'                                               \
)
```

```console
Fee: Lovelace 1390994
Size: 14979 / 16384 = 91%
Execution units:
  Memory: 7202344 / 14000000 = 51%
  Steps: 1958468831 / 10000000000 = 19%
```

The contract received the margin deposit of 8000000 lovelace in the transaction `36cc5526428184f59652f0334c0f43d6f96fc07032021830d85f5339aba91cbc`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
36cc5526428184f59652f0334c0f43d6f96fc07032021830d85f5339aba91cbc     1        17000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "cffebf6e5a23c6999dff782c26ae6196e10bc90653cf5233942c7c99ecff7f6a"
```

Here is the UTxO at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
36cc5526428184f59652f0334c0f43d6f96fc07032021830d85f5339aba91cbc     0        40433913 lovelace + TxOutDatumNone
36cc5526428184f59652f0334c0f43d6f96fc07032021830d85f5339aba91cbc     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.544d + TxOutDatumNone
```

## Transaction 4. Oracle Reports the First Price.

First we compute the Marlowe input required for the oracle to report the price.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe   \
                        --choice-name "Price in first window" \
                        --choice-party "$ORACLE_ROLE"         \
                        --choice-number "$FIRST_PRICE"        \
                        --invalid-before "$((NOW-18*MINUTE))" \
                        --invalid-hereafter "$((NOW+3*HOUR))" \
                        --out-file tx-4.marlowe               \
                        --print-stats
```

```console
Datum size: 843
```

Now the oracle Christopher Marlowe submits the transaction containing their price report:

```
TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                        \
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
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)
```

```console
Fee: Lovelace 1370508
Size: 14898 / 16384 = 90%
Execution units:
  Memory: 6993318 / 14000000 = 49%
  Steps: 1891038116 / 10000000000 = 18%
```

The contract received the price report in the transaction `7a7ba6b0c19003394243943b20886fd1937d30d8a1a167d8f3b662a9b239cd8b`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7a7ba6b0c19003394243943b20886fd1937d30d8a1a167d8f3b662a9b239cd8b     1        17000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "ba6ccd4de304239a7f064e3fa3c6b087f1a828c6eab2f9500a894c9eb97fa138"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7a7ba6b0c19003394243943b20886fd1937d30d8a1a167d8f3b662a9b239cd8b     0        241007323 lovelace + TxOutDatumNone
7a7ba6b0c19003394243943b20886fd1937d30d8a1a167d8f3b662a9b239cd8b     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.434d + TxOutDatumNone
```

## Transaction 5. Oracle Reports the Second Price.

First we compute the Marlowe input required for the oracle to report the price.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe    \
                        --choice-name "Price in second window" \
                        --choice-party "$ORACLE_ROLE"          \
                        --choice-number "$SECOND_PRICE"        \
                        --invalid-before "$((NOW-18*MINUTE))"  \
                        --invalid-hereafter "$((NOW+3*HOUR))"  \
                        --out-file tx-5.marlowe                \
                        --print-stats
```

```console
Datum size: 163
Payment 1
  Acccount: "FB"
  Payee: Account "TM"
  Ada: Lovelace {getLovelace = 500000}
Payment 2
  Acccount: "CM"
  Payee: Party "CM"
  Ada: Lovelace {getLovelace = 2000000}
Payment 3
  Acccount: "FB"
  Payee: Party "FB"
  Ada: Lovelace {getLovelace = 6500000}
Payment 4
  Acccount: "TM"
  Payee: Party "TM"
  Ada: Lovelace {getLovelace = 8500000}
```

Now the oracle Christopher Marlowe submits the transaction containing their price report:

```
TX_5=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                        \
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
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)
```

```console
Fee: Lovelace 1698744
Size: 14274 / 16384 = 87%
Execution units:
  Memory: 11722834 / 14000000 = 83%
  Steps: 3039431139 / 10000000000 = 30%
```

The contract received the price report in the transaction `b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089` and settled the payments. There is no UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     0        239308579 lovelace + TxOutDatumNone
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     4        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.434d + TxOutDatumNone
```

This settles the difference contract, so payments are made to each participant. Here are the UTxO for payments at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     1        2000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "0e0294a0ceca79b3d3c996d22f6c628e1b18f034f8b25d8976e20fd924cceb7d"
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     2        6500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "2a01957d3e857a4e7c5c56d7eaac6e14b49eef2795b9e978ad5e531678d3e1b6"
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     3        8500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "f46074bf415dbf3385d0c02cd12ccd9b43aa085563cd65917d7573e5c5a473af"
```

## Transaction 6. Party Withdraws Their Remaining Margin Deposit.

The party Francis Beaumont submits a transaction to withdraw their margin deposit minus the price difference.

```
TX_6=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                      \
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
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 436285
Size: 3083 / 16384 = 18%
Execution units:
  Memory: 1606962 / 14000000 = 11%
  Steps: 454555011 / 10000000000 = 4%
```

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     1        2000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "0e0294a0ceca79b3d3c996d22f6c628e1b18f034f8b25d8976e20fd924cceb7d"
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     3        8500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "f46074bf415dbf3385d0c02cd12ccd9b43aa085563cd65917d7573e5c5a473af"
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d70deca8ab2665e55bbf945590348aea554a37d2e1aaf481b68f8b180f2edeca     0        184312633 lovelace + TxOutDatumNone
d70deca8ab2665e55bbf945590348aea554a37d2e1aaf481b68f8b180f2edeca     1        6500000 lovelace + TxOutDatumNone
d70deca8ab2665e55bbf945590348aea554a37d2e1aaf481b68f8b180f2edeca     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.4642 + TxOutDatumNone
```

## Transaction 7. Counterparty Withdraws Their Margin Deposit and Profit.

The counterparty Thomas Middleton submits a transaction to withdraw their margin deposit plus the price difference.

```
TX_7=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                    \
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
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                \
)
```

```console
Fee: Lovelace 432223
Size: 3083 / 16384 = 18%
Execution units:
  Memory: 1552162 / 14000000 = 11%
  Steps: 442070090 / 10000000000 = 4%
```

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b7eab3fda8898b800a586e877a5c03fce26bae5425988706b7ae51fb9ceaa089     1        2000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "0e0294a0ceca79b3d3c996d22f6c628e1b18f034f8b25d8976e20fd924cceb7d"
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d283f81f24811b4adf6dd5067c9c254367533a1d7a011de4df9423817d01b665     0        40001690 lovelace + TxOutDatumNone
d283f81f24811b4adf6dd5067c9c254367533a1d7a011de4df9423817d01b665     1        8500000 lovelace + TxOutDatumNone
d283f81f24811b4adf6dd5067c9c254367533a1d7a011de4df9423817d01b665     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.544d + TxOutDatumNone
```

## Transaction 8. Oracle Withdraws Their Deposit.

The oracle Christopher Marlowe submits a transaction to withdraw their minimum ADA deposit.

```
TX_8=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                        \
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
| sed -e 's/^TxId "\(.*\)"$/\1/'                                    \
)
```

```console
Fee: Lovelace 436285
Size: 3083 / 16384 = 18%
Execution units:
  Memory: 1606962 / 14000000 = 11%
  Steps: 454555011 / 10000000000 = 4%
```

There are no UTxOs at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d70deca8ab2665e55bbf945590348aea554a37d2e1aaf481b68f8b180f2edeca     0        184312633 lovelace + TxOutDatumNone
d70deca8ab2665e55bbf945590348aea554a37d2e1aaf481b68f8b180f2edeca     1        6500000 lovelace + TxOutDatumNone
d70deca8ab2665e55bbf945590348aea554a37d2e1aaf481b68f8b180f2edeca     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.4642 + TxOutDatumNone
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d283f81f24811b4adf6dd5067c9c254367533a1d7a011de4df9423817d01b665     0        40001690 lovelace + TxOutDatumNone
d283f81f24811b4adf6dd5067c9c254367533a1d7a011de4df9423817d01b665     1        8500000 lovelace + TxOutDatumNone
d283f81f24811b4adf6dd5067c9c254367533a1d7a011de4df9423817d01b665     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.544d + TxOutDatumNone
```

Here are the UTxOs at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1cc930893d1e82f620f3ba9f4a24008450bf795129f6ae9331369b5f985b7d5c     0        238872294 lovelace + TxOutDatumNone
1cc930893d1e82f620f3ba9f4a24008450bf795129f6ae9331369b5f985b7d5c     1        2000000 lovelace + TxOutDatumNone
1cc930893d1e82f620f3ba9f4a24008450bf795129f6ae9331369b5f985b7d5c     2        2000000 lovelace + 1 c0676cf5f97d705149f1aa241b9fceadfbc78eb5f666ba235aec04c2.434d + TxOutDatumNone
```

## Clean Up

```
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
marlowe-cli transaction simple --testnet-magic "$MAGIC"                  \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                               --tx-in "$TX_6"#0                                 \
                               --tx-in "$TX_6"#1                                 \
                               --tx-in "$TX_6"#2                                 \
                               --tx-out "$BURN_ADDRESS+1400000+1 $PARTY_TOKEN" \
                               --required-signer "$PARTY_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                \
                               --out-file /dev/null                              \
                               --submit 600
```

```console
TxId "0a290ab288c59210c51dc40fee7cac77807cc286641e9e4d00d30a4fb467a336"
```

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
marlowe-cli transaction simple --testnet-magic "$MAGIC"                         \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                \
                               --tx-in "$TX_7"#0                                        \
                               --tx-in "$TX_7"#1                                        \
                               --tx-in "$TX_7"#2                                        \
                               --tx-out "$BURN_ADDRESS+1400000+1 $COUNTERPARTY_TOKEN" \
                               --required-signer "$COUNTERPARTY_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                       \
                               --out-file /dev/null                                     \
                               --submit 600
```

```console
TxId "28feeefe737ccd324104c2d93b9972de932b293390ae208a307732e406944857"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
marlowe-cli transaction simple --testnet-magic "$MAGIC"                         \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                \
                               --tx-in "$TX_8"#0                                        \
                               --tx-in "$TX_8"#1                                        \
                               --tx-in "$TX_8"#2                                        \
                               --tx-out "$BURN_ADDRESS+1400000+1 $ORACLE_TOKEN" \
                               --required-signer "$ORACLE_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                       \
                               --out-file /dev/null                                     \
                               --submit 600
```

```console
TxId "43276856ba98981aa17af193ccde83d2204483554328ca74c0d05459609fe88d"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
