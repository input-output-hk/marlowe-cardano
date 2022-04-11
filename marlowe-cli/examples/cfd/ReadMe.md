# Test of a Contract for Differences

In this example execution of a contract for differences, the party and counterparty each make a deposit to cover margin, an oracle reports prices, and the price change is settled among the parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed
* tr
* basenc

Signing and verification keys must be provided below for the oracle and party roles: to do this, set the environment variables `PARTY_PREFIX`, `COUNTERPARTY_PREFIX`, and `ORACLE_PREFIX` where they appear below.

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

#### The Party

The party bets that the asset under consideration will decrease. If it increases, they pay the difference in price to the counterparty.

```
PARTY_PREFIX="$TREASURY/francis-beaumont"
PARTY_NAME="Francis Beaumont"
PARTY_ROLE=FB
PARTY_ROLE_HEX=$(echo -n "$PARTY_ROLE" | basenc --base16 | tr '[:upper:]' '[:lower:]')
PARTY_TOKEN="$ROLE_CURRENCY.$PARTY_ROLE"
PARTY_PAYMENT_SKEY="$PARTY_PREFIX".skey
PARTY_PAYMENT_VKEY="$PARTY_PREFIX".vkey
PARTY_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                         \
                            --payment-verification-key-file "$PARTY_PAYMENT_VKEY" \
)
```

The party Francis Beaumont has the address `addr_test1vrtntkszteptml4e9ce9l3fsmgavwv4ywunvdnhxv6nw5ksq6737a` and role token named `FB`. They have the following UTxOs in their wallet:

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
d2c880a3b1626ff96c593ffbdba9dc0e1c3ad3574dbd32ec43ced945d3f5fbb8     0        11414257969 lovelace + TxOutDatumNone
d2c880a3b1626ff96c593ffbdba9dc0e1c3ad3574dbd32ec43ced945d3f5fbb8     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
```

We select the UTxO with the party Francis Beaumont's role token.

```
TX_0_PARTY_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$PARTY_ADDRESS"                                                               \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_PARTY_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                         \
                       --address "$PARTY_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$PARTY_ROLE_HEX"'" == 1) | .key' \
)
```

Francis Beaumont will spend the UTxOs `d2c880a3b1626ff96c593ffbdba9dc0e1c3ad3574dbd32ec43ced945d3f5fbb8#0` and `d2c880a3b1626ff96c593ffbdba9dc0e1c3ad3574dbd32ec43ced945d3f5fbb8#1`.

### The Counterparty

The counterparty bets that the asset under consideration will increase. If it decreases, they pay the difference in price to the counterparty.

```
COUNTERPARTY_PREFIX="$TREASURY/thomas-middleton"
COUNTERPARTY_NAME="Thomas Middleton"
COUNTERPARTY_ROLE=TM
COUNTERPARTY_ROLE_HEX=$(echo -n "$COUNTERPARTY_ROLE" | basenc --base16 | tr '[:upper:]' '[:lower:]')
COUNTERPARTY_TOKEN="$ROLE_CURRENCY.$COUNTERPARTY_ROLE"
COUNTERPARTY_PAYMENT_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_PAYMENT_VKEY="$COUNTERPARTY_PREFIX".vkey
COUNTERPARTY_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                                \
                            --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY" \
)
```

The counterparty Thomas Middleton has the address `addr_test1vzgrqnlp6elmettvuelx5vkn0uxhtu2ewqdhx297ukgjmjgpss5k0` and role token named `TM`. They have the following UTxOs in their wallet:

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
48c3a31df672c33f4d094fde7f439728467260143bff0d4369641668fa8ad841     0        10380072306 lovelace + TxOutDatumNone
48c3a31df672c33f4d094fde7f439728467260143bff0d4369641668fa8ad841     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
```

We select the UTxO with the counterparty Thomas Middleton's role token.

```
TX_0_COUNTERPARTY_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$COUNTERPARTY_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_COUNTERPARTY_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                                \
                       --address "$COUNTERPARTY_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                       \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$COUNTERPARTY_ROLE_HEX"'" == 1) | .key' \
)
```

Thomas Middleton will spend the UTxOs `48c3a31df672c33f4d094fde7f439728467260143bff0d4369641668fa8ad841#0` and `48c3a31df672c33f4d094fde7f439728467260143bff0d4369641668fa8ad841#1`.

### The Oracle

The oracle reports prices to the contract.

```
ORACLE_PREFIX="$TREASURY/christopher-marlowe"
ORACLE_NAME="Christopher Marlowe"
ORACLE_ROLE=CM
ORACLE_ROLE_HEX=$(echo -n "$ORACLE_ROLE" | basenc --base16 | tr '[:upper:]' '[:lower:]')
ORACLE_TOKEN="$ROLE_CURRENCY.$ORACLE_ROLE"
ORACLE_PAYMENT_SKEY="$ORACLE_PREFIX".skey
ORACLE_PAYMENT_VKEY="$ORACLE_PREFIX".vkey
ORACLE_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$ORACLE_PAYMENT_VKEY" \
)
```

The oracle Christopher Marlowe has the address `addr_test1vqhqudxtwqcpjqesns79hqgqq2q0xx5q0hnzz5es9492yaqpxltpy` and role token named `CM`. They have the following UTxOs in their wallet:

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
69fe68002ac6e00d74ac85407a48f6c91b89ad7dee1bcda59c25a1f1a051a876     0        21870318964 lovelace + TxOutDatumNone
69fe68002ac6e00d74ac85407a48f6c91b89ad7dee1bcda59c25a1f1a051a876     1        2000000 lovelace + 5000 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4265617247617264656e + TxOutDatumNone
69fe68002ac6e00d74ac85407a48f6c91b89ad7dee1bcda59c25a1f1a051a876     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
```

We select the UTxO with the oracle Christopher Marlowe's role token.

```
TX_0_ORACLE_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$ORACLE_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_ORACLE_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                          \
                       --address "$ORACLE_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                 \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$ORACLE_ROLE_HEX"'" == 1) | .key' \
)
```

Christopher Marlowe will spend the UTxOs `69fe68002ac6e00d74ac85407a48f6c91b89ad7dee1bcda59c25a1f1a051a876#0` and `69fe68002ac6e00d74ac85407a48f6c91b89ad7dee1bcda59c25a1f1a051a876#2`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"
```

The tip is at slot 2430099. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2430100. Tests may fail if this is not the case.

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

* The current slot is 2430099.
* The party Francis Beaumont must pay the margin deposit of 7000000 lovelace before Wed, 16 Mar 2022 03:55:39 +0000.
* The counterparty Thomas Middleton must pay the margin deposit of 8000000 lovelace before Wed, 16 Mar 2022 09:55:39 +0000.
* The oracle Christopher Marlowe report the first price of 1000000 lovelace between Tue, 15 Mar 2022 15:35:39 +0000 and Wed, 16 Mar 2022 15:55:39 +0000.
* The oracle Christopher Marlowe report the second price of 1500000 lovelace between Tue, 15 Mar 2022 15:36:39 +0000 and Thu, 17 Mar 2022 15:55:39 +0000.

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
Fee: Lovelace 217509
Size: 1169 / 32768 = 3%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 2000000 lovelace from the oracle Christopher Marlowe in the transaction `2e3dfed91df4d8b00ed175b74676a7d53f4e35003fc3da02293e0af398340ecb`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2e3dfed91df4d8b00ed175b74676a7d53f4e35003fc3da02293e0af398340ecb     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1266bf3c8e3ad3a08d3c6f681f0cbbd8da4c0a748a5c7a5fbaad9d9d1bf5e330"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2e3dfed91df4d8b00ed175b74676a7d53f4e35003fc3da02293e0af398340ecb     0        21868101455 lovelace + TxOutDatumNone
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
Fee: Lovelace 1304808
Size: 14279 / 32768 = 43%
Execution units:
  Memory: 5928096 / 30000000 = 19%
  Steps: 2211258555 / 10000000000 = 22%
```

The contract received the margin deposit of 7000000 lovelace in the transaction `84880ff45f23bf563491ce336ac1568d6136bb6326c10fc7e83542dbaaa988d9`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
84880ff45f23bf563491ce336ac1568d6136bb6326c10fc7e83542dbaaa988d9     1        9000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "37dc3ed6e80367dd57ab75d49a2f03c4eeac1091082e6bb866d0097be90ebd8b"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
84880ff45f23bf563491ce336ac1568d6136bb6326c10fc7e83542dbaaa988d9     0        11405953161 lovelace + TxOutDatumNone
84880ff45f23bf563491ce336ac1568d6136bb6326c10fc7e83542dbaaa988d9     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
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
Fee: Lovelace 1317704
Size: 14199 / 32768 = 43%
Execution units:
  Memory: 6152534 / 30000000 = 20%
  Steps: 2259325124 / 10000000000 = 22%
```

The contract received the margin deposit of 8000000 lovelace in the transaction `281c603c3ac7156bbb06d50efdc27bde814057f541ee0a82fe333bde4588a51f`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
281c603c3ac7156bbb06d50efdc27bde814057f541ee0a82fe333bde4588a51f     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "d95cf1ec40b4672b240e7cd38f22691f8b0d7aa5ab3f966b3503d610df746dd1"
```

Here is the UTxO at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
281c603c3ac7156bbb06d50efdc27bde814057f541ee0a82fe333bde4588a51f     0        10370754602 lovelace + TxOutDatumNone
281c603c3ac7156bbb06d50efdc27bde814057f541ee0a82fe333bde4588a51f     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
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
Fee: Lovelace 1306464
Size: 14118 / 32768 = 43%
Execution units:
  Memory: 6081312 / 30000000 = 20%
  Steps: 2209861116 / 10000000000 = 22%
```

The contract received the price report in the transaction `6aaa7db3e34e7fa31f2244bf6aa2f0f86f671a4d3ec38abe4c57522eead19af1`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6aaa7db3e34e7fa31f2244bf6aa2f0f86f671a4d3ec38abe4c57522eead19af1     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5b4083ba9a546e9da76486f076d34c667df7826aa2cace25d282b8b45faf9508"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6aaa7db3e34e7fa31f2244bf6aa2f0f86f671a4d3ec38abe4c57522eead19af1     0        21866794991 lovelace + TxOutDatumNone
6aaa7db3e34e7fa31f2244bf6aa2f0f86f671a4d3ec38abe4c57522eead19af1     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
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
Fee: Lovelace 1550883
Size: 13416 / 32768 = 40%
Execution units:
  Memory: 9589068 / 30000000 = 31%
  Steps: 3221092093 / 10000000000 = 32%
```

The contract received the price report in the transaction `2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654` and settled the payments. There is no UTxO at the contract address:

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
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     0        21865244108 lovelace + TxOutDatumNone
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     4        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.434d + TxOutDatumNone
```

This settles the difference contract, so payments are made to each participant. Here are the UTxO for payments at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     2        6500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
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
Fee: Lovelace 426739
Size: 2889 / 32768 = 8%
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
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
eaef3f85b05026ae893da78130e3829ced360b57efd08e4257f2aa5e6dade863     0        11405526422 lovelace + TxOutDatumNone
eaef3f85b05026ae893da78130e3829ced360b57efd08e4257f2aa5e6dade863     1        6500000 lovelace + TxOutDatumNone
eaef3f85b05026ae893da78130e3829ced360b57efd08e4257f2aa5e6dade863     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4642 + TxOutDatumNone
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
Fee: Lovelace 422397
Size: 2889 / 32768 = 8%
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
2ceae26e3ed94f82a36619f1c3b4c98d38688132f4b276ae701cbd12b4b26654     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
84e47afdb64b78c8c1135c058451c84219955b8ae29b12cbc5b5b2ae4812f03f     0        10370332205 lovelace + TxOutDatumNone
84e47afdb64b78c8c1135c058451c84219955b8ae29b12cbc5b5b2ae4812f03f     1        8500000 lovelace + TxOutDatumNone
84e47afdb64b78c8c1135c058451c84219955b8ae29b12cbc5b5b2ae4812f03f     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
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
Fee: Lovelace 426739
Size: 2889 / 32768 = 8%
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

Here are the UTxOs at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
84e47afdb64b78c8c1135c058451c84219955b8ae29b12cbc5b5b2ae4812f03f     0        10370332205 lovelace + TxOutDatumNone
84e47afdb64b78c8c1135c058451c84219955b8ae29b12cbc5b5b2ae4812f03f     1        8500000 lovelace + TxOutDatumNone
84e47afdb64b78c8c1135c058451c84219955b8ae29b12cbc5b5b2ae4812f03f     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.544d + TxOutDatumNone
```

