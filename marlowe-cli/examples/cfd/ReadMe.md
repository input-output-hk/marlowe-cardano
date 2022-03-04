# Test of a Contract for Differences

In this example execution of a contract for differences, the party and counterparty each make a deposit to cover margin, an oracle reports prices, and the price change is settled among the parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the oracle and party roles: to do this, set the environment variables `PARTY_PREFIX`, `COUNTERPARTY_PREFIX`, and `ORACLE_PREFIX` where they appear below.

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
be121e04fd24ee5675a2af311ddacdaff6aea605931c70bf9d888caeb44a3126     0        987156771 lovelace + TxOutDatumNone
be121e04fd24ee5675a2af311ddacdaff6aea605931c70bf9d888caeb44a3126     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
be121e04fd24ee5675a2af311ddacdaff6aea605931c70bf9d888caeb44a3126     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FrancisBeaumont + TxOutDatumNone
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
cardano-cli query utxo "${MAGIC[@]}"                                                                     \
                       --address "$PARTY_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                            \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$PARTY_ROLE"'" == 1) | .key' \
)
```

Francis Beaumont will spend the UTxOs `be121e04fd24ee5675a2af311ddacdaff6aea605931c70bf9d888caeb44a3126#0` and `be121e04fd24ee5675a2af311ddacdaff6aea605931c70bf9d888caeb44a3126#1`.

### The Counterparty

The counterparty bets that the asset under consideration will increase. If it decreases, they pay the difference in price to the counterparty.

```
COUNTERPARTY_PREFIX="$TREASURY/thomas-middleton"
COUNTERPARTY_NAME="Thomas Middleton"
COUNTERPARTY_ROLE=TM
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
1d6f4a1f24ae5ac6a8b5ee633c26615778a2e8511cb93805fc9efa2ceab7086e     0        990079718 lovelace + TxOutDatumNone
1d6f4a1f24ae5ac6a8b5ee633c26615778a2e8511cb93805fc9efa2ceab7086e     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
1d6f4a1f24ae5ac6a8b5ee633c26615778a2e8511cb93805fc9efa2ceab7086e     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
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
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$COUNTERPARTY_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$COUNTERPARTY_ROLE"'" == 1) | .key' \
)
```

Thomas Middleton will spend the UTxOs `1d6f4a1f24ae5ac6a8b5ee633c26615778a2e8511cb93805fc9efa2ceab7086e#0` and `1d6f4a1f24ae5ac6a8b5ee633c26615778a2e8511cb93805fc9efa2ceab7086e#1`.

### The Oracle

The oracle reports prices to the contract.

```
ORACLE_PREFIX="$TREASURY/christopher-marlowe"
ORACLE_NAME="Christopher Marlowe"
ORACLE_ROLE=CM
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
a213491f0e3f23cb315d479dcc186e3600bb525f9ff98277429e53bb7f479a62     0        969542057 lovelace + TxOutDatumNone
a213491f0e3f23cb315d479dcc186e3600bb525f9ff98277429e53bb7f479a62     1        2000000 lovelace + 9000 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.BearGarden + TxOutDatumNone
a213491f0e3f23cb315d479dcc186e3600bb525f9ff98277429e53bb7f479a62     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
a213491f0e3f23cb315d479dcc186e3600bb525f9ff98277429e53bb7f479a62     3        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ChristopherMarlowe + TxOutDatumNone
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
cardano-cli query utxo "${MAGIC[@]}"                                                                      \
                       --address "$ORACLE_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                             \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$ORACLE_ROLE"'" == 1) | .key' \
)
```

Christopher Marlowe will spend the UTxOs `a213491f0e3f23cb315d479dcc186e3600bb525f9ff98277429e53bb7f479a62#0` and `a213491f0e3f23cb315d479dcc186e3600bb525f9ff98277429e53bb7f479a62#2`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"
```

The tip is at slot 52064877. The current POSIX time implies that the tip of the blockchain should be slightly before slot 52064879. Tests may fail if this is not the case.

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

* The current slot is 52064877.
* The party Francis Beaumont must pay the margin deposit of 7000000 lovelace before Sat, 05 Mar 2022 10:48:13 +0000.
* The counterparty Thomas Middleton must pay the margin deposit of 8000000 lovelace before Sat, 05 Mar 2022 16:48:13 +0000.
* The oracle Christopher Marlowe report the first price of 1000000 lovelace between Fri, 04 Mar 2022 22:28:13 +0000 and Sat, 05 Mar 2022 22:48:13 +0000.
* The oracle Christopher Marlowe report the second price of 1500000 lovelace between Fri, 04 Mar 2022 22:29:13 +0000 and Sun, 06 Mar 2022 22:48:13 +0000.

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
Validator size: 13905
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 38794319, exBudgetMemory = ExMemory 130400}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wqw36qew4yx6mdc9lfwe5yr9flkz5pej734mujkgqr4kj8gwqcfh9`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpdsreg339dsku87hskeenlwcaalhdu32dd85rrrjwr002g3c0yy3`.

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
Size: 1165 / 16384 = 7%
Execution units:
  Memory: 0 / 16000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 2000000 lovelace from the oracle Christopher Marlowe in the transaction `608f1f76e322bf16ca32331f197749f3d6184138aa82a0c48704a29ab6aafa94`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
608f1f76e322bf16ca32331f197749f3d6184138aa82a0c48704a29ab6aafa94     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "d9265174a7897db93cded22d2a13168e0178184662cc3d0b2924fab28e3e79ba"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
608f1f76e322bf16ca32331f197749f3d6184138aa82a0c48704a29ab6aafa94     0        967324724 lovelace + TxOutDatumNone
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
Fee: Lovelace 1443226
Size: 16219 / 16384 = 98%
Execution units:
  Memory: 6598496 / 16000000 = 41%
  Steps: 2410646373 / 10000000000 = 24%
```

The contract received the margin deposit of 7000000 lovelace in the transaction `b9e5203b8d6d4be3bb97cec2728fc67b56155fb000e2d1d5fe95a27eab8e2ea9`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b9e5203b8d6d4be3bb97cec2728fc67b56155fb000e2d1d5fe95a27eab8e2ea9     1        9000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1bec751e6642c8b9180bb242b1ac508a60b077d7ce6c2152fea68f4e6223bb19"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b9e5203b8d6d4be3bb97cec2728fc67b56155fb000e2d1d5fe95a27eab8e2ea9     0        978713545 lovelace + TxOutDatumNone
b9e5203b8d6d4be3bb97cec2728fc67b56155fb000e2d1d5fe95a27eab8e2ea9     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
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
Fee: Lovelace 1462075
Size: 16139 / 16384 = 98%
Execution units:
  Memory: 6898134 / 16000000 = 43%
  Steps: 2481100262 / 10000000000 = 24%
```

The contract received the margin deposit of 8000000 lovelace in the transaction `89dc32258975d3b0a045370ba350abfd2cd5398468c429f5036942fda9fd017a`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
89dc32258975d3b0a045370ba350abfd2cd5398468c429f5036942fda9fd017a     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "882f54d5fc1eeaff12d9c5b418dea523cff173ad01d4353a36ad26f9cfedc9e9"
```

Here is the UTxO at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
89dc32258975d3b0a045370ba350abfd2cd5398468c429f5036942fda9fd017a     0        980617643 lovelace + TxOutDatumNone
89dc32258975d3b0a045370ba350abfd2cd5398468c429f5036942fda9fd017a     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
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
Fee: Lovelace 1452252
Size: 16058 / 16384 = 98%
Execution units:
  Memory: 6844812 / 16000000 = 42%
  Steps: 2436965127 / 10000000000 = 24%
```

The contract received the price report in the transaction `f3558cfa7bd0f68f23cb189eda74a0521a172e350e696714f330864b75380b9d`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f3558cfa7bd0f68f23cb189eda74a0521a172e350e696714f330864b75380b9d     1        17000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9fe88b08aa9c74982058fabdd9efa4854e704b94119661b660b356b337c337ea"
```

Here is the UTxO at the oracle Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f3558cfa7bd0f68f23cb189eda74a0521a172e350e696714f330864b75380b9d     0        965872472 lovelace + TxOutDatumNone
f3558cfa7bd0f68f23cb189eda74a0521a172e350e696714f330864b75380b9d     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
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
Fee: Lovelace 1751777
Size: 15356 / 16384 = 93%
Execution units:
  Memory: 11048468 / 16000000 = 69%
  Steps: 3655576037 / 10000000000 = 36%
```

The contract received the price report in the transaction `43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06` and settled the payments. There is no UTxO at the contract address:

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
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     0        964120695 lovelace + TxOutDatumNone
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     4        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
```

This settles the difference contract, so payments are made to each participant. Here are the UTxO for payments at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     2        6500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
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
Fee: Lovelace 451385
Size: 3187 / 16384 = 19%
Execution units:
  Memory: 1607510 / 16000000 = 10%
  Steps: 601307457 / 10000000000 = 6%
```

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     3        8500000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
```

Here are the UTxOs at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3ff79fa5570b28af3708d6410c5ba813edc57ad2c3ee3a6b0523c1b40161434d     0        978262160 lovelace + TxOutDatumNone
3ff79fa5570b28af3708d6410c5ba813edc57ad2c3ee3a6b0523c1b40161434d     1        6500000 lovelace + TxOutDatumNone
3ff79fa5570b28af3708d6410c5ba813edc57ad2c3ee3a6b0523c1b40161434d     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
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
Fee: Lovelace 451385
Size: 3187 / 16384 = 19%
Execution units:
  Memory: 1607510 / 16000000 = 10%
  Steps: 601307457 / 10000000000 = 6%
```

There are still pending payment UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
43dcd994ffe6bb3a170ddd26536d01f4ef08cb0f0af33c102586c6924373ec06     1        2000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
```

Here are the UTxOs at the counterparty Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e1d1a31371e9083bd574c1b0db5d73d44c2bada86987a2407ce78dcf82c499ed     0        980166258 lovelace + TxOutDatumNone
e1d1a31371e9083bd574c1b0db5d73d44c2bada86987a2407ce78dcf82c499ed     1        8500000 lovelace + TxOutDatumNone
e1d1a31371e9083bd574c1b0db5d73d44c2bada86987a2407ce78dcf82c499ed     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
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
Fee: Lovelace 451385
Size: 3187 / 16384 = 19%
Execution units:
  Memory: 1607510 / 16000000 = 10%
  Steps: 601307457 / 10000000000 = 6%
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
e1d1a31371e9083bd574c1b0db5d73d44c2bada86987a2407ce78dcf82c499ed     0        980166258 lovelace + TxOutDatumNone
e1d1a31371e9083bd574c1b0db5d73d44c2bada86987a2407ce78dcf82c499ed     1        8500000 lovelace + TxOutDatumNone
e1d1a31371e9083bd574c1b0db5d73d44c2bada86987a2407ce78dcf82c499ed     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
```

