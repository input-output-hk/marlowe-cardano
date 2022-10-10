#!/usr/bin/env bash

# This script exits with an error value if the end-to-end test fails.
set -eo pipefail

echo '# Test of a Contract for Differences'

echo "In this example execution of a contract for differences, the party and counterparty each make a deposit to cover margin, an oracle reports prices, and the price change is settled among the parties."

echo "## Prerequisites"

echo "The environment variable "'`'"CARDANO_NODE_SOCKET_PATH"'`'" must be set to the path to the cardano node's socket."
echo "See below for how to set "'`'"MAGIC"'`'" to select the network."
echo
echo 'The following tools must be on the PATH:'
echo '* [marlowe-cli](../../ReadMe.md)'
echo '* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)'
echo '* [jq](https://stedolan.github.io/jq/manual/)'
echo '* sed'
echo
echo "Signing and verification keys must be provided below for the oracle and party roles, or they will be created automatically: to do this, set the environment variables "'`'"PARTY_PREFIX"'`'", "'`'"COUNTERPARTY_PREFIX"'`'", and "'`'"ORACLE_PREFIX"'`'" where they appear below."

echo "## Preliminaries"

: "${FAUCET_ADDRESS:?FAUCET_ADDRESS not set}"
: "${FAUCET_SKEY_FILE:?FAUCET_SKEY_FILE not set}"

echo "### Select Network"

: "${MAGIC:=2}"
echo "MAGIC=$MAGIC"

SLOT_LENGTH=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotLength)
SLOT_OFFSET=$(marlowe-cli util slotting --testnet-magic "$MAGIC" --socket-path "$CARDANO_NODE_SOCKET_PATH" | jq .scSlotZeroTime)

echo "### Tip of the Blockchain"

TIP=$(cardano-cli query tip --testnet-magic "$MAGIC" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"

echo "The tip is at slot $TIP. The current POSIX time implies that the tip of the blockchain should be slightly before slot $(((1000 * $(date -u +%s) - SLOT_OFFSET) / SLOT_LENGTH)). Tests may fail if this is not the case."

echo "### Participants"

echo "#### The Party"

echo "The party sells an item for a price."

PARTY_PREFIX="$TREASURY/francis-beaumont"
PARTY_NAME="Francis Beaumont"
PARTY_ROLE=FB
PARTY_PAYMENT_SKEY="$PARTY_PREFIX".skey
PARTY_PAYMENT_VKEY="$PARTY_PREFIX".vkey

echo "Create the party's keys, if necessary."

if [[ ! -e "$PARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_PAYMENT_VKEY"
fi
PARTY_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_PAYMENT_VKEY")

echo "Fund the party's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                        "$PARTY_ADDRESS"

echo "#### The Counterparty"

COUNTERPARTY_PREFIX="$TREASURY/thomas-middleton"
COUNTERPARTY_NAME="Thomas Middleton"
COUNTERPARTY_ROLE=TM
COUNTERPARTY_PAYMENT_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_PAYMENT_VKEY="$COUNTERPARTY_PREFIX".vkey

echo "Create the counterparty's keys, if necessary."

if [[ ! -e "$COUNTERPARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$COUNTERPARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$COUNTERPARTY_PAYMENT_VKEY"
fi
COUNTERPARTY_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY")

echo "Fund the counterparty's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 50000000                       \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$COUNTERPARTY_ADDRESS"

echo "#### The Oracle"

ORACLE_PREFIX="$TREASURY/christopher-marlowe"
ORACLE_NAME="Christopher Marlowe"
ORACLE_ROLE=CM
ORACLE_PAYMENT_SKEY="$ORACLE_PREFIX".skey
ORACLE_PAYMENT_VKEY="$ORACLE_PREFIX".vkey

echo "Create the oracle's keys, if necessary."

if [[ ! -e "$ORACLE_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$ORACLE_PAYMENT_SKEY"      \
                              --verification-key-file "$ORACLE_PAYMENT_VKEY"
fi
ORACLE_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$ORACLE_PAYMENT_VKEY")

echo "Fund the oracle's address."

marlowe-cli util fund-address --testnet-magic "$MAGIC"                  \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --out-file /dev/null                      \
                              --submit 600                              \
                              --lovelace 100000000                      \
                              --source-wallet-credentials  "$FAUCET_ADDRESS:$FAUCET_SKEY_FILE" \
                              "$ORACLE_ADDRESS"

echo "### Role Tokens"

echo "The oracle mints the role tokens."

MINT_EXPIRES=$((TIP + 1000000))


ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                          \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"         \
                      --issuer "$ORACLE_ADDRESS:$ORACLE_PAYMENT_SKEY"   \
                      --expires "$MINT_EXPIRES"                         \
                      --out-file /dev/null                              \
                      --submit=600                                      \
                      "$ORACLE_ROLE:$ORACLE_ADDRESS"                    \
                      "$PARTY_ROLE:$PARTY_ADDRESS"                     \
                      "$COUNTERPARTY_ROLE:$COUNTERPARTY_ADDRESS"        \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                    \
)

ORACLE_TOKEN="$ROLE_CURRENCY.$ORACLE_ROLE"
PARTY_TOKEN="$ROLE_CURRENCY.$PARTY_ROLE"
COUNTERPARTY_TOKEN="$ROLE_CURRENCY.$COUNTERPARTY_ROLE"

echo "The oracle $ORACLE_NAME is the minimum-ADA provider and has the address "'`'"$ORACLE_ADDRESS"'`'" and role token named "'`'"$ORACLE_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"          \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$ORACLE_PAYMENT_SKEY"  \
                       --change-address "$ORACLE_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS"

echo "We select the UTxO with the oracle $ORACLE_NAME's role token."

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

echo "$ORACLE_NAME will spend the UTxOs "'`'"$TX_0_ORACLE_ADA"'`'" and "'`'"$TX_0_ORACLE_TOKEN"'`.'

echo "The party $PARTY_NAME has the address "'`'"$PARTY_ADDRESS"'`'" and role token named "'`'"$PARTY_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"          \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_PAYMENT_SKEY"   \
                       --change-address "$PARTY_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS"

echo "We select the UTxO with the lender $PARTY_NAME's role token."

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

echo "$PARTY_NAME will spend the UTxOs "'`'"$TX_0_PARTY_ADA"'`'" and "'`'"$TX_0_PARTY_TOKEN"'`.'

echo "The counterparty $COUNTERPARTY_NAME has the address "'`'"$COUNTERPARTY_ADDRESS"'`'" and role token named "'`'"$COUNTERPARTY_ROLE"'`'". They have the following UTxOs in their wallet:"

marlowe-cli util clean --testnet-magic "$MAGIC"               \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                       --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                       --change-address "$COUNTERPARTY_ADDRESS"       \
                       --out-file /dev/null                           \
                       --submit=600                                   \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS"

echo "We select the UTxO with the lender $COUNTERPARTY_NAME's role token."

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

echo "$COUNTERPARTY_NAME will spend the UTxOs "'`'"$TX_0_COUNTERPARTY_ADA"'`'" and "'`'"$TX_0_COUNTERPARTY_TOKEN"'`.'

echo "## The Contract"

echo "The contract has several deadlines. These are set to unrealistic values so that the example can be run quickly."

PARTY_MARGIN_DEADLINE="$((NOW+12*HOUR))"
COUNTERPARTY_MARGIN_DEADLINE="$((NOW+18*HOUR))"
FIRST_PRICE_TIME="$((NOW-20*MINUTE))"
FIRST_PRICE_DEADLINE="$((NOW+24*HOUR))"
SECOND_PRICE_TIME="$((NOW-19*MINUTE))"
SECOND_PRICE_DEADLINE="$((NOW+48*HOUR))"

echo "The contract also involves the margin deposit amount and a minimum-ADA value."

MINIMUM_ADA=2000000
PARTY_MARGIN=7000000
COUNTERPARTY_MARGIN=8000000

echo "The oracle will report a pair of prices."

FIRST_PRICE=1000000
SECOND_PRICE=1500000

echo "* The current slot is $TIP."
echo "* The party $PARTY_NAME must pay the margin deposit of $PARTY_MARGIN lovelace before $(date -u -R -d @$((PARTY_MARGIN_DEADLINE/1000)))."
echo "* The counterparty $COUNTERPARTY_NAME must pay the margin deposit of $COUNTERPARTY_MARGIN lovelace before $(date -u -R -d @$((COUNTERPARTY_MARGIN_DEADLINE/1000)))."
echo "* The oracle $ORACLE_NAME report the first price of $FIRST_PRICE lovelace between $(date -u -R -d @$((FIRST_PRICE_TIME/1000))) and $(date -u -R -d @$((FIRST_PRICE_DEADLINE/1000)))."
echo "* The oracle $ORACLE_NAME report the second price of $SECOND_PRICE lovelace between $(date -u -R -d @$((SECOND_PRICE_TIME/1000))) and $(date -u -R -d @$((SECOND_PRICE_DEADLINE/1000)))."

echo "We create the contract for the previously specified parameters. User Marlowe Playground to create contracts and export them to JSON files like the following."

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

echo "Also create the initial state for the contract, which simply has the oracle starting the contract with the minimum required ADA."

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

echo "## Transaction 1. Create the Contract by Providing the Minimum ADA."

echo "First we create a "'`'".marlowe"'`'" file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require."

marlowe-cli run initialize --testnet-magic "$MAGIC"          \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --roles-currency "$ROLE_CURRENCY"         \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --print-stats

echo "In particular, we can extract the contract's address from the "'`'".marlowe"'`'" file."

CONTRACT_ADDRESS=$(jq -r '.tx.marloweValidator.address' tx-1.marlowe)

echo "The Marlowe contract resides at address "'`'"$CONTRACT_ADDRESS"'`.'

echo "Because this is a role-based contract, we compute the address of the script for roles."

ROLE_ADDRESS=$(jq -r '.tx.rolesValidator.address' tx-1.marlowe)

echo "The role address is "'`'"$ROLE_ADDRESS"'`.'

echo "The oracle $ORACLE_NAME submits the transaction along with the minimum ADA $MINIMUM_ADA lovelace required for the contract's initial state. Submitting with the "'`'"--print-stats"'`'" switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits."

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

echo "The contract received the minimum ADA of $MINIMUM_ADA lovelace from the oracle $ORACLE_NAME in the transaction "'`'"$TX_1"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "Here is the UTxO at the oracle $ORACLE_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"

echo "## Transaction 2. Party Deposits Margin Funds into Their Account."

echo "First we compute the Marlowe input required to make the margin deposit by the party."

marlowe-cli run prepare --marlowe-file tx-1.marlowe   \
                        --deposit-account "$PARTY_ROLE"       \
                        --deposit-party "$PARTY_ROLE"         \
                        --deposit-amount "$PARTY_MARGIN"      \
                        --invalid-before "$((NOW-18*MINUTE))" \
                        --invalid-hereafter "$((NOW+3*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats

echo "Now the party $PARTY_NAME submits the transaction along with their deposit:"

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

echo "The contract received the margin deposit of $PARTY_MARGIN lovelace in the transaction "'`'"$TX_2"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "Here is the UTxO at the party $PARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"

echo "## Transaction 3. Counterparty Deposits Margin Funds into Their Account."

echo "First we compute the Marlowe input required to make the margin deposit by the counterparty."

marlowe-cli run prepare --marlowe-file tx-2.marlowe         \
                        --deposit-account "$COUNTERPARTY_ROLE"      \
                        --deposit-party "$COUNTERPARTY_ROLE"        \
                        --deposit-amount "$COUNTERPARTY_MARGIN"     \
                        --invalid-before "$((NOW-18*MINUTE))"       \
                        --invalid-hereafter "$((NOW+3*HOUR))"       \
                        --out-file tx-3.marlowe                     \
                        --print-stats

echo "Now the counterparty $COUNTERPARTY_NAME submits the transaction along with their deposit:"

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

echo "The contract received the margin deposit of $COUNTERPARTY_MARGIN lovelace in the transaction "'`'"$TX_3"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "Here is the UTxO at the counterparty $COUNTERPARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"

echo "## Transaction 4. Oracle Reports the First Price."

echo "First we compute the Marlowe input required for the oracle to report the price."

marlowe-cli run prepare --marlowe-file tx-3.marlowe   \
                        --choice-name "Price in first window" \
                        --choice-party "$ORACLE_ROLE"         \
                        --choice-number "$FIRST_PRICE"        \
                        --invalid-before "$((NOW-18*MINUTE))" \
                        --invalid-hereafter "$((NOW+3*HOUR))" \
                        --out-file tx-4.marlowe               \
                        --print-stats

echo "Now the oracle $ORACLE_NAME submits the transaction containing their price report:"

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

echo "The contract received the price report in the transaction "'`'"$TX_4"'`'".  Here is the UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "Here is the UTxO at the oracle $ORACLE_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"

echo "## Transaction 5. Oracle Reports the Second Price."

echo "First we compute the Marlowe input required for the oracle to report the price."

marlowe-cli run prepare --marlowe-file tx-4.marlowe    \
                        --choice-name "Price in second window" \
                        --choice-party "$ORACLE_ROLE"          \
                        --choice-number "$SECOND_PRICE"        \
                        --invalid-before "$((NOW-18*MINUTE))"  \
                        --invalid-hereafter "$((NOW+3*HOUR))"  \
                        --out-file tx-5.marlowe                \
                        --print-stats

echo "Now the oracle $ORACLE_NAME submits the transaction containing their price report:"

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

echo "The contract received the price report in the transaction "'`'"$TX_5"'`'" and settled the payments. There is no UTxO at the contract address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "Here is the UTxO at the oracle $ORACLE_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "This settles the difference contract, so payments are made to each participant. Here are the UTxO for payments at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"

echo "## Transaction 6. Party Withdraws Their Remaining Margin Deposit."

echo "The party $PARTY_NAME submits a transaction to withdraw their margin deposit minus the price difference."

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

echo "There are still pending payment UTxOs at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"

echo "Here are the UTxOs at the party $PARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"

echo "## Transaction 7. Counterparty Withdraws Their Margin Deposit and Profit."

echo "The counterparty $COUNTERPARTY_NAME submits a transaction to withdraw their margin deposit plus the price difference."

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

echo "There are still pending payment UTxOs at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"

echo "Here are the UTxOs at the counterparty $COUNTERPARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"

echo "## Transaction 8. Oracle Withdraws Their Deposit."

echo "The oracle $ORACLE_NAME submits a transaction to withdraw their minimum ADA deposit."

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

echo "There are no UTxOs at the role address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"

echo "Here are the UTxOs at the party $PARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"

echo "Here are the UTxOs at the counterparty $COUNTERPARTY_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"

echo "Here are the UTxOs at the oracle $ORACLE_NAME's address:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"

echo "## Clean Up"

echo "Burning tokens issued by ORACLE:"

marlowe-cli util burn --testnet-magic "$MAGIC" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --issuer "$ORACLE_ADDRESS:$ORACLE_PAYMENT_SKEY" \
                      --expires $MINT_EXPIRES \
                      --token-provider "$ORACLE_ADDRESS:$ORACLE_PAYMENT_SKEY" \
                      --token-provider "$PARTY_ADDRESS:$PARTY_PAYMENT_SKEY" \
                      --token-provider "$COUNTERPARTY_ADDRESS:$COUNTERPARTY_PAYMENT_SKEY" \
                      --submit 600 \
                      --out-file /dev/null

echo "Sending back funds:"

marlowe-cli util fund-address --testnet-magic "$MAGIC" \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --send-all \
                              --source-wallet-credentials "$ORACLE_ADDRESS:$ORACLE_PAYMENT_SKEY" \
                              --submit 600 \
                              --out-file /dev/null \
                              "$FAUCET_ADDRESS"

marlowe-cli util fund-address --testnet-magic "$MAGIC" \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --send-all \
                           --source-wallet-credentials "$PARTY_ADDRESS:$PARTY_PAYMENT_SKEY" \
                           --submit 600 \
                           --out-file /dev/null \
                           "$FAUCET_ADDRESS"

marlowe-cli util fund-address --testnet-magic "$MAGIC" \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                              --send-all \
                              --source-wallet-credentials "$COUNTERPARTY_ADDRESS:$COUNTERPARTY_PAYMENT_SKEY" \
                              --submit 600 \
                              --out-file /dev/null \
                              "$FAUCET_ADDRESS"

echo "Here are the UTxOs at the party $PARTY_NAME's address after cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"

echo "Here are the UTxOs at the counterparty $COUNTERPARTY_NAME's addres after cleanups:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"

echo "Here are the UTxOs at the oracle $ORACLE_NAME's address after cleanup:"

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ORACLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p;/$TX_8/p"


