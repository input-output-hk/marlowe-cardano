# Example Escrow Contract: "Dismiss Claim"

In this example execution of [an escrow contract](ReadMe.md), the buyer reports a problem, the seller disputes the problem, and the mediator dismisses the buyer's claim.

![Flow chart for "dismiss claim".](dismiss-claim.svg)

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.
See below for how to set `MAGIC` to select the network.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the bystander and party roles, or they will be created automatically: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `MEDIATOR_PREFIX` where they appear below.

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
```

The tip is at slot 3419593. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3419594. Tests may fail if this is not the case.

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
SELLER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$SELLER_PAYMENT_VKEY")
```

Fund the seller's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$SELLER_ADDRESS"
```

```console
TxId "86e761bff3d7e2fd5258d2c1f9f4fb442e37b13063524a5df1d55730b9b0ab37"
```

#### The Buyer

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
BUYER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$BUYER_PAYMENT_VKEY")
```

Fund the buyer's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 350000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$BUYER_ADDRESS"
```

```console
TxId "6d15e94d17d039a5e36077d6b64a5107dc662e1e41646d4557e5725e22cf6edf"
```

#### The Mediator

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
MEDIATOR_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY")
```

Fund the mediator's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 120000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$MEDIATOR_ADDRESS"
```

```console
TxId "c2d21e91f0db37580c53fb11e3f1d1bb807f27ec6dff49f58440f184a19333a5"
```

### Role Tokens

The mediator mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC" \
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
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$SELLER_TOKEN"              \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the seller their role token.

```
marlowe-cli transaction simple --testnet-magic "$MAGIC"                           \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"          \
                               --tx-in "$TX_MINT_SELLER"                          \
                               --tx-out "$SELLER_ADDRESS+2000000+1 $SELLER_TOKEN" \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY"         \
                               --change-address "$MEDIATOR_ADDRESS"               \
                               --out-file /dev/null                               \
                               --submit 600
```

```console
TxId "451cd37266be5b87c915b9d60a310b871505c98f7f69c16987901fb80d888dc7"
```

Find the transaction output with the buyer's role token.

```
TX_MINT_BUYER=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BUYER_TOKEN"               \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the buyer their role token.

```
marlowe-cli transaction simple --testnet-magic "$MAGIC"                         \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"        \
                               --tx-in "$TX_MINT_BUYER"                         \
                               --tx-out "$BUYER_ADDRESS+2000000+1 $BUYER_TOKEN" \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY"       \
                               --change-address "$MEDIATOR_ADDRESS"             \
                               --out-file /dev/null                             \
                               --submit 600
```

```console
TxId "5990bbb6c10e58c37aca73027ef50ef830c81c8fd1a60e26a21d48c2c750a815"
```

### Available UTxOs

The mediator Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz` and role token named `CM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$MEDIATOR_PAYMENT_SKEY"\
                       --change-address "$MEDIATOR_ADDRESS"      \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
38287caa020519f51016a925d0b63fed17a06386390b98ec25fa2bbfd98dbef7     0        113294700 lovelace + TxOutDatumNone
38287caa020519f51016a925d0b63fed17a06386390b98ec25fa2bbfd98dbef7     1        2000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.434d + TxOutDatumNone
```

We select the UTxO with the mediator Christopher Marlowe's role token.

```
TX_0_MEDIATOR_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 80000000                  \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_MEDIATOR_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$MEDIATOR_TOKEN"            \
                        "$MEDIATOR_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Christopher Marlowe will spend the UTxOs `38287caa020519f51016a925d0b63fed17a06386390b98ec25fa2bbfd98dbef7#0` and `38287caa020519f51016a925d0b63fed17a06386390b98ec25fa2bbfd98dbef7#1`.

The seller Francis Beaumont has the address `addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q` and role token named `FB`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$SELLER_PAYMENT_SKEY"  \
                       --change-address "$SELLER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c0b4ea32ecd9fae767f1017c411b4f35b5f254f7a1bf2fb8f73c5c01b6029080     0        49824907 lovelace + TxOutDatumNone
c0b4ea32ecd9fae767f1017c411b4f35b5f254f7a1bf2fb8f73c5c01b6029080     1        2000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.4642 + TxOutDatumNone
```

We select the UTxO with the lender Francis Beaumont's role token.

```
TX_0_SELLER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$SELLER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_SELLER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$SELLER_TOKEN"              \
                        "$SELLER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Francis Beaumont will spend the UTxOs `c0b4ea32ecd9fae767f1017c411b4f35b5f254f7a1bf2fb8f73c5c01b6029080#0` and `c0b4ea32ecd9fae767f1017c411b4f35b5f254f7a1bf2fb8f73c5c01b6029080#1`.

The buyer Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `TM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$BUYER_PAYMENT_SKEY"   \
                       --change-address "$BUYER_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
afa3e792dd039dd78a6ac9bf39d401b4d116e2840d0e75fc000da7824d2d04a0     0        349824907 lovelace + TxOutDatumNone
afa3e792dd039dd78a6ac9bf39d401b4d116e2840d0e75fc000da7824d2d04a0     1        2000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.544d + TxOutDatumNone
```

We select the UTxO with the lender Thomas Middleton's role token.

```
TX_0_BUYER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BUYER_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_BUYER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BUYER_TOKEN"               \
                        "$BUYER_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Middleton will spend the UTxOs `afa3e792dd039dd78a6ac9bf39d401b4d116e2840d0e75fc000da7824d2d04a0#0` and `afa3e792dd039dd78a6ac9bf39d401b4d116e2840d0e75fc000da7824d2d04a0#1`.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))
```

* The current slot is 3419593.
* The buyer Thomas Middleton must pay before Sun, 18 Sep 2022 13:53:13 +0000.
* They buyer Thomas Middleton has until Mon, 19 Sep 2022 13:53:13 +0000 to complain.
* The seller Francis Beaumont has until Tue, 20 Sep 2022 13:53:13 +0000 to dispute a complaint.
* The mediator Christopher Marlowe has until Wed, 21 Sep 2022 13:53:13 +0000 to decide on a disputed complaint.

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
                            --seller "$SELLER_ROLE"                    \
                            --buyer "$BUYER_ROLE"                      \
                            --mediator "$MEDIATOR_ROLE"                \
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
marlowe-cli run initialize --testnet-magic "$MAGIC"                  \
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

The mediator Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                   \
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
Fee: Lovelace 199513
Size: 758 / 16384 = 4%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `112e8bfeebe6dec09d408b65bc9dc337659609a52753af3fa0aca8dc74cc2951`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
112e8bfeebe6dec09d408b65bc9dc337659609a52753af3fa0aca8dc74cc2951     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "bd00c124963f7a78d2c5e18a68bd63bb9775bf8413eba9cfde0377f098810727"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
112e8bfeebe6dec09d408b65bc9dc337659609a52753af3fa0aca8dc74cc2951     0        110095187 lovelace + TxOutDatumNone
```

## Transaction 2. Buyer Deposits Funds into Seller's Account.

First we compute the Marlowe input required to make the initial deposit by the buyer.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "$SELLER_ROLE"      \
                        --deposit-party "$BUYER_ROLE"         \
                        --deposit-amount "$PRICE"             \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 499
```

Now the buyer Thomas Middleton submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                              \
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
Fee: Lovelace 1279428
Size: 14175 / 16384 = 86%
Execution units:
  Memory: 6236414 / 14000000 = 44%
  Steps: 1674754941 / 10000000000 = 16%
```

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `e487e497c82d3b0b4ec0659e79a8fb02c8402edd38e63fd5a18c12ff0614cefe`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e487e497c82d3b0b4ec0659e79a8fb02c8402edd38e63fd5a18c12ff0614cefe     1        259000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "4fdd1833c2eeea9bd070f056a7b92c4a75e99d9f3b9bfb107b81fe2befa2316a"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e487e497c82d3b0b4ec0659e79a8fb02c8402edd38e63fd5a18c12ff0614cefe     0        91545479 lovelace + TxOutDatumNone
e487e497c82d3b0b4ec0659e79a8fb02c8402edd38e63fd5a18c12ff0614cefe     2        3000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.544d + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --choice-name "Report problem"        \
                        --choice-party "$BUYER_ROLE"          \
                        --choice-number 1                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats
```

```console
Datum size: 377
Payment 1
  Acccount: "FB"
  Payee: Account "TM"
  Ada: Lovelace {getLovelace = 256000000}
```

Now the buyer Thomas Middleton can submit a transaction to report that there is a problem:

```
TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                              \
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
Fee: Lovelace 1355385
Size: 14030 / 16384 = 85%
Execution units:
  Memory: 7330052 / 14000000 = 52%
  Steps: 1941518520 / 10000000000 = 19%
```

The reporting of a problem was recorded in the transaction `383ba818017e4dacc0204d464ce42ce93c627ba6d2abb9136fe1893d1a5000ff`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
383ba818017e4dacc0204d464ce42ce93c627ba6d2abb9136fe1893d1a5000ff     1        259000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "20e59d7233f2c82226886f0852e38937ce6a167c4b3bfa2a6e641397746e55c0"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
383ba818017e4dacc0204d464ce42ce93c627ba6d2abb9136fe1893d1a5000ff     0        90190094 lovelace + TxOutDatumNone
383ba818017e4dacc0204d464ce42ce93c627ba6d2abb9136fe1893d1a5000ff     2        3000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.544d + TxOutDatumNone
```

## Transaction 4. The Seller Disputes that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe            \
                        --choice-name "Dispute problem"        \
                        --choice-party "$SELLER_ROLE"      \
                        --choice-number 0                      \
                        --invalid-before "$NOW"                \
                        --invalid-hereafter "$((NOW+4*HOUR))"  \
                        --out-file tx-4.marlowe                \
                        --print-stats
```

```console
Datum size: 298
```

Now the seller Francis Beaumont can submit a transaction to dispute that there is a problem:

```
TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                \
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
Fee: Lovelace 1234958
Size: 13830 / 16384 = 84%
Execution units:
  Memory: 5889426 / 14000000 = 42%
  Steps: 1546192625 / 10000000000 = 15%
```

The dispute that this is a problem is recorded in the transaction `f7313818664aebc13b83c188c0d17e14b4f72d05dd641c47a33090e1c67967db`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f7313818664aebc13b83c188c0d17e14b4f72d05dd641c47a33090e1c67967db     1        259000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "6eb02ebe66ff8164bf371c1711e5ecf47c8bdb07399828d566ea42fdfcab40ce"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f7313818664aebc13b83c188c0d17e14b4f72d05dd641c47a33090e1c67967db     0        47589949 lovelace + TxOutDatumNone
f7313818664aebc13b83c188c0d17e14b4f72d05dd641c47a33090e1c67967db     2        3000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.4642 + TxOutDatumNone
```

## Transaction 5. The Mediator Dismisses the Claim

Funds are released to the seller and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe           \
                        --choice-name "Dismiss claim"         \
                        --choice-party "$MEDIATOR_ROLE"       \
                        --choice-number 0                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-5.marlowe               \
                        --print-stats
```

```console
Datum size: 140
Payment 1
  Acccount: "TM"
  Payee: Account "FB"
  Ada: Lovelace {getLovelace = 256000000}
Payment 2
  Acccount: "CM"
  Payee: Party "CM"
  Ada: Lovelace {getLovelace = 3000000}
Payment 3
  Acccount: "FB"
  Payee: Party "FB"
  Ada: Lovelace {getLovelace = 256000000}
```

Now the mediator Christopher Marlowe can submit a transaction to release funds:

```
TX_5=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                    \
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
Fee: Lovelace 1328425
Size: 13603 / 16384 = 83%
Execution units:
  Memory: 7266182 / 14000000 = 51%
  Steps: 1879295446 / 10000000000 = 18%
```

The dismissal of the claim resulted in closing the contract, paying 256000000 lovelace to the role address for the benefit of the seller Francis Beaumont and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `49de0ba4339e520007aedc3c599da4688d0518d090290ec099e5da5f591f2bee`.  There is no UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
49de0ba4339e520007aedc3c599da4688d0518d090290ec099e5da5f591f2bee     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "348bdadb57c0c103002fa1ad6c0233cfede11350f382445b5a842a237b5b166b"
49de0ba4339e520007aedc3c599da4688d0518d090290ec099e5da5f591f2bee     2        256000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "a5552ad81e85d5160fc6d7c93119da716ed590d6b7357289f6e3b9746c88ebc9"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
49de0ba4339e520007aedc3c599da4688d0518d090290ec099e5da5f591f2bee     0        107766762 lovelace + TxOutDatumNone
49de0ba4339e520007aedc3c599da4688d0518d090290ec099e5da5f591f2bee     3        3000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.434d + TxOutDatumNone
```

## Transactions 6 and 7. Seller and Mediator Withdraw Funds.

The seller Francis Beaumont submits a transaction to withdraw the payment from the role address.

```
TX_6=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                \
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
Fee: Lovelace 436285
Size: 3083 / 16384 = 18%
Execution units:
  Memory: 1606962 / 14000000 = 11%
  Steps: 454555011 / 10000000000 = 4%
```

The mediator Christopher Marlowe submits a transaction to withdraw the minimum ADA from the role address.

```
TX_7=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                    \
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
Fee: Lovelace 436285
Size: 3083 / 16384 = 18%
Execution units:
  Memory: 1606962 / 14000000 = 11%
  Steps: 454555011 / 10000000000 = 4%
```

There is no UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the seller Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ac18d8ea7675e8432d598818fd606dd268ea6d216e028f557e6ad0d639a9cf59     0        47153664 lovelace + TxOutDatumNone
ac18d8ea7675e8432d598818fd606dd268ea6d216e028f557e6ad0d639a9cf59     1        256000000 lovelace + TxOutDatumNone
ac18d8ea7675e8432d598818fd606dd268ea6d216e028f557e6ad0d639a9cf59     2        3000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.4642 + TxOutDatumNone
```

Here are the UTxOs at the buyer Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
383ba818017e4dacc0204d464ce42ce93c627ba6d2abb9136fe1893d1a5000ff     0        90190094 lovelace + TxOutDatumNone
383ba818017e4dacc0204d464ce42ce93c627ba6d2abb9136fe1893d1a5000ff     2        3000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.544d + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2742c0bb540f68d9c48d6165f6fc6cc8a1e07a6af9098124e24198c329f3ffd3     0        107330477 lovelace + TxOutDatumNone
2742c0bb540f68d9c48d6165f6fc6cc8a1e07a6af9098124e24198c329f3ffd3     1        3000000 lovelace + TxOutDatumNone
2742c0bb540f68d9c48d6165f6fc6cc8a1e07a6af9098124e24198c329f3ffd3     2        3000000 lovelace + 1 e185925cd3f9043ce84591314b3a056c6bf50bbe12e74d25a9fad619.434d + TxOutDatumNone
```

## Clean Up

```
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
TX=$(
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$MEDIATOR_PAYMENT_SKEY"\
                       --change-address "$MEDIATOR_ADDRESS"      \
                       --out-file /dev/null                      \
                       --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                 \
)
marlowe-cli transaction simple --testnet-magic "$MAGIC"                   \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                               --tx-in "$TX#0"                              \
                               --tx-in "$TX#1"                              \
                               --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"         \
                               --tx-out "$BURN_ADDRESS+1400000+1 $MEDIATOR_TOKEN" \
                               --out-file /dev/null                       \
                               --submit 600
```

```console
TxId "7a4747b6af237d3801e4452383fee94c45d37e008637d2df82f91854d5ae485a"
```

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
TX=$(
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$SELLER_PAYMENT_SKEY"\
                       --change-address "$SELLER_ADDRESS"      \
                       --out-file /dev/null                      \
                       --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                 \
)
marlowe-cli transaction simple --testnet-magic "$MAGIC"                   \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                               --tx-in "$TX#0"                              \
                               --tx-in "$TX#1"                              \
                               --required-signer "$SELLER_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"         \
                               --tx-out "$BURN_ADDRESS+1400000+1 $SELLER_TOKEN" \
                               --out-file /dev/null                       \
                               --submit 600
```

```console
TxId "5da04a977ce1c54978f7d0e4d228a897978d9fd9dc756382f8de684d3352e9c1"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
TX=$(
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$BUYER_PAYMENT_SKEY"\
                       --change-address "$BUYER_ADDRESS"      \
                       --out-file /dev/null                      \
                       --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                 \
)
marlowe-cli transaction simple --testnet-magic "$MAGIC"                   \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                               --tx-in "$TX#0"                              \
                               --tx-in "$TX#1"                              \
                               --required-signer "$BUYER_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"         \
                               --tx-out "$BURN_ADDRESS+1400000+1 $BUYER_TOKEN" \
                               --out-file /dev/null                       \
                               --submit 600
```

```console
TxId "c3ec74494c777337ba9d8c38b3fa8459bfe551f6f92bf501e01a5832e650c7ee"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
