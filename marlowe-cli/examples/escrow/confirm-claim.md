# Example Escrow Contract: "Confirm Claim"

In this example execution of [an escrow contract](ReadMe.md), the buyer reports a problem, the seller disputes the problem, but the mediator confirms the problem.

![Flow chart for "confirm claim".](confirm-claim.svg)

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

The tip is at slot 3418686. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3418691. Tests may fail if this is not the case.

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
TxId "721e4a517dd203e072847bb79b5f21a5af162d537a10aeb53ba0431f4e4c2982"
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
TxId "6953366f0f8e017b44580e5f346639524cde3fcad983e7051b3f9207724e9664"
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
TxId "775c7b603c2d1d23d4c0bb287ff3b46938bab1c276433b2bc4046d9317f2687b"
```

### Role Tokens

The mediator mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                      \
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
TxId "3caa9e86a780a4b803c12365370752f1c834a025e5e3dad16721b082c5f0ca6a"
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
TxId "0f8c28e09e9fda2573301e5c9604615c684ca5b759951bd68fa43d4c91936cf3"
```

### Available UTxOs

The mediator Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz` and role token named `CM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                   \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                       --change-address "$MEDIATOR_ADDRESS"       \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e68ca19e03f757b314c86200bcbe5d8b28e23e2cc2fe3094757054ac4f3563a1     0        113294700 lovelace + TxOutDatumNone
e68ca19e03f757b314c86200bcbe5d8b28e23e2cc2fe3094757054ac4f3563a1     1        2000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.434d + TxOutDatumNone
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

Christopher Marlowe will spend the UTxOs `e68ca19e03f757b314c86200bcbe5d8b28e23e2cc2fe3094757054ac4f3563a1#0` and `e68ca19e03f757b314c86200bcbe5d8b28e23e2cc2fe3094757054ac4f3563a1#1`.

The seller Francis Beaumont has the address `addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q` and role token named `FB`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                   \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$SELLER_PAYMENT_SKEY"   \
                       --change-address "$SELLER_ADDRESS"         \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
77a2bfe46d0be25012ed3c314a398d0552cef9c54fd3b577822fe8d1ab3a6733     0        49824907 lovelace + TxOutDatumNone
77a2bfe46d0be25012ed3c314a398d0552cef9c54fd3b577822fe8d1ab3a6733     1        2000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.4642 + TxOutDatumNone
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

Francis Beaumont will spend the UTxOs `77a2bfe46d0be25012ed3c314a398d0552cef9c54fd3b577822fe8d1ab3a6733#0` and `77a2bfe46d0be25012ed3c314a398d0552cef9c54fd3b577822fe8d1ab3a6733#1`.

The buyer Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `TM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                   \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$BUYER_PAYMENT_SKEY"    \
                       --change-address "$BUYER_ADDRESS"          \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f16ccd007a2278784a7e95d90eb379fa10697082af8cb2b5c30fbbd53548b702     0        349824907 lovelace + TxOutDatumNone
f16ccd007a2278784a7e95d90eb379fa10697082af8cb2b5c30fbbd53548b702     1        2000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.544d + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `f16ccd007a2278784a7e95d90eb379fa10697082af8cb2b5c30fbbd53548b702#0` and `f16ccd007a2278784a7e95d90eb379fa10697082af8cb2b5c30fbbd53548b702#1`.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((NOW+1*24*HOUR))
COMPLAINT_DEADLINE=$((NOW+2*24*HOUR))
DISPUTE_DEADLINE=$((NOW+3*24*HOUR))
MEDIATION_DEADLINE=$((NOW+4*24*HOUR))
```

* The current slot is 3418686.
* The buyer Thomas Middleton must pay before Sun, 18 Sep 2022 13:38:06 +0000.
* They buyer Thomas Middleton has until Mon, 19 Sep 2022 13:38:06 +0000 to complain.
* The seller Francis Beaumont has until Tue, 20 Sep 2022 13:38:06 +0000 to dispute a complaint.
* The mediator Christopher Marlowe has until Wed, 21 Sep 2022 13:38:06 +0000 to decide on a disputed complaint.

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

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `8dcaf80d610b0f2e8dd2ecc9c90168ea794aadc571d63ba67469bae4e9c6fd37`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8dcaf80d610b0f2e8dd2ecc9c90168ea794aadc571d63ba67469bae4e9c6fd37     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "2c0cb399cc1f1633222794202162f74da3b2dcc0e88b729b18e7a2c1cb3c6ca3"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8dcaf80d610b0f2e8dd2ecc9c90168ea794aadc571d63ba67469bae4e9c6fd37     0        110095187 lovelace + TxOutDatumNone
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

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `87ae6aeafda245a4053df53a72a5b19ca6adbbeee22d35927a10b25b06c737a5`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
87ae6aeafda245a4053df53a72a5b19ca6adbbeee22d35927a10b25b06c737a5     1        259000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "b11c5f4c281bae225ac753eaa45d31c0cf1a6b132be8553a8f46d9db3c8e60ad"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
87ae6aeafda245a4053df53a72a5b19ca6adbbeee22d35927a10b25b06c737a5     0        91545479 lovelace + TxOutDatumNone
87ae6aeafda245a4053df53a72a5b19ca6adbbeee22d35927a10b25b06c737a5     2        3000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.544d + TxOutDatumNone
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

The reporting of a problem was recorded in the transaction `fd26187099478755e3ab62e54e7e23bea5388cc2c6d435c5c30e2118c9cce0cb`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fd26187099478755e3ab62e54e7e23bea5388cc2c6d435c5c30e2118c9cce0cb     1        259000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "54b3ce59db3029e183e8c46629d12ca6e5012b2135ce9228af605ae6d9c5973c"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fd26187099478755e3ab62e54e7e23bea5388cc2c6d435c5c30e2118c9cce0cb     0        90190094 lovelace + TxOutDatumNone
fd26187099478755e3ab62e54e7e23bea5388cc2c6d435c5c30e2118c9cce0cb     2        3000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.544d + TxOutDatumNone
```

## Transaction 4. The Seller Disputes that There is a Problem

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe            \
                        --choice-name "Dispute problem"        \
                        --choice-party "$SELLER_ROLE"          \
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
Fee: Lovelace 1230866
Size: 13830 / 16384 = 84%
Execution units:
  Memory: 5834226 / 14000000 = 41%
  Steps: 1533615704 / 10000000000 = 15%
```

The dispute that this is a problem is recorded in the transaction `f60a2eef1c688c2dafebfbac459936476cfa1619c3db426bc4a768ece9616ee0`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f60a2eef1c688c2dafebfbac459936476cfa1619c3db426bc4a768ece9616ee0     1        259000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "cc5eeff7b5da5da327781927d29c862075b9660fc938063d39ffabae46dbe5c9"
```

Here is the UTxO at the seller Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f60a2eef1c688c2dafebfbac459936476cfa1619c3db426bc4a768ece9616ee0     0        47594041 lovelace + TxOutDatumNone
f60a2eef1c688c2dafebfbac459936476cfa1619c3db426bc4a768ece9616ee0     2        3000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.4642 + TxOutDatumNone
```

## Transaction 5. The Mediator Confirms the Claim

Funds are released to the buyer and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe           \
                        --choice-name "Confirm claim"         \
                        --choice-party "$MEDIATOR_ROLE"       \
                        --choice-number 1                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-5.marlowe               \
                        --print-stats
```

```console
Datum size: 140
Payment 1
  Acccount: "CM"
  Payee: Party "CM"
  Ada: Lovelace {getLovelace = 3000000}
Payment 2
  Acccount: "TM"
  Payee: Party "TM"
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
Fee: Lovelace 1244490
Size: 13603 / 16384 = 83%
Execution units:
  Memory: 6163390 / 14000000 = 44%
  Steps: 1597679188 / 10000000000 = 15%
```

The confirmation of the claim resulted in closing the contract, paying 256000000 lovelace to the role address for the benefit of the buyer Thomas Middleton and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `a1c760eff6e1d4a0fcbd536fcfaea9643547c0bb2803bd2fb7f9900e161b5c2f`.  There is no UTxO at the contract address:

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
a1c760eff6e1d4a0fcbd536fcfaea9643547c0bb2803bd2fb7f9900e161b5c2f     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "18a86fdeb37e4e0536f813df644852f884ab12d2f77c315ac633d5734507c8a9"
a1c760eff6e1d4a0fcbd536fcfaea9643547c0bb2803bd2fb7f9900e161b5c2f     2        256000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "04b267252694c2b496b1bf36d3238943a3862e2ddc31a954a7091603000e1d8e"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a1c760eff6e1d4a0fcbd536fcfaea9643547c0bb2803bd2fb7f9900e161b5c2f     0        107850697 lovelace + TxOutDatumNone
a1c760eff6e1d4a0fcbd536fcfaea9643547c0bb2803bd2fb7f9900e161b5c2f     3        3000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.434d + TxOutDatumNone
```

## Transactions 6 and 7. Buyer and Mediator Withdraw Funds.

The buyer Thomas Middleton submits a transaction to withdraw the repayment from the role address.

```
TX_6=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                              \
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
f60a2eef1c688c2dafebfbac459936476cfa1619c3db426bc4a768ece9616ee0     0        47594041 lovelace + TxOutDatumNone
f60a2eef1c688c2dafebfbac459936476cfa1619c3db426bc4a768ece9616ee0     2        3000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.4642 + TxOutDatumNone
```

Here are the UTxOs at the buyer Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b2d1abd853d733ad21fc2ffa584afcd153c2b239722f84352f30fc632a291004     0        89753809 lovelace + TxOutDatumNone
b2d1abd853d733ad21fc2ffa584afcd153c2b239722f84352f30fc632a291004     1        256000000 lovelace + TxOutDatumNone
b2d1abd853d733ad21fc2ffa584afcd153c2b239722f84352f30fc632a291004     2        3000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.544d + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p;/$TX_7/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
003e035e158ab6c715f80991723530182cd267e02bdbd7898d07a574dcd6b833     0        107414412 lovelace + TxOutDatumNone
003e035e158ab6c715f80991723530182cd267e02bdbd7898d07a574dcd6b833     1        3000000 lovelace + TxOutDatumNone
003e035e158ab6c715f80991723530182cd267e02bdbd7898d07a574dcd6b833     2        3000000 lovelace + 1 5f4033d2a7124e1b7d200f1632d38f192be69f26ee384606cc6de405.434d + TxOutDatumNone
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
TxId "51974f53d08f560bf0c2fa229eda16b901972677c5610b2b45e1b44f92f9d71c"
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
TxId "a259603143ca9ef8f792521706eb58e647d107bbfd4b87126c5b94201d98e44a"
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
TxId "3318c46c21b2b8b9591cff20cd563243df2e0031c4cd99584938bd137c54c5c5"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BUYER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
