# Test of a Swap Contract

[This swap contract](../../src/Language/Marlowe/CLI/Examples/Swap.hs) swaps native tokens between two parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed
* xargs

Signing and verification keys must be provided below for the two parties: to do this, set the environment variables `PARTY_A_PREFIX` and `PARTY_B_PREFIX` where they appear below.

The two parties' wallets much have exactly one UTxO with the token they want to swap and at least one UTxO without tokens.

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
  SLOT_OFFSET=1638215277000
fi
```

#### The First Party

```
PARTY_A_PREFIX="$TREASURY/john-fletcher"
PARTY_A_NAME="John Fletcher"
PARTY_A_PAYMENT_SKEY="$PARTY_A_PREFIX".skey
PARTY_A_PAYMENT_VKEY="$PARTY_A_PREFIX".vkey
PARTY_A_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                           \
                            --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" \
)
PARTY_A_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY"
)
```

The first party John Fletcher is the minimum-ADA provider and has the address `addr_test1vrsuucqupq5xz7dw0tnw7w3cyck5zyk2kpnr56xlhr57m3gll8m84` and public-key hash `e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5`. They have the following UTxOs in their wallet:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
97dce0221b65361e3c310fa4f1e78f2a6d9a74f5ac4e8295b3c7c3d4c58945c4     0        70693878 lovelace + TxOutDatumNone
97dce0221b65361e3c310fa4f1e78f2a6d9a74f5ac4e8295b3c7c3d4c58945c4     1        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with exactly one type of native token.

```
TX_0_A_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                           \
                       --address "$PARTY_A_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                  \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) ==1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                               \
                       --address "$PARTY_A_ADDRESS"                \
                       --out-file /dev/stdout                      \
| jq '. | to_entries | .[] | select((.value.value | length) == 2)' \
> utxo-0-a.json
TX_0_A_TOKEN=$(jq -r '.key' utxo-0-a.json)
CURRENCY_SYMBOL_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-a.json)
TOKEN_NAME_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-a.json)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
AMOUNT_A=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-a.json)
```

John Fletcher will spend the UTxOs `97dce0221b65361e3c310fa4f1e78f2a6d9a74f5ac4e8295b3c7c3d4c58945c4#0` and `97dce0221b65361e3c310fa4f1e78f2a6d9a74f5ac4e8295b3c7c3d4c58945c4#1`. They will trade 200 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan`.

### The Second Party

```
PARTY_B_PREFIX="$TREASURY/thomas-middleton"
PARTY_B_NAME="Thomas Middleton"
PARTY_B_PAYMENT_SKEY="$PARTY_B_PREFIX".skey
PARTY_B_PAYMENT_VKEY="$PARTY_B_PREFIX".vkey
PARTY_B_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                           \
                            --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" \
)
PARTY_B_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY"
)
```

The second party Thomas Middleton has the address `` and public-key hash `90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9`. They have the following UTxOs in their wallet:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d66daf4ca5e82ca1580ca81b9ad4cfd28b575f953a942a0c48e5b8540c2876f8     0        71495214 lovelace + TxOutDatumNone
d66daf4ca5e82ca1580ca81b9ad4cfd28b575f953a942a0c48e5b8540c2876f8     2        3000000 lovelace + 100 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with exactly one type of native token.

```
TX_0_B_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                           \
                       --address "$PARTY_B_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                  \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) ==1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                               \
                       --address "$PARTY_B_ADDRESS"                \
                       --out-file /dev/stdout                      \
| jq '. | to_entries | .[] | select((.value.value | length) == 2)' \
> utxo-0-b.json
TX_0_B_TOKEN=$(jq -r '.key' utxo-0-b.json)
CURRENCY_SYMBOL_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-b.json)
TOKEN_NAME_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-b.json)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
AMOUNT_B=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-b.json)
```

Thomas Middleton will spend the UTxOs `d66daf4ca5e82ca1580ca81b9ad4cfd28b575f953a942a0c48e5b8540c2876f8#0` and `d66daf4ca5e82ca1580ca81b9ad4cfd28b575f953a942a0c48e5b8540c2876f8#2`. They will trade 100 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe`.

### Validator Script and Address

The contract has a validator script and address. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

```
CONTRACT_ADDRESS=$(
marlowe-cli export-address "${MAGIC[@]}" \
            --slot-length "$SLOT_LENGTH" \
            --slot-offset "$SLOT_OFFSET" \
)
marlowe-cli export-validator "${MAGIC[@]}"                \
                             --slot-length "$SLOT_LENGTH" \
                             --slot-offset "$SLOT_OFFSET" \
                             --out-file swap.plutus
```

```console
addr_test1wq3n0x7khejv6r0k3rzjw3tfwjjxlmvc7e5zxa8z4jwc57ca4jg8m
```

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 1988360. The current POSIX time implies that the tip of the blockchain should be slightly before slot 1988398. Tests may fail if this is not the case.

## The Contract

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party John Fletcher will swap 200 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` for 100 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` from the second party Thomas Middleton.

```
MINIMUM_ADA=3000000
PARTY_A_TIMEOUT=$(($TIP+12*3600))
PARTY_B_TIMEOUT=$(($TIP+24*3600))
```

## Transaction 1. Create the Contract by Providing the Minimum ADA.

We create the contract for the previously specified parameters.

```
marlowe-cli contract-swap --minimum-ada "$MINIMUM_ADA"       \
                          --a-party "PK=$PARTY_A_PUBKEYHASH" \
                          --a-token "$TOKEN_A"               \
                          --a-amount "$AMOUNT_A"             \
                          --a-timeout "$PARTY_A_TIMEOUT"     \
                          --b-party "PK=$PARTY_B_PUBKEYHASH" \
                          --b-token "$TOKEN_B"               \
                          --b-amount "$AMOUNT_B"             \
                          --b-timeout "$PARTY_B_TIMEOUT"     \
                          --out-file tx-1.marlowe
```

We extract the initial state and full contract from the `.marlowe`file that contains comprehensive information.

```
jq '.marloweState'    tx-1.marlowe > tx-1.state
jq '.marloweContract' tx-1.marlowe > tx-1.contract
```

For each transaction, we construct the output datum. Here is its size and hash:

```
marlowe-cli export-datum --contract-file tx-1.contract \
                         --state-file    tx-1.state    \
                         --out-file      tx-1.datum
```

```console
be57a41c791cb0936bf951293d94ec79a180ad4eca3b4a99f6a8638f66d54a75
```

The first party John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace requiredd for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli transaction-create "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --script-address "$CONTRACT_ADDRESS"      \
                               --tx-in "$TX_0_A_ADA"                     \
                               --required-signer "$PARTY_A_PAYMENT_SKEY" \
                               --tx-out-datum-file tx-1.datum            \
                               --tx-out-marlowe "$MINIMUM_ADA"           \
                               --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA"  \
                               --change-address "$PARTY_A_ADDRESS"       \
                               --out-file tx-1.raw                       \
                               --print-stats                             \
                               --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                         \
)
```

```console
Fee: Lovelace 197533
Size: 816 / 32768 = 2%
Execution units:
  Memory: 0 / 10000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `aa803311dd741c892f8529a2c54b80def5a5a3723b1ffd6e4ef82dbf14475c55`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
aa803311dd741c892f8529a2c54b80def5a5a3723b1ffd6e4ef82dbf14475c55     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "be57a41c791cb0936bf951293d94ec79a180ad4eca3b4a99f6a8638f66d54a75"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
aa803311dd741c892f8529a2c54b80def5a5a3723b1ffd6e4ef82dbf14475c55     0        64496345 lovelace + TxOutDatumNone
aa803311dd741c892f8529a2c54b80def5a5a3723b1ffd6e4ef82dbf14475c55     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 2. First Party Deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli input-deposit --deposit-account "PK=$PARTY_A_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_A_PUBKEYHASH"   \
                          --deposit-token "$TOKEN_A"                 \
                          --deposit-amount "$AMOUNT_A"               \
                          --out-file tx-2.input
```

Next we compute the transition caused by that input to the contract.

```
marlowe-cli compute --contract-file tx-1.contract           \
                    --state-file    tx-1.state              \
                    --input-file    tx-2.input              \
                    --invalid-before "$TIP"                 \
                    --invalid-hereafter "$(($TIP+4*3600))"  \
                    --out-file tx-2.marlowe                 \
                    --print-stats
```

```console
Datum size: 526
```

As in the first transaction, we compute the new state and contract.

```
jq '.state'    tx-2.marlowe > tx-2.state
jq '.contract' tx-2.marlowe > tx-2.contract
```

Because this transaction spends from the script address, it also needs a redeemer:

```
marlowe-cli export-redeemer --input-file tx-2.input    \
                            --out-file   tx-2.redeemer
```

As in the first transaction, we compute the datum and its hash:

```
marlowe-cli export-datum --contract-file tx-2.contract \
                         --state-file    tx-2.state    \
                         --out-file      tx-2.datum
```

```console
19adede6e09547fc31b4dd4562958dfce1873606d12bb2280806247fb445ff07
```

Now the first party John Fletcher submits the transaction that deposits their tokens.

```
TX_2=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                                      \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH"          \
                                --script-address "$CONTRACT_ADDRESS"               \
                                --tx-in-marlowe "$TX_1"#1                          \
                                --tx-in-script-file swap.plutus                    \
                                --tx-in-datum-file tx-1.datum                      \
                                --tx-in-redeemer-file tx-2.redeemer                \
                                --tx-in "$TX_0_A_TOKEN"                            \
                                --tx-in "$TX_1"#0                                  \
                                --tx-in "$TX_1"#2                                  \
                                --tx-in-collateral "$TX_1"#0                       \
                                --required-signer "$PARTY_A_PAYMENT_SKEY"          \
                                --tx-out-marlowe "$MINIMUM_ADA+$AMOUNT_A $TOKEN_A" \
                                --tx-out-datum-file tx-2.datum                     \
                                --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA"           \
                                --change-address "$PARTY_A_ADDRESS"                \
                                --invalid-before "$TIP"                            \
                                --invalid-hereafter "$(($TIP+4*3600))"             \
                                --out-file tx-2.raw                                \
                                --print-stats                                      \
                                --submit=600                                       \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                   \
)
```

```console
Fee: Lovelace 1201453
Size: 15550 / 32768 = 47%
Execution units:
  Memory: 4021822 / 10000000 = 40%
  Steps: 1466029531 / 10000000000 = 14%
```

The contract received the deposit of 200 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` in the transaction `d719d63dea30ed9f875ded6110895c038ed378c0015165e02ac8c8c32a47b1cd`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d719d63dea30ed9f875ded6110895c038ed378c0015165e02ac8c8c32a47b1cd     1        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumHash ScriptDataInAlonzoEra "19adede6e09547fc31b4dd4562958dfce1873606d12bb2280806247fb445ff07"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d719d63dea30ed9f875ded6110895c038ed378c0015165e02ac8c8c32a47b1cd     0        66294892 lovelace + TxOutDatumNone
d719d63dea30ed9f875ded6110895c038ed378c0015165e02ac8c8c32a47b1cd     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap.

First we compute the input for the contract to transition forward.

```
marlowe-cli input-deposit --deposit-account "PK=$PARTY_B_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_B_PUBKEYHASH"   \
                          --deposit-token "$TOKEN_B"                 \
                          --deposit-amount "$AMOUNT_B"               \
                          --out-file tx-3.input
```

As in the second transaction we compute the contract's transition, its new state, and the redeemer. Because the contract is being closed, no new datum need be computed.

```
marlowe-cli compute --contract-file tx-2.contract          \
                    --state-file    tx-2.state             \
                    --input-file    tx-3.input             \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --out-file tx-3.marlowe                \
                    --print-stats
```

```console
Payment 1
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9")
  Ada: 0.000000
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Swan": 200
Payment 2
  Acccount: PK "90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
  Ada: 0.000000
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Globe": 100
Payment 3
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
  Ada: 3.000000
Datum size: 19
```

```
jq '.state'    tx-3.marlowe > tx-3.state
jq '.contract' tx-3.marlowe > tx-3.contract
marlowe-cli export-redeemer --input-file tx-3.input    \
                            --out-file   tx-3.redeemer
marlowe-cli export-datum --contract-file tx-3.contract \
                         --state-file    tx-3.state    \
                         --out-file      tx-3.datum
```

```console
a0e282b633757927bb23a5ea12f5ccea859e397728492d6b9735ae2f8a60be2f
```

Now the second party Thomas Middleton can submit a transaction that deposits their tokens and completes the swap.

```
TX_3=$(
marlowe-cli transaction-close "${MAGIC[@]}"                                               \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                              --tx-in-marlowe "$TX_2"#1                                   \
                              --tx-in-script-file swap.plutus                             \
                              --tx-in-datum-file tx-2.datum                               \
                              --tx-in-redeemer-file tx-3.redeemer                         \
                              --tx-in "$TX_0_B_ADA"                                       \
                              --tx-in "$TX_0_B_TOKEN"                                     \
                              --required-signer "$PARTY_B_PAYMENT_SKEY"                   \
                              --tx-in-collateral "$TX_0_B_ADA"                            \
                              --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA+$AMOUNT_B $TOKEN_B" \
                              --tx-out "$PARTY_B_ADDRESS+$MINIMUM_ADA+$AMOUNT_A $TOKEN_A" \
                              --change-address "$PARTY_B_ADDRESS"                         \
                              --invalid-before "$TIP"                                     \
                              --invalid-hereafter "$(($TIP+4*3600))"                      \
                              --out-file tx-3.raw                                         \
                              --print-stats                                               \
                              --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                          \
)
```

```console
Fee: Lovelace 1378258
Size: 14944 / 32768 = 45%
Execution units:
  Memory: 6675614 / 10000000 = 66%
  Steps: 2225927891 / 10000000000 = 22%
```

The closing of the contract paid 100 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 200 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` to the second party Thomas Middleton in the transaction `e596a6e8eac5f9bbbdef67d5377f650035ba62937b52f3d3a5bb059bf1b93430`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d719d63dea30ed9f875ded6110895c038ed378c0015165e02ac8c8c32a47b1cd     0        66294892 lovelace + TxOutDatumNone
d719d63dea30ed9f875ded6110895c038ed378c0015165e02ac8c8c32a47b1cd     2        3000000 lovelace + TxOutDatumNone
e596a6e8eac5f9bbbdef67d5377f650035ba62937b52f3d3a5bb059bf1b93430     1        3000000 lovelace + 100 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e596a6e8eac5f9bbbdef67d5377f650035ba62937b52f3d3a5bb059bf1b93430     0        70116956 lovelace + TxOutDatumNone
e596a6e8eac5f9bbbdef67d5377f650035ba62937b52f3d3a5bb059bf1b93430     2        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
```

## Clean Up

It's convenient to consolidate the UTxOs for the first party.

```
marlowe-cli transaction-simple "${MAGIC[@]}"                                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                               --tx-in "$TX_2"#0                                           \
                               --tx-in "$TX_2"#2                                           \
                               --tx-in "$TX_3"#1                                           \
                               --required-signer "$PARTY_A_PAYMENT_SKEY"                   \
                               --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA+$AMOUNT_B $TOKEN_B" \
                               --change-address "$PARTY_A_ADDRESS"                         \
                               --out-file tx-4.raw                                         \
                               --submit=600
```

