# Test of a Swap Contract

[This swap contract](../../src/Language/Marlowe/CLI/Examples/Swap.hs) swaps native tokens between two parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables `PARTY_A_PREFIX` and `PARTY_B_PREFIX` where they appear below.

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
```

The tip is at slot 4870986. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4870990. Tests may fail if this is not the case.

### Participants

#### The First Party

```
PARTY_A_PREFIX="$TREASURY/john-fletcher"
PARTY_A_NAME="John Fletcher"
PARTY_A_PAYMENT_SKEY="$PARTY_A_PREFIX".skey
PARTY_A_PAYMENT_VKEY="$PARTY_A_PREFIX".vkey
```

Create the first party's keys, if necessary.

```
if [[ ! -e "$PARTY_A_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_A_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_A_PAYMENT_VKEY"
fi
PARTY_A_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" )
PARTY_A_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY")
```

Fund the first party's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        "$PARTY_A_ADDRESS"
```

```console
TxId "fa0664bdfe3435b69040300546ec0fe76721ebf68272969b5e75661bb665ca4b"
```

The first party mints their tokens for the swap.

```
MINT_EXPIRES=$((TIP + 1000000))
TOKEN_NAME_A=Globe
AMOUNT_A=300
CURRENCY_SYMBOL_A=$(
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_A_PAYMENT_SKEY" \
                      --change-address  "$PARTY_A_ADDRESS"      \
                      --count "$AMOUNT_A"                       \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_A"                           \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
```

The first party John Fletcher is the minimum-ADA provider and has the address `addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r` and public-key hash `1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_A_PAYMENT_SKEY" \
                       --change-address "$PARTY_A_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a2c7229518b47e6b75b794c201ba26b6b2dbb54a42f007e1f4f04226936c6346     0        97651442 lovelace + TxOutDatumNone
a2c7229518b47e6b75b794c201ba26b6b2dbb54a42f007e1f4f04226936c6346     1        2000000 lovelace + 300 2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208.476c6f6265 + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_A_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_A_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_A"                   \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

John Fletcher will spend the UTxOs `a2c7229518b47e6b75b794c201ba26b6b2dbb54a42f007e1f4f04226936c6346#0` and `a2c7229518b47e6b75b794c201ba26b6b2dbb54a42f007e1f4f04226936c6346#1`. They will trade 300 of `2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208.Globe`.

#### The Second Party

```
PARTY_B_PREFIX="$TREASURY/thomas-kyd"
PARTY_B_NAME="Thomas Kyd"
PARTY_B_PAYMENT_SKEY="$PARTY_B_PREFIX".skey
PARTY_B_PAYMENT_VKEY="$PARTY_B_PREFIX".vkey
```

Create the second party's keys, if necessary.

```
if [[ ! -e "$PARTY_B_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_B_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_B_PAYMENT_VKEY"
fi
PARTY_B_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" )
PARTY_B_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY")
```

Fund the second party's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        "$PARTY_B_ADDRESS"
```

```console
TxId "cf937b84fefd599d48a1892f07e65221c5f9adfbd5bb832ba3e994794e3d99d4"
```

The second party mints their tokens for the swap.

```
TOKEN_NAME_B=Swan
AMOUNT_B=500
CURRENCY_SYMBOL_B=$(
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_B_PAYMENT_SKEY" \
                      --change-address  "$PARTY_B_ADDRESS"      \
                      --count "$AMOUNT_B"                       \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_B"                           \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
```

The second party Thomas Kyd has the address `addr_test1vrqhqzkgnzekjy30fpxff3vcap82a8vw75m30yvpv0sawgg0pqxf8` and public-key hash `c1700ac898b369122f484c94c598e84eae9d8ef53717918163e1d721`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_B_PAYMENT_SKEY" \
                       --change-address "$PARTY_B_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1d9ba64e488e38fcdbd40d05326fe7756ebaf07b0be5473c796889159688f9b6     0        97651574 lovelace + TxOutDatumNone
1d9ba64e488e38fcdbd40d05326fe7756ebaf07b0be5473c796889159688f9b6     1        2000000 lovelace + 500 f24dfc461978982682328cb843df50c0e0ecf18da4b6c7c4d2b397bf.5377616e + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_B_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_B_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_B"                   \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Kyd will spend the UTxOs `1d9ba64e488e38fcdbd40d05326fe7756ebaf07b0be5473c796889159688f9b6#0` and `1d9ba64e488e38fcdbd40d05326fe7756ebaf07b0be5473c796889159688f9b6#1`. They will trade 500 of `f24dfc461978982682328cb843df50c0e0ecf18da4b6c7c4d2b397bf.Swan`.

## The Contract

```
MINIMUM_ADA=3000000
PARTY_A_TIMEOUT=$((NOW+12*HOUR))
PARTY_B_TIMEOUT=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party John Fletcher will swap 300 of `2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208.Globe` before Wed, 13 Apr 2022 09:57:06 +0000 for 500 of `f24dfc461978982682328cb843df50c0e0ecf18da4b6c7c4d2b397bf.Swan` from the second party Thomas Kyd before Wed, 13 Apr 2022 21:57:06 +0000.

We create the contract for the previously specified parameters.

```
marlowe-cli template swap --minimum-ada "$MINIMUM_ADA"       \
                          --a-party "PK=$PARTY_A_PUBKEYHASH" \
                          --a-token "$TOKEN_A"               \
                          --a-amount "$AMOUNT_A"             \
                          --a-timeout "$PARTY_A_TIMEOUT"     \
                          --b-party "PK=$PARTY_B_PUBKEYHASH" \
                          --b-token "$TOKEN_B"               \
                          --b-amount "$AMOUNT_B"             \
                          --b-timeout "$PARTY_B_TIMEOUT"     \
                          --out-contract-file tx-1.contract  \
                          --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA.

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require.

```
marlowe-cli run initialize "${MAGIC[@]}"                 \
                           --slot-length "$SLOT_LENGTH"  \
                           --slot-offset "$SLOT_OFFSET"  \
                           --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.marlowe  \
                           --print-stats
```

```console
Validator size: 12350
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wrpwjj2eqlt7weufhmtphza92hsg3luz7f8gndlvza6d62srp373k`.

The first party John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_A_ADA"                     \
                        --required-signer "$PARTY_A_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe           \
                        --change-address "$PARTY_A_ADDRESS"       \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 202197
Size: 821 / 32768 = 2%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `ae625dab1b02858d4a8613bca98d7b1be7f2461baa173d300b70ab87d1acf2a6`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ae625dab1b02858d4a8613bca98d7b1be7f2461baa173d300b70ab87d1acf2a6     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f1dd9f4744e71f866ec66ea32621800642860f32a2848820087ced8083acb9c8"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ae625dab1b02858d4a8613bca98d7b1be7f2461baa173d300b70ab87d1acf2a6     0        94449245 lovelace + TxOutDatumNone
```

## Transaction 2. First Party Deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe                \
                        --deposit-account "PK=$PARTY_A_PUBKEYHASH" \
                        --deposit-party "PK=$PARTY_A_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_A"                 \
                        --deposit-amount "$AMOUNT_A"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-2.marlowe                    \
                        --print-stats
```

```console
Datum size: 538
```

Now the first party John Fletcher submits the transaction that deposits their tokens.

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_1"#0              \
                        --tx-in "$TX_1"#0                         \
                        --tx-in "$TX_0_A_TOKEN"                   \
                        --required-signer "$PARTY_A_PAYMENT_SKEY" \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$PARTY_A_ADDRESS"       \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1168843
Size: 14011 / 32768 = 42%
Execution units:
  Memory: 4536628 / 30000000 = 15%
  Steps: 1602585938 / 10000000000 = 16%
```

The contract received the deposit of 300 `2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208.Globe` in the transaction `edec833d9cc5ac14062db001615c25bb20542b533f51bb197e0ba63829cea1e0`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
edec833d9cc5ac14062db001615c25bb20542b533f51bb197e0ba63829cea1e0     1        3000000 lovelace + 300 2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208.476c6f6265 + TxOutDatumHash ScriptDataInAlonzoEra "0d4a5b58194a423f032b7061244433be865585b84908bbff29049e5bfce2d31e"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
edec833d9cc5ac14062db001615c25bb20542b533f51bb197e0ba63829cea1e0     0        95280402 lovelace + TxOutDatumNone
```

## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "PK=$PARTY_B_PUBKEYHASH" \
                        --deposit-party "PK=$PARTY_B_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_B"                 \
                        --deposit-amount "$AMOUNT_B"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-3.marlowe                    \
                        --print-stats
```

```console
Datum size: 23
Payment 1
  Acccount: PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf"
  Payee: Party (PK "c1700ac898b369122f484c94c598e84eae9d8ef53717918163e1d721")
  Ada: 0.000000
  2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208."Globe": 300
Payment 2
  Acccount: PK "c1700ac898b369122f484c94c598e84eae9d8ef53717918163e1d721"
  Payee: Party (PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf")
  Ada: 0.000000
  f24dfc461978982682328cb843df50c0e0ecf18da4b6c7c4d2b397bf."Swan": 500
Payment 3
  Acccount: PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf"
  Payee: Party (PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf")
  Ada: 3.000000
```

Now the second party Thomas Kyd can submit a transaction that deposits their tokens and completes the swap.

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_0_B_ADA"          \
                        --tx-in "$TX_0_B_ADA"                     \
                        --tx-in "$TX_0_B_TOKEN"                   \
                        --required-signer "$PARTY_B_PAYMENT_SKEY" \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$PARTY_B_ADDRESS"       \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1256481
Size: 13464 / 32768 = 41%
Execution units:
  Memory: 5958914 / 30000000 = 19%
  Steps: 2013682866 / 10000000000 = 20%
```

The closing of the contract paid 500 `f24dfc461978982682328cb843df50c0e0ecf18da4b6c7c4d2b397bf.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 300 `2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208.Globe` to the second party Thomas Kyd in the transaction `be2324f29159c61647b671016e6968f09b853abbc4172ea937e7dd161ea75d62`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
be2324f29159c61647b671016e6968f09b853abbc4172ea937e7dd161ea75d62     2        3000000 lovelace + 500 f24dfc461978982682328cb843df50c0e0ecf18da4b6c7c4d2b397bf.5377616e + TxOutDatumNone
edec833d9cc5ac14062db001615c25bb20542b533f51bb197e0ba63829cea1e0     0        95280402 lovelace + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
be2324f29159c61647b671016e6968f09b853abbc4172ea937e7dd161ea75d62     0        97050295 lovelace + TxOutDatumNone
be2324f29159c61647b671016e6968f09b853abbc4172ea937e7dd161ea75d62     1        1344798 lovelace + 300 2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208.476c6f6265 + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple "${MAGIC[@]}"                                          \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"              \
                               --tx-in "$TX_2"#0                                      \
                               --tx-in "$TX_3"#2                                      \
                               --tx-out "$PARTY_B_ADDRESS+1500000+$AMOUNT_B $TOKEN_B" \
                               --required-signer "$PARTY_A_PAYMENT_SKEY"              \
                               --change-address "$PARTY_A_ADDRESS"                    \
                               --out-file /dev/null                                   \
                               --submit 600
```

```console
TxId "56cc841c5daea07b11e660c3e51c9b097817670db358881feb4b888963aebb2e"
```

marlowe-cli transaction simple "${MAGIC[@]}"                                          \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"              \
                               --tx-in "$TX_3"#0                                      \
                               --tx-in "$TX_3"#1                                      \
                               --tx-out "$PARTY_A_ADDRESS+1500000+$AMOUNT_A $TOKEN_A" \
                               --required-signer "$PARTY_B_PAYMENT_SKEY"              \
                               --change-address "$PARTY_B_ADDRESS"                    \
                               --out-file /dev/null                                   \
                               --submit 600
```

```console
TxId "cc064edc424a2d42f9eaac9acd589c433e2818d7603556807e3ed08e7d598430"
marlowe-cli util mint "${MAGIC[@]}"                             \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_A_PAYMENT_SKEY" \
                      --change-address  "$PARTY_A_ADDRESS"      \
                      --count "-$AMOUNT_A"                      \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_A"
```

```console
PolicyID "2fcaab75a77530041ab38e1024d2784d255d63e3cc7afece2e759208"
marlowe-cli util mint "${MAGIC[@]}"                             \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$PARTY_B_PAYMENT_SKEY" \
                      --change-address  "$PARTY_B_ADDRESS"      \
                      --count "-$AMOUNT_B"                      \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_B"
```

```console
PolicyID "f24dfc461978982682328cb843df50c0e0ecf18da4b6c7c4d2b397bf"
TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$PARTY_A_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600
```

```console
TxId "16114e3e6058041786809096a6988955cfd3d72dc555c453f4ab656d58a4d3fa"
TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$PARTY_B_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600
```

```console
TxId "5882039eeba4fd5012527da0068d48ee2aecea0654c703cfe78ba64abea50159"
