# Test of a Swap Contract

[This swap contract](../../src/Language/Marlowe/CLI/Examples/Swap.hs) swaps native tokens between two parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.
See below for how to set `MAGIC` to select the network.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables `PARTY_A_PREFIX` and `PARTY_B_PREFIX` where they appear below.

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
```

The tip is at slot 32073. The current POSIX time implies that the tip of the blockchain should be slightly before slot 32074. Tests may fail if this is not the case.

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
TxId "9481ad3c3b3548ffb455cd4dfd7bd1c7631278095b28de564b53bee53d770717"
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
786cd7b4729784ae145107f79cbead056ee3c21c1ed41d1397b2ca7e40521d02     0        97651442 lovelace + TxOutDatumNone
786cd7b4729784ae145107f79cbead056ee3c21c1ed41d1397b2ca7e40521d02     1        2000000 lovelace + 300 1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0.476c6f6265 + TxOutDatumNone
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

John Fletcher will spend the UTxOs `786cd7b4729784ae145107f79cbead056ee3c21c1ed41d1397b2ca7e40521d02#0` and `786cd7b4729784ae145107f79cbead056ee3c21c1ed41d1397b2ca7e40521d02#1`. They will trade 300 of `1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0.Globe`.

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
TxId "211b6cb708d3763055b3492dd167d83ac420c184feac81662ae000aadf999abc"
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

The second party Thomas Kyd has the address `addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5` and public-key hash `fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff`. They have the following UTxOs in their wallet:

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
f497bc31041eaac13d6457ad5f0f71428fb495f075eb70e6cd6c6c9017651ba4     0        97651574 lovelace + TxOutDatumNone
f497bc31041eaac13d6457ad5f0f71428fb495f075eb70e6cd6c6c9017651ba4     1        2000000 lovelace + 500 838ee2d1fa88224cd31f23db2c8a5f7de6b8bfe83be7bcbf6ba05d4f.5377616e + TxOutDatumNone
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

Thomas Kyd will spend the UTxOs `f497bc31041eaac13d6457ad5f0f71428fb495f075eb70e6cd6c6c9017651ba4#0` and `f497bc31041eaac13d6457ad5f0f71428fb495f075eb70e6cd6c6c9017651ba4#1`. They will trade 500 of `838ee2d1fa88224cd31f23db2c8a5f7de6b8bfe83be7bcbf6ba05d4f.Swan`.

## The Contract

```
MINIMUM_ADA=3000000
PARTY_A_TIMEOUT=$((NOW+12*HOUR))
PARTY_B_TIMEOUT=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party John Fletcher will swap 300 of `1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0.Globe` before Fri, 15 Apr 2022 12:15:04 +0000 for 500 of `838ee2d1fa88224cd31f23db2c8a5f7de6b8bfe83be7bcbf6ba05d4f.Swan` from the second party Thomas Kyd before Sat, 16 Apr 2022 00:15:04 +0000.

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
marlowe-cli run initialize "${MAGIC[@]}"                             \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --print-stats
```

```console
Validator size: 12604
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24920101, exBudgetMemory = ExMemory 83800}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wp20ushgkk9nsfdcetcxpac8dgx09q08yhtdskptacvkcxsz2qjzl`.

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

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `a2450396612d97c889064412e2a3e2cda879acdbd5a6f3aa206d6554e5bea09c`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a2450396612d97c889064412e2a3e2cda879acdbd5a6f3aa206d6554e5bea09c     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "ce8f1d23e6f43a6d657c845bc401fd130bc07576087d965008402d313728d396"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a2450396612d97c889064412e2a3e2cda879acdbd5a6f3aa206d6554e5bea09c     0        94449245 lovelace + TxOutDatumNone
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
Fee: Lovelace 1180033
Size: 14261 / 32768 = 43%
Execution units:
  Memory: 4539028 / 30000000 = 15%
  Steps: 1603300490 / 10000000000 = 16%
```

The contract received the deposit of 300 `1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0.Globe` in the transaction `fccffa3d36d753089b16c5390893ba32307c783916063554f547b52e10dac52c`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fccffa3d36d753089b16c5390893ba32307c783916063554f547b52e10dac52c     1        3000000 lovelace + 300 1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0.476c6f6265 + TxOutDatumHash ScriptDataInAlonzoEra "54a4927cdbc0c8fd98a1dd779e0ccd56846ae475c0c533de02cc293b3865b8d2"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fccffa3d36d753089b16c5390893ba32307c783916063554f547b52e10dac52c     0        95269212 lovelace + TxOutDatumNone
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
  Payee: Party (PK "fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff")
  Ada: 0.000000
  1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0."Globe": 300
Payment 2
  Acccount: PK "fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff"
  Payee: Party (PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf")
  Ada: 0.000000
  838ee2d1fa88224cd31f23db2c8a5f7de6b8bfe83be7bcbf6ba05d4f."Swan": 500
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
Fee: Lovelace 1267592
Size: 13714 / 32768 = 41%
Execution units:
  Memory: 5960314 / 30000000 = 19%
  Steps: 2014099688 / 10000000000 = 20%
```

The closing of the contract paid 500 `838ee2d1fa88224cd31f23db2c8a5f7de6b8bfe83be7bcbf6ba05d4f.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 300 `1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0.Globe` to the second party Thomas Kyd in the transaction `3e2c1b1f86a9530d7e78b51eea306082160122be2715da0d8be604797364e002`. There is no UTxO at the contract address:

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
3e2c1b1f86a9530d7e78b51eea306082160122be2715da0d8be604797364e002     2        3000000 lovelace + 500 838ee2d1fa88224cd31f23db2c8a5f7de6b8bfe83be7bcbf6ba05d4f.5377616e + TxOutDatumNone
fccffa3d36d753089b16c5390893ba32307c783916063554f547b52e10dac52c     0        95269212 lovelace + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3e2c1b1f86a9530d7e78b51eea306082160122be2715da0d8be604797364e002     0        97039184 lovelace + TxOutDatumNone
3e2c1b1f86a9530d7e78b51eea306082160122be2715da0d8be604797364e002     1        1344798 lovelace + 300 1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0.476c6f6265 + TxOutDatumNone
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
TxId "456d79682bc1613817ed0740f320eef959afacb425c8e7a07bf44cc9fb482921"
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
TxId "f0e2aca7d87be67d392d871ada3970168d4c38af4583efc56d3e7fa27690e06c"
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
PolicyID "1588565b98bd53b52fd92afc829b50d85f61021ca7229f3d1f8a71f0"
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
PolicyID "838ee2d1fa88224cd31f23db2c8a5f7de6b8bfe83be7bcbf6ba05d4f"
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
TxId "11942ea146de097b9b37dc7745ee8ef25388b4e297c49f38e4345dac3a649975"
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
TxId "fd6681df1689ec3824941a4e26d941ab95cd8b93b15f4129d98b288548e3da26"
