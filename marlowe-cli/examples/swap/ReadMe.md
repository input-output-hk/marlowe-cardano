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

The tip is at slot 4851045. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4851058. Tests may fail if this is not the case.

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
TxId "060a31c7323a3759230ab6b1331e9d51a2f65304209827f35d8531e199fbd5a8"
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
7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1     0        137557370 lovelace + TxOutDatumNone
7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1     1        2000000 lovelace + 500 357bc35cce53a1bd85fa37fe50c335da555c7855d285dd77cb564693.5377616e + TxOutDatumNone
7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1     2        2000000 lovelace + 300 56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b.476c6f6265 + TxOutDatumNone
7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1     3        2000000 lovelace + 300 6312ed95a755c99a22d4641899a70451ae2b893086f0cad446e294bb.476c6f6265 + TxOutDatumNone
7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1     4        2000000 lovelace + 300 7bd1eb01abaf6f825e20069db08d39f3335e8c623554164f6b859f00.476c6f6265 + TxOutDatumNone
7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1     5        2000000 lovelace + 1 830ce5deeb541779233daaf00502988fe29a929368726f309624f270.4a46 + TxOutDatumNone
7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1     6        2000000 lovelace + 1 830ce5deeb541779233daaf00502988fe29a929368726f309624f270.544d + TxOutDatumNone
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

John Fletcher will spend the UTxOs `7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1#0` and `7b0dcfe4fbaa54ecf8ce8243116af0d413b90e53f1d7e9cf4804b4b7d0dc17b1#2`. They will trade 300 of `56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b.Globe`.

### The Second Party

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
TxId "aa713e477dcc7e11955f287b7e89b752591527f1167035a885f5a8861f490918"
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
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     0        153518958 lovelace + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     1        2000000 lovelace + 500 2e50160f6887d1f424114ef6c85685d1b9ab3efa82f467b2d94f019d.5377616e + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     2        2000000 lovelace + 500 646f64b796c5564e9f11abb5c005e935cf6b07fcb590e0f1762e69a3.5377616e + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     3        2000000 lovelace + 500 75e8ef0d7cb7c95836f9e65d1319aea1ec6abfcfa88dcb6c11d5eed1.5377616e + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     4        2000000 lovelace + 500 7c11003f4c44d5635c3885cbc41148d5a482056254b435700ab08d15.5377616e + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     5        2000000 lovelace + 500 a5b114ba5ecc5e21c5f99b4d14e472248d6a9e99ab3d2b66e53371d6.5377616e + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     6        2000000 lovelace + 300 a63dc1279a2f473900c160157cc5e6776dc300bafe567af313780afe.476c6f6265 + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     7        2000000 lovelace + 500 b6cbc53a81b9b196b057f28b488a4aa92a70cd7b06f5f2bc75afa0fe.5377616e + TxOutDatumNone
fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d     8        2000000 lovelace + 300 ef40e94ee1fc579a19c9e5a8ec2a23792d347336e9554f2b20da3946.476c6f6265 + TxOutDatumNone
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

Thomas Kyd will spend the UTxOs `fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d#0` and `fda1a7c26a88eb5ab22b4e73b2891bddd946a735dfb827d1cc199ee3abaa0a1d#5`. They will trade 500 of `a5b114ba5ecc5e21c5f99b4d14e472248d6a9e99ab3d2b66e53371d6.Swan`.

## The Contract

```
MINIMUM_ADA=3000000
PARTY_A_TIMEOUT=$((NOW+12*HOUR))
PARTY_B_TIMEOUT=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party John Fletcher will swap 300 of `56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b.Globe` before Wed, 13 Apr 2022 04:24:45 +0000 for 500 of `a5b114ba5ecc5e21c5f99b4d14e472248d6a9e99ab3d2b66e53371d6.Swan` from the second party Thomas Kyd before Wed, 13 Apr 2022 16:24:45 +0000.

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

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `93b8aeae54c668a611dee751b8f6b47a9d5f63c0a729fe42d85018364bd28281`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
93b8aeae54c668a611dee751b8f6b47a9d5f63c0a729fe42d85018364bd28281     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "a245f3f00aec810d86c14cd0a5a1e9b6fcee4856086b64cb7d5927d1511b5553"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
93b8aeae54c668a611dee751b8f6b47a9d5f63c0a729fe42d85018364bd28281     0        134355173 lovelace + TxOutDatumNone
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

The contract received the deposit of 300 `56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b.Globe` in the transaction `8ddd1741e24f537ab01b791fd719475537b152b26b7d189eef1daac2f9067233`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8ddd1741e24f537ab01b791fd719475537b152b26b7d189eef1daac2f9067233     1        3000000 lovelace + 300 56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b.476c6f6265 + TxOutDatumHash ScriptDataInAlonzoEra "a8d1e692224027d66c702710be0f817d23cbd5a7718d2081d6c84ce7eaa6809b"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8ddd1741e24f537ab01b791fd719475537b152b26b7d189eef1daac2f9067233     0        135186330 lovelace + TxOutDatumNone
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
  56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b."Globe": 300
Payment 2
  Acccount: PK "c1700ac898b369122f484c94c598e84eae9d8ef53717918163e1d721"
  Payee: Party (PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf")
  Ada: 0.000000
  a5b114ba5ecc5e21c5f99b4d14e472248d6a9e99ab3d2b66e53371d6."Swan": 500
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

The closing of the contract paid 500 `a5b114ba5ecc5e21c5f99b4d14e472248d6a9e99ab3d2b66e53371d6.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 300 `56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b.Globe` to the second party Thomas Kyd in the transaction `cf834b514a0a189dd60931c6af332bc63f08b0af3bf2c18dfbe14c13ad201a6f`. There is no UTxO at the contract address:

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
8ddd1741e24f537ab01b791fd719475537b152b26b7d189eef1daac2f9067233     0        135186330 lovelace + TxOutDatumNone
cf834b514a0a189dd60931c6af332bc63f08b0af3bf2c18dfbe14c13ad201a6f     2        3000000 lovelace + 500 a5b114ba5ecc5e21c5f99b4d14e472248d6a9e99ab3d2b66e53371d6.5377616e + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cf834b514a0a189dd60931c6af332bc63f08b0af3bf2c18dfbe14c13ad201a6f     0        152917679 lovelace + TxOutDatumNone
cf834b514a0a189dd60931c6af332bc63f08b0af3bf2c18dfbe14c13ad201a6f     1        1344798 lovelace + 300 56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b.476c6f6265 + TxOutDatumNone
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
TxId "98acbe3bc2fb6d7d007e12a7564da04fde27124c7a32d27cb857d209b3eb2ce8"
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
TxId "2835e5bcb927da5aa2c900a3df8161ac1dd1784a596286f1ab426b0eb6d781f8"
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
PolicyID "56608c31ae3ea15c6a6d9c8e7d78037c0ea4b277f3d6669b177c066b"
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
PolicyID "a5b114ba5ecc5e21c5f99b4d14e472248d6a9e99ab3d2b66e53371d6"
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
TxId "dc7364a53f6a790e7d69d62e04c3d7581a16b144b70a0a01e1ba6589dd3f81e9"
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
TxId "ad03f069cd043589ea13bd2a0826e7d8c537c7da041ffd181f1f433b75b5c730"
