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

The tip is at slot 3421270. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3421273. Tests may fail if this is not the case.

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
PARTY_A_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_A_PAYMENT_VKEY" )
```

Fund the first party's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$PARTY_A_ADDRESS"
```

```console
TxId "a150a263fa7f2db916f1a018e7000035eb0142a52649a2eb128d71f738bbf859"
```

The first party mints their tokens for the swap.

```
MINT_EXPIRES=$((TIP + 1000000))
TOKEN_NAME_A=Globe
AMOUNT_A=300
CURRENCY_SYMBOL_A=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                  \
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

The first party John Fletcher is the minimum-ADA provider and has the address `addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_A_PAYMENT_SKEY" \
                       --change-address "$PARTY_A_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0487d696be04374cc3c84c7d7d04393be96e2b20e0115188037f4e145cf21ac6     0        97651266 lovelace + TxOutDatumNone
0487d696be04374cc3c84c7d7d04393be96e2b20e0115188037f4e145cf21ac6     1        2000000 lovelace + 300 20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd.476c6f6265 + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_A_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_A_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_A"                   \
                        "$PARTY_A_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

John Fletcher will spend the UTxOs `0487d696be04374cc3c84c7d7d04393be96e2b20e0115188037f4e145cf21ac6#0` and `0487d696be04374cc3c84c7d7d04393be96e2b20e0115188037f4e145cf21ac6#1`. They will trade 300 of `20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd.Globe`.

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
PARTY_B_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$PARTY_B_PAYMENT_VKEY" )
```

Fund the second party's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 100000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$PARTY_B_ADDRESS"
```

```console
TxId "76e693b7e556a163bffdebc335e5ca48a89cabca2d8c86d1ef7e16e64feb249c"
```

The second party mints their tokens for the swap.

```
TOKEN_NAME_B=Swan
AMOUNT_B=500
CURRENCY_SYMBOL_B=$(
marlowe-cli util mint --testnet-magic "$MAGIC" \
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

The second party Thomas Kyd has the address `addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_B_PAYMENT_SKEY" \
                       --change-address "$PARTY_B_ADDRESS"       \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_B_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9987dcb6253eac1a7b7299374c57cafaaaf28d11fc5b04236d85ae85ea4f9bfb     0        97651398 lovelace + TxOutDatumNone
9987dcb6253eac1a7b7299374c57cafaaaf28d11fc5b04236d85ae85ea4f9bfb     1        2000000 lovelace + 500 4d4fbba15d36506985cbd9bc98e53a8aa71820d0ff828fa4e99c76dc.5377616e + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_B_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_B_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_B"                   \
                        "$PARTY_B_ADDRESS"                        \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Kyd will spend the UTxOs `9987dcb6253eac1a7b7299374c57cafaaaf28d11fc5b04236d85ae85ea4f9bfb#0` and `9987dcb6253eac1a7b7299374c57cafaaaf28d11fc5b04236d85ae85ea4f9bfb#1`. They will trade 500 of `4d4fbba15d36506985cbd9bc98e53a8aa71820d0ff828fa4e99c76dc.Swan`.

## The Contract

```
MINIMUM_ADA=3000000
PARTY_A_TIMEOUT=$((NOW+12*HOUR))
PARTY_B_TIMEOUT=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party John Fletcher will swap 300 of `20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd.Globe` before Sun, 18 Sep 2022 02:21:10 +0000 for 500 of `4d4fbba15d36506985cbd9bc98e53a8aa71820d0ff828fa4e99c76dc.Swan` from the second party Thomas Kyd before Sun, 18 Sep 2022 14:21:10 +0000.

We create the contract for the previously specified parameters.

```
marlowe-cli template swap --minimum-ada "$MINIMUM_ADA"       \
                          --a-party "$PARTY_A_ADDRESS"       \
                          --a-token "$TOKEN_A"               \
                          --a-amount "$AMOUNT_A"             \
                          --a-timeout "$PARTY_A_TIMEOUT"     \
                          --b-party "$PARTY_B_ADDRESS"       \
                          --b-token "$TOKEN_B"               \
                          --b-amount "$AMOUNT_B"             \
                          --b-timeout "$PARTY_B_TIMEOUT"     \
                          --out-contract-file tx-1.contract  \
                          --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA.

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require.

```
marlowe-cli run initialize --testnet-magic "$MAGIC"                  \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
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

The first party John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 208269
Size: 957 / 16384 = 5%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `606a00d0ab3cb6de6ea0fbbb7c3a6c9511f0ae12b8c31db1344eb08ca0c2f666`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
606a00d0ab3cb6de6ea0fbbb7c3a6c9511f0ae12b8c31db1344eb08ca0c2f666     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "740113f9d098db69ea74de8e6e3ae9ceb8901cc1d29c169479eec438b4bd69e8"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
606a00d0ab3cb6de6ea0fbbb7c3a6c9511f0ae12b8c31db1344eb08ca0c2f666     0        94442997 lovelace + TxOutDatumNone
```

## Transaction 2. First Party Deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe                \
                        --deposit-account "$PARTY_A_ADDRESS"       \
                        --deposit-party "$PARTY_A_ADDRESS"         \
                        --deposit-token "$TOKEN_A"                 \
                        --deposit-amount "$AMOUNT_A"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-2.marlowe                    \
                        --print-stats
```

```console
Datum size: 657
```

Now the first party John Fletcher submits the transaction that deposits their tokens.

```
TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 1217355
Size: 14612 / 16384 = 89%
Execution units:
  Memory: 5151478 / 14000000 = 36%
  Steps: 1415385794 / 10000000000 = 14%
```

The contract received the deposit of 300 `20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd.Globe` in the transaction `2d5b66206587bf6b0491684ad0ad27e490df54db49abd64123edfb34989d33ab`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2d5b66206587bf6b0491684ad0ad27e490df54db49abd64123edfb34989d33ab     1        3000000 lovelace + 300 20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd.476c6f6265 + TxOutDatumHash ScriptDataInBabbageEra "4f1626c72836301c126ec3ecde5546c79c831f65f502ca5b2a3fdf9fda87966b"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2d5b66206587bf6b0491684ad0ad27e490df54db49abd64123edfb34989d33ab     0        95225642 lovelace + TxOutDatumNone
```

## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "$PARTY_B_ADDRESS"       \
                        --deposit-party "$PARTY_B_ADDRESS"         \
                        --deposit-token "$TOKEN_B"                 \
                        --deposit-amount "$AMOUNT_B"               \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-3.marlowe                    \
                        --print-stats
```

```console
Datum size: 30
Payment 1
  Acccount: Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r""
  Payee: Party (Address ""addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5"")
  Ada: Lovelace {getLovelace = 0}
  20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd."Globe": 300
Payment 2
  Acccount: Address ""addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5""
  Payee: Party (Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r"")
  Ada: Lovelace {getLovelace = 0}
  4d4fbba15d36506985cbd9bc98e53a8aa71820d0ff828fa4e99c76dc."Swan": 500
Payment 3
  Acccount: Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r""
  Payee: Party (Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r"")
  Ada: Lovelace {getLovelace = 3000000}
```

Now the second party Thomas Kyd can submit a transaction that deposits their tokens and completes the swap.

```
TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 1420467
Size: 13933 / 16384 = 85%
Execution units:
  Memory: 8265186 / 14000000 = 59%
  Steps: 2155015112 / 10000000000 = 21%
```

The closing of the contract paid 500 `4d4fbba15d36506985cbd9bc98e53a8aa71820d0ff828fa4e99c76dc.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 300 `20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd.Globe` to the second party Thomas Kyd in the transaction `b0be44fc286e594e503f4a55ac9db8d04a31d5883a8540ec840e85d97fc9e015`. There is no UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2d5b66206587bf6b0491684ad0ad27e490df54db49abd64123edfb34989d33ab     0        95225642 lovelace + TxOutDatumNone
b0be44fc286e594e503f4a55ac9db8d04a31d5883a8540ec840e85d97fc9e015     1        3000000 lovelace + 500 4d4fbba15d36506985cbd9bc98e53a8aa71820d0ff828fa4e99c76dc.5377616e + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Kyd's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b0be44fc286e594e503f4a55ac9db8d04a31d5883a8540ec840e85d97fc9e015     0        97192221 lovelace + TxOutDatumNone
b0be44fc286e594e503f4a55ac9db8d04a31d5883a8540ec840e85d97fc9e015     2        1038710 lovelace + 300 20a65d46f059cf4e52f97bba60573b2d54cf0e47d76980ed12f84fdd.476c6f6265 + TxOutDatumNone
```

## Clean Up

```
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
TX_CLEANUP_TOKEN_B=$(
marlowe-cli util select --testnet-magic "$MAGIC"                   \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --asset-only "$TOKEN_B" "$PARTY_A_ADDRESS" \
| sed -e 's/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/'                  \
)
marlowe-cli transaction simple --testnet-magic "$MAGIC"                            \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"           \
                               --tx-in "$TX_2"#0                                   \
                               --tx-in "$TX_CLEANUP_TOKEN_B"                       \
                               --tx-out "$BURN_ADDRESS+1500000+$AMOUNT_B $TOKEN_B" \
                               --required-signer "$PARTY_A_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                  \
                               --out-file /dev/null                                \
                               --submit 600
```

```console
TxId "c2a1f404c922b5d61f2136a016bd75e2294472d3c52eec82eac80bd31ac17562"
```

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_A_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
TX_CLEANUP_TOKEN_A=$(
marlowe-cli util select --testnet-magic "$MAGIC"                   \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --asset-only "$TOKEN_A" "$PARTY_B_ADDRESS" \
| sed -e 's/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/'                  \
)
marlowe-cli transaction simple --testnet-magic "$MAGIC"                            \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"           \
                               --tx-in "$TX_3"#0                                   \
                               --tx-in "$TX_CLEANUP_TOKEN_A"                       \
                               --tx-out "$BURN_ADDRESS+1500000+$AMOUNT_A $TOKEN_A" \
                               --required-signer "$PARTY_B_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                  \
                               --out-file /dev/null                                \
                               --submit 600
```

```console
TxId "f2e5136796a961b0f63489dc677fc8ac9a40f03971b322b43c9096070bd96e4e"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_B_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
