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
1dc4eacf0035d82043f150492ef77dd83b020283116d6c34df350c27e1a6da5c     0        1036678792 lovelace + TxOutDatumNone
2c28ae9d938493e23d6369a687e8f6b1d795cf82065fd6cff30438e3e3c67c4a     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JohnFletcher + TxOutDatumNone
ca0c84635577e7d7c0b2768816df54da2cfc69cc784456ae673511c52616a49a     2        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with exactly one type of native token.

```
TX_0_A_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$PARTY_A_ADDRESS"                                                             \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$PARTY_A_ADDRESS"                                                                                                    \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[0].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-a.json
TX_0_A_TOKEN=$(jq -r '.key' utxo-0-a.json | head -n 1)
CURRENCY_SYMBOL_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-a.json | head -n 1)
TOKEN_NAME_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-a.json | head -n 1)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
AMOUNT_A=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-a.json | head -n 1)
```

John Fletcher will spend the UTxOs `1dc4eacf0035d82043f150492ef77dd83b020283116d6c34df350c27e1a6da5c#0` and `ca0c84635577e7d7c0b2768816df54da2cfc69cc784456ae673511c52616a49a#2`. They will trade 200 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan`.

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
0407579ed4e0718cad128967f5987bacce8a6fd0f8f002ad26d1d20e460e3b7e     4        1000000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
ca0c84635577e7d7c0b2768816df54da2cfc69cc784456ae673511c52616a49a     0        56611584 lovelace + TxOutDatumNone
ca0c84635577e7d7c0b2768816df54da2cfc69cc784456ae673511c52616a49a     1        1344798 lovelace + 100 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with exactly one type of native token.

```
TX_0_B_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$PARTY_B_ADDRESS"                                                             \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$PARTY_B_ADDRESS"                                                                                                    \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[0].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-b.json
TX_0_B_TOKEN=$(jq -r '.key' utxo-0-b.json | head -n 1)
CURRENCY_SYMBOL_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-b.json | head -n 1)
TOKEN_NAME_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-b.json | head -n 1)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
AMOUNT_B=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-b.json | head -n 1)
```

Thomas Middleton will spend the UTxOs `ca0c84635577e7d7c0b2768816df54da2cfc69cc784456ae673511c52616a49a#0` and `ca0c84635577e7d7c0b2768816df54da2cfc69cc784456ae673511c52616a49a#1`. They will trade 100 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 2670200. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2670208. Tests may fail if this is not the case.

## The Contract

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party John Fletcher will swap 200 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` for 100 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` from the second party Thomas Middleton.

```
MINIMUM_ADA=3000000
PARTY_A_TIMEOUT=$(($TIP+12*3600))
PARTY_B_TIMEOUT=$(($TIP+24*3600))
```

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

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

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
Validator size: 13848
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 37484307, exBudgetMemory = ExMemory 126000}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wq3n0x7khejv6r0k3rzjw3tfwjjxlmvc7e5zxa8z4jwc57ca4jg8m`.

The first party John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace requiredd for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

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
Fee: Lovelace 201669
Size: 809 / 32768 = 2%
Execution units:
  Memory: 0 / 10000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `5eac953220e6954d490bf08758b7aa78c7073885228129219ee1ec02c5ed2276`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5eac953220e6954d490bf08758b7aa78c7073885228129219ee1ec02c5ed2276     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "966539f423139d12542542b5545007264d03f8cd941ee87c5c62a74bd8ae6c9b"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5eac953220e6954d490bf08758b7aa78c7073885228129219ee1ec02c5ed2276     0        1033477123 lovelace + TxOutDatumNone
```

## Transaction 2. First Party Deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe                \
                        --deposit-account "PK=$PARTY_A_PUBKEYHASH" \
                        --deposit-party "PK=$PARTY_A_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_A"                 \
                        --deposit-amount "$AMOUNT_A"               \
                        --invalid-before "$TIP"                    \
                        --invalid-hereafter "$(($TIP+4*3600))"     \
                        --out-file tx-2.marlowe                    \
                        --print-stats
```

```console
Datum size: 526
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
Fee: Lovelace 1175384
Size: 15477 / 32768 = 47%
Execution units:
  Memory: 3810872 / 10000000 = 38%
  Steps: 1379465306 / 10000000000 = 13%
```

The contract received the deposit of 200 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` in the transaction `824b791b8a746425c72a2d0b3a0851189b63fe040e3b019bd716e669ca0ee46b`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
824b791b8a746425c72a2d0b3a0851189b63fe040e3b019bd716e669ca0ee46b     1        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumHash ScriptDataInAlonzoEra "3792f2b1bb88bf90e4759119558aaf3b1d28c1482313afef101c63fac81288e6"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
824b791b8a746425c72a2d0b3a0851189b63fe040e3b019bd716e669ca0ee46b     0        1035301739 lovelace + TxOutDatumNone
```

## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "PK=$PARTY_B_PUBKEYHASH" \
                        --deposit-party "PK=$PARTY_B_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_B"                 \
                        --deposit-amount "$AMOUNT_B"               \
                        --invalid-before "$TIP"                    \
                        --invalid-hereafter "$(($TIP+4*3600))"     \
                        --out-file tx-3.marlowe                    \
                        --print-stats
```

```console
Datum size: 19
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Swan": 200
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Globe": 100
Payment 1
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9")
  Ada: 0.000000
Payment 2
  Acccount: PK "90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
  Ada: 0.000000
Payment 3
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
  Ada: 3.000000
```

Now the second party Thomas Middleton can submit a transaction that deposits their tokens and completes the swap.

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                                               \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                        --marlowe-in-file tx-2.marlowe                              \
                        --tx-in-marlowe "$TX_2"#1                                   \
                        --tx-in-collateral "$TX_0_B_ADA"                            \
                        --tx-in "$TX_0_B_ADA"                                       \
                        --tx-in "$TX_0_B_TOKEN"                                     \
                        --required-signer "$PARTY_B_PAYMENT_SKEY"                   \
                        --marlowe-out-file tx-3.marlowe                             \
                        --change-address "$PARTY_B_ADDRESS"                         \
                        --out-file tx-3.raw                                         \
                        --print-stats                                               \
                        --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                    \
)
```

```console
Fee: Lovelace 1378258
Size: 14944 / 32768 = 45%
Execution units:
  Memory: 6675614 / 10000000 = 66%
  Steps: 2225927891 / 10000000000 = 22%
```

The closing of the contract paid 100 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 200 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` to the second party Thomas Middleton in the transaction `bfc9b50001d8a1f040778d56c216aafbeed4cdeab63ca6633927843939682b07`. There is no UTxO at the contract address:

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
824b791b8a746425c72a2d0b3a0851189b63fe040e3b019bd716e669ca0ee46b     0        1035301739 lovelace + TxOutDatumNone
bfc9b50001d8a1f040778d56c216aafbeed4cdeab63ca6633927843939682b07     2        3000000 lovelace + 100 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bfc9b50001d8a1f040778d56c216aafbeed4cdeab63ca6633927843939682b07     0        55233326 lovelace + TxOutDatumNone
bfc9b50001d8a1f040778d56c216aafbeed4cdeab63ca6633927843939682b07     1        1344798 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
```

