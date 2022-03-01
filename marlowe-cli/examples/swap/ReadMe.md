# Test of a Swap Contract

[This swap contract](../../src/Language/Marlowe/CLI/Examples/Swap.hs) swaps native tokens between two parties.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the two parties: to do this, set the environment variables `PARTY_A_PREFIX` and `PARTY_B_PREFIX` where they appear below.

The two parties' wallets much have exactly one UTxO with the token they want to swap and at least one UTxO without tokens.

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
a219d7ba2aed415f3c337e306b7759d27ba7b36e1c7a343960cd5eaa48671502     0        3009916517 lovelace + TxOutDatumNone
a219d7ba2aed415f3c337e306b7759d27ba7b36e1c7a343960cd5eaa48671502     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JF + TxOutDatumNone
a219d7ba2aed415f3c337e306b7759d27ba7b36e1c7a343960cd5eaa48671502     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JohnFletcher + TxOutDatumNone
a219d7ba2aed415f3c337e306b7759d27ba7b36e1c7a343960cd5eaa48671502     3        2000000 lovelace + 500 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
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

John Fletcher will spend the UTxOs `a219d7ba2aed415f3c337e306b7759d27ba7b36e1c7a343960cd5eaa48671502#0` and `a219d7ba2aed415f3c337e306b7759d27ba7b36e1c7a343960cd5eaa48671502#3`. They will trade 500 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan`.

### The Second Party

```
PARTY_B_PREFIX="$TREASURY/thomas-kyd"
PARTY_B_NAME="Thomas Kyd"
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

The second party Thomas Kyd has the address `addr_test1vpkedn0l6slaj8fzkx08qyce9vpreuu3spmnlxkepx4757grxzyzf` and public-key hash `6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79`. They have the following UTxOs in their wallet:

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
824fd5a474931ebc84b6da23d5e5ffe1f94eb742f2fd96bdc5e44dac6a009010     0        952692170 lovelace + TxOutDatumNone
824fd5a474931ebc84b6da23d5e5ffe1f94eb742f2fd96bdc5e44dac6a009010     1        2000000 lovelace + 800 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
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

Thomas Kyd will spend the UTxOs `824fd5a474931ebc84b6da23d5e5ffe1f94eb742f2fd96bdc5e44dac6a009010#0` and `824fd5a474931ebc84b6da23d5e5ffe1f94eb742f2fd96bdc5e44dac6a009010#1`. They will trade 800 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
```

The tip is at slot 50843196. The current POSIX time implies that the tip of the blockchain should be slightly before slot 50843197. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
PARTY_A_TIMEOUT=$((NOW+12*HOUR))
PARTY_B_TIMEOUT=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the first party John Fletcher will swap 500 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` before Sat, 19 Feb 2022 07:26:52 +0000 for 800 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` from the second party Thomas Kyd before Sat, 19 Feb 2022 19:26:52 +0000.

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
Validator size: 13876
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 38794319, exBudgetMemory = ExMemory 130400}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wp6wcsaydajh99vkam49tgstxhjhxy0wvz8u2mr3rdrwugqnsnda9`.

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
Fee: Lovelace 202197
Size: 821 / 16384 = 5%
Execution units:
  Memory: 0 / 16000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `d6f09e868f73aee028d281645de5f5d92e5286af345e55d1bf1cd846e6583246`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d6f09e868f73aee028d281645de5f5d92e5286af345e55d1bf1cd846e6583246     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d5496eeea77d4d55847652811fdb8943e14c01167d09ac0c152ccbe0f4bfdb0"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d6f09e868f73aee028d281645de5f5d92e5286af345e55d1bf1cd846e6583246     0        3006714320 lovelace + TxOutDatumNone
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
Fee: Lovelace 1239331
Size: 15535 / 16384 = 94%
Execution units:
  Memory: 4581692 / 16000000 = 28%
  Steps: 1614123830 / 10000000000 = 16%
```

The contract received the deposit of 500 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` in the transaction `f7f34e8c177c3b3cff95e918b4ac6858208a5205f05a4eabadd5469c67061920`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f7f34e8c177c3b3cff95e918b4ac6858208a5205f05a4eabadd5469c67061920     1        3000000 lovelace + 500 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumHash ScriptDataInAlonzoEra "d6936ce36846f899c4a0e7620922b92a4ddc5fd337f3739b3075c3c33035df71"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f7f34e8c177c3b3cff95e918b4ac6858208a5205f05a4eabadd5469c67061920     0        3007474989 lovelace + TxOutDatumNone
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
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Swan": 500
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Globe": 800
Payment 1
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")
  Ada: 0.000000
Payment 2
  Acccount: PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
  Ada: 0.000000
Payment 3
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
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
Fee: Lovelace 1380831
Size: 14991 / 16384 = 91%
Execution units:
  Memory: 6681602 / 16000000 = 41%
  Steps: 2228142891 / 10000000000 = 22%
```

The closing of the contract paid 800 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 500 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` to the second party Thomas Kyd in the transaction `1753640713cfae8d0c44f2c381b1d27dc727b0779e8933342e731380f216e9b5`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f7f34e8c177c3b3cff95e918b4ac6858208a5205f05a4eabadd5469c67061920     0        3007474989 lovelace + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1753640713cfae8d0c44f2c381b1d27dc727b0779e8933342e731380f216e9b5     0        951966541 lovelace + TxOutDatumNone
1753640713cfae8d0c44f2c381b1d27dc727b0779e8933342e731380f216e9b5     1        1344798 lovelace + 500 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
```

