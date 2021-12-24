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
0e5ad6877386c745123ad724a07710e81071ac5bcfcbbf0f213255e5bd2dda0c     0        982872621 lovelace + TxOutDatumNone
0e5ad6877386c745123ad724a07710e81071ac5bcfcbbf0f213255e5bd2dda0c     1        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
14bf728eff2805b061d5eff828478f7ea51b27a59b87a25c0598fa387bd07eea     2        3000000 lovelace + TxOutDatumNone
177649276d74dbcb29a0eb893083d2a6a2a3bb351759ae6d49828aa5d5d4dc7c     0        65743037 lovelace + TxOutDatumNone
2c28ae9d938493e23d6369a687e8f6b1d795cf82065fd6cff30438e3e3c67c4a     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JohnFletcher + TxOutDatumNone
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

John Fletcher will spend the UTxOs `0e5ad6877386c745123ad724a07710e81071ac5bcfcbbf0f213255e5bd2dda0c#0` and `0e5ad6877386c745123ad724a07710e81071ac5bcfcbbf0f213255e5bd2dda0c#1`. They will trade 200 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan`.

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
7b6501ae658e1d7f06580cfa98624a4ddabbeaa1400e11097a21862a9d35f0db     0        65982270 lovelace + TxOutDatumNone
7b6501ae658e1d7f06580cfa98624a4ddabbeaa1400e11097a21862a9d35f0db     2        3000000 lovelace + 100 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
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
> utxo-0-a.json
TX_0_B_TOKEN=$(jq -r '.key' utxo-0-a.json | head -n 1)
CURRENCY_SYMBOL_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-a.json | head -n 1)
TOKEN_NAME_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-a.json | head -n 1)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
AMOUNT_B=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-a.json | head -n 1)
```

Thomas Middleton will spend the UTxOs `7b6501ae658e1d7f06580cfa98624a4ddabbeaa1400e11097a21862a9d35f0db#0` and `7b6501ae658e1d7f06580cfa98624a4ddabbeaa1400e11097a21862a9d35f0db#2`. They will trade 100 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe`.

### Validator Script and Address

The contract has a validator script and address. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

```
CONTRACT_ADDRESS=$(
marlowe-cli contract address "${MAGIC[@]}"                \
                             --slot-length "$SLOT_LENGTH" \
                             --slot-offset "$SLOT_OFFSET" \
)
marlowe-cli contract validator "${MAGIC[@]}"                \
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

The tip is at slot 2168200. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2168202. Tests may fail if this is not the case.

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
marlowe-cli template swap --minimum-ada "$MINIMUM_ADA"       \
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
marlowe-cli contract datum --contract-file tx-1.contract \
                           --state-file    tx-1.state    \
                           --out-file      tx-1.datum
```

```console
5e8f409229ccef500c22fec2bad753781a0f7f83be397cea6563a97d4f0a50c7
```

The first party John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace requiredd for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli transaction create "${MAGIC[@]}"                             \
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

The contract received the minimum ADA of 3000000 lovelace from the first party John Fletcher in the transaction `fb631faf70260b6da7b9a571e90444c85dec4f692e08b6b0e44dae6c98b12c90`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fb631faf70260b6da7b9a571e90444c85dec4f692e08b6b0e44dae6c98b12c90     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "5e8f409229ccef500c22fec2bad753781a0f7f83be397cea6563a97d4f0a50c7"
```

Here are the UTxOs at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fb631faf70260b6da7b9a571e90444c85dec4f692e08b6b0e44dae6c98b12c90     0        976675088 lovelace + TxOutDatumNone
fb631faf70260b6da7b9a571e90444c85dec4f692e08b6b0e44dae6c98b12c90     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 2. First Party Deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli input deposit --deposit-account "PK=$PARTY_A_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_A_PUBKEYHASH"   \
                          --deposit-token "$TOKEN_A"                 \
                          --deposit-amount "$AMOUNT_A"               \
                          --out-file tx-2.input
```

Next we compute the transition caused by that input to the contract.

```
marlowe-cli run compute --contract-file tx-1.contract          \
                        --state-file    tx-1.state             \
                        --input-file    tx-2.input             \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-2.marlowe                \
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
marlowe-cli contract redeemer --input-file tx-2.input    \
                              --out-file   tx-2.redeemer
```

As in the first transaction, we compute the datum and its hash:

```
marlowe-cli contract datum --contract-file tx-2.contract \
                           --state-file    tx-2.state    \
                           --out-file      tx-2.datum
```

```console
8bc4fc62a302095aa5df288e547e72e174fd0f80345b457336de8c8ecefa0e84
```

Now the first party John Fletcher submits the transaction that deposits their tokens.

```
TX_2=$(
marlowe-cli transaction advance "${MAGIC[@]}"                                      \
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

The contract received the deposit of 200 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` in the transaction `af5b41c7dd6e90e38e569e625b86bce5556febc8f238f0a542b31c95f84f89eb`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
af5b41c7dd6e90e38e569e625b86bce5556febc8f238f0a542b31c95f84f89eb     1        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumHash ScriptDataInAlonzoEra "8bc4fc62a302095aa5df288e547e72e174fd0f80345b457336de8c8ecefa0e84"
```

Here is the UTxO at the first party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_A_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
af5b41c7dd6e90e38e569e625b86bce5556febc8f238f0a542b31c95f84f89eb     0        978473635 lovelace + TxOutDatumNone
af5b41c7dd6e90e38e569e625b86bce5556febc8f238f0a542b31c95f84f89eb     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 3. The Second Party Deposits their Tokens to Complete the Swap.

First we compute the input for the contract to transition forward.

```
marlowe-cli input deposit --deposit-account "PK=$PARTY_B_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_B_PUBKEYHASH"   \
                          --deposit-token "$TOKEN_B"                 \
                          --deposit-amount "$AMOUNT_B"               \
                          --out-file tx-3.input
```

As in the second transaction we compute the contract's transition, its new state, and the redeemer. Because the contract is being closed, no new datum need be computed.

```
marlowe-cli run compute --contract-file tx-2.contract          \
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
marlowe-cli contract redeemer --input-file tx-3.input    \
                              --out-file   tx-3.redeemer
marlowe-cli contract datum --contract-file tx-3.contract \
                           --state-file    tx-3.state    \
                           --out-file      tx-3.datum
```

```console
0e0819a8896036b0c42aa1f7cc6eb002dcb5156bda237790f5be7e513ab3f479
```

Now the second party Thomas Middleton can submit a transaction that deposits their tokens and completes the swap.

```
TX_3=$(
marlowe-cli transaction close "${MAGIC[@]}"                                               \
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

The closing of the contract paid 100 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 200 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` to the second party Thomas Middleton in the transaction `9d1fa5b132cf677b61b3cf432acee0656ae098224fbf9bed9fc024ea061fa8f0`. There is no UTxO at the contract address:

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
9d1fa5b132cf677b61b3cf432acee0656ae098224fbf9bed9fc024ea061fa8f0     1        3000000 lovelace + 100 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
af5b41c7dd6e90e38e569e625b86bce5556febc8f238f0a542b31c95f84f89eb     0        978473635 lovelace + TxOutDatumNone
af5b41c7dd6e90e38e569e625b86bce5556febc8f238f0a542b31c95f84f89eb     2        3000000 lovelace + TxOutDatumNone
```

Here are the UTxOs at the second party Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_B_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d1fa5b132cf677b61b3cf432acee0656ae098224fbf9bed9fc024ea061fa8f0     0        64604012 lovelace + TxOutDatumNone
9d1fa5b132cf677b61b3cf432acee0656ae098224fbf9bed9fc024ea061fa8f0     2        3000000 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
```

## Clean Up

It's convenient to consolidate the UTxOs for the first party.

```
marlowe-cli transaction simple "${MAGIC[@]}"                                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                               --tx-in "$TX_2"#0                                           \
                               --tx-in "$TX_2"#2                                           \
                               --tx-in "$TX_3"#1                                           \
                               --required-signer "$PARTY_A_PAYMENT_SKEY"                   \
                               --tx-out "$PARTY_A_ADDRESS+$MINIMUM_ADA+$AMOUNT_B $TOKEN_B" \
                               --change-address "$PARTY_A_ADDRESS"                         \
                               --out-file tx-4.raw                                         \
                               --submit=600                                                \
> /dev/null
