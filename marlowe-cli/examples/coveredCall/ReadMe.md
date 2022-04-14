# Test of a Covered Call Contract
[This option contract](../../../marlowe-contracts/src/Marlowe/Contracts/Options.hs) transfers a token if the counter-party exercises the option.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.
See below for how to set `MAGIC` to select the network.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables `ISSUER_PREFIX` and `COUNTERPARTY_PREFIX` where they appear below.

The two parties' wallets much have exactly one UTxO with the token they want to swap and at least one UTxO without tokens.

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

The tip is at slot 32425. The current POSIX time implies that the tip of the blockchain should be slightly before slot 32426. Tests may fail if this is not the case.

### Participants

#### The Issuer

```
ISSUER_PREFIX="$TREASURY/john-fletcher"
ISSUER_NAME="John Fletcher"
ISSUER_PAYMENT_SKEY="$ISSUER_PREFIX".skey
ISSUER_PAYMENT_VKEY="$ISSUER_PREFIX".vkey
```

Create the issuer's keys, if necessary.

```
if [[ ! -e "$ISSUER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$ISSUER_PAYMENT_SKEY"      \
                              --verification-key-file "$ISSUER_PAYMENT_VKEY"
fi
ISSUER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$ISSUER_PAYMENT_VKEY" )
ISSUER_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$ISSUER_PAYMENT_VKEY")
```

Fund the issuer's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 150000000                      \
                        "$ISSUER_ADDRESS"
```

```console
TxId "e967a501e35220c28258e0dce8885e324318c966c0db480d7debb755b5f47658"
```

The issuer mints their tokens for the contract.

```
MINT_EXPIRES=$((TIP + 1000000))
TOKEN_NAME_A=Globe
AMOUNT_A=300
CURRENCY_SYMBOL_A=$(
marlowe-cli util mint "${MAGIC[@]}"                             \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$ISSUER_PAYMENT_SKEY"  \
                      --change-address  "$ISSUER_ADDRESS"       \
                      --count "$AMOUNT_A"                       \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_A"                           \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
```

The issuer John Fletcher is the minimum-ADA provider and has the address `addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r` and public-key hash `1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$ISSUER_PAYMENT_SKEY"  \
                       --change-address "$ISSUER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
62f41d248c2023520023eb025804d5583d095a89871157f4b0e8be6ff852c534     0        147651442 lovelace + TxOutDatumNone
62f41d248c2023520023eb025804d5583d095a89871157f4b0e8be6ff852c534     1        2000000 lovelace + 300 48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.476c6f6265 + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_A_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$ISSUER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_A_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_A"                   \
                        "$ISSUER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

John Fletcher will spend the UTxOs `62f41d248c2023520023eb025804d5583d095a89871157f4b0e8be6ff852c534#0` and `62f41d248c2023520023eb025804d5583d095a89871157f4b0e8be6ff852c534#1`. They will trade 300 of `48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.Globe`.

#### The Counterparty

```
COUNTERPARTY_PREFIX="$TREASURY/thomas-kyd"
COUNTERPARTY_NAME="Thomas Kyd"
COUNTERPARTY_PAYMENT_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_PAYMENT_VKEY="$COUNTERPARTY_PREFIX".vkey
```

Create the counterparty's keys, if necessary.

```
if [[ ! -e "$COUNTERPARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$COUNTERPARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$COUNTERPARTY_PAYMENT_VKEY"
fi
COUNTERPARTY_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY" )
COUNTERPARTY_PUBKEYHASH=$(cardano-cli address key-hash --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY")
```

Fund the counterparty's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 150000000                      \
                        "$COUNTERPARTY_ADDRESS"
```

```console
TxId "d54a9b66662d63f8ec8e74e7ff0c0f5ae9514eae4f6afeed78bb94075bf71252"
```

The counterparty mints their tokens for the swap.

```
TOKEN_NAME_B=Swan
AMOUNT_B=500
CURRENCY_SYMBOL_B=$(
marlowe-cli util mint "${MAGIC[@]}"                                  \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                      --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                      --change-address  "$COUNTERPARTY_ADDRESS"      \
                      --count "$AMOUNT_B"                            \
                      --expires "$MINT_EXPIRES"                      \
                      --out-file /dev/null                           \
                      --submit=600                                   \
                      "$TOKEN_NAME_B"                                \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                                 \
)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
```

The counterparty Thomas Kyd has the address `addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5` and public-key hash `fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                       --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                       --change-address "$COUNTERPARTY_ADDRESS"       \
                       --out-file /dev/null                           \
                       --submit=600                                   \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cc2b416377e4a7e3c9af9b5398a874e59f306dcfe9901a41ebad4541807d0830     0        147651574 lovelace + TxOutDatumNone
cc2b416377e4a7e3c9af9b5398a874e59f306dcfe9901a41ebad4541807d0830     1        2000000 lovelace + 500 02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659.5377616e + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_B_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_B_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_B"                   \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Kyd will spend the UTxOs `cc2b416377e4a7e3c9af9b5398a874e59f306dcfe9901a41ebad4541807d0830#0` and `cc2b416377e4a7e3c9af9b5398a874e59f306dcfe9901a41ebad4541807d0830#1`. They will trade 500 of `02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659.Swan`.

The tip is at slot 32425. The current POSIX time implies that the tip of the blockchain should be slightly before slot 32571. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
ISSUE_TIMEOUT=$((NOW+5*HOUR))
MATURITY_TIMEOUT=$((NOW+0*HOUR))
SETTLEMENT_TIMEOUT=$((NOW+12*HOUR))
```

The contract has a minimum-ADA requirement and three timeouts. It also specifies that the issuer John Fletcher will put 300 of `48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.Globe` before Fri, 15 Apr 2022 05:20:56 +0000 into the contract and if the counter-party Thomas Kyd exercises the option for 500 of `02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659.Swan` after Fri, 15 Apr 2022 00:20:56 +0000 and before Fri, 15 Apr 2022 12:20:56 +0000.

We create the contract for the previously specified parameters.

```
marlowe-cli template coveredCall --minimum-ada "$MINIMUM_ADA"                  \
                                 --issuer "PK=$ISSUER_PUBKEYHASH"              \
                                 --counter-party "PK=$COUNTERPARTY_PUBKEYHASH" \
                                 --currency "$TOKEN_B"                         \
                                 --underlying "$TOKEN_A"                       \
                                 --strike "$AMOUNT_B"                          \
                                 --amount "$AMOUNT_A"                          \
                                 --issue-date "$ISSUE_TIMEOUT"                 \
                                 --maturity-date "$MATURITY_TIMEOUT"           \
                                 --settlement-date "$SETTLEMENT_TIMEOUT"       \
                                 --out-contract-file tx-1.contract             \
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

The issuer John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_A_ADA"                     \
                        --required-signer "$ISSUER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-1.marlowe           \
                        --change-address "$ISSUER_ADDRESS"        \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 209853
Size: 995 / 32768 = 3%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the issuer John Fletcher in the transaction `1b9713f3d2c6a7c7aae8fe86c8effa5e2108468ba66d74bb9a456cbb8b18440a`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1b9713f3d2c6a7c7aae8fe86c8effa5e2108468ba66d74bb9a456cbb8b18440a     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "bc7eb334bccb1e68b926c143fdd57215fc5a04cf0e4ea259db18af0804af5078"
```

Here are the UTxOs at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1b9713f3d2c6a7c7aae8fe86c8effa5e2108468ba66d74bb9a456cbb8b18440a     0        144441589 lovelace + TxOutDatumNone
```

## Transaction 2. The issuer deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe               \
                        --deposit-account "PK=$ISSUER_PUBKEYHASH" \
                        --deposit-party "PK=$ISSUER_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_A"                \
                        --deposit-amount "$AMOUNT_A"              \
                        --invalid-before "$NOW"                   \
                        --invalid-hereafter "$((NOW+1*HOUR))"     \
                        --out-file tx-2.marlowe                   \
                        --print-stats
```

```console
Datum size: 698
```

Now the issuer John Fletcher submits the transaction that deposits their tokens.

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_1"#0              \
                        --tx-in "$TX_1"#0                         \
                        --tx-in "$TX_0_A_TOKEN"                   \
                        --required-signer "$ISSUER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$ISSUER_ADDRESS"        \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1227446
Size: 14595 / 32768 = 44%
Execution units:
  Memory: 4907764 / 30000000 = 16%
  Steps: 1761979990 / 10000000000 = 17%
```

The contract received the deposit of 300 `48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.Globe` in the transaction `09c0585add0b9507d3b35ecd2ca1d4b04487903ddebe47818e29d0370a85dd43`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
09c0585add0b9507d3b35ecd2ca1d4b04487903ddebe47818e29d0370a85dd43     1        3000000 lovelace + 300 48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.476c6f6265 + TxOutDatumHash ScriptDataInAlonzoEra "189e28c14c9993d0f7c2abcd483b25f79833c72f15f4731e846adce24f930bbc"
```

Here is the UTxO at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
09c0585add0b9507d3b35ecd2ca1d4b04487903ddebe47818e29d0370a85dd43     0        145214143 lovelace + TxOutDatumNone
```

## Transaction 3. The Counter-Party chooses to exercise the option

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                  \
                        --choice-name "Exercise Call"                \
                        --choice-party "PK=$COUNTERPARTY_PUBKEYHASH" \
                        --choice-number 1                            \
                        --invalid-before "$NOW"                      \
                        --invalid-hereafter "$((NOW+1*HOUR))"        \
                        --out-file tx-3.marlowe                      \
                        --print-stats
```

```console
Datum size: 591
```

TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-2.marlowe                 \
                        --tx-in-marlowe "$TX_2"#1                      \
                        --tx-in-collateral "$TX_0_B_ADA"               \
                        --tx-in "$TX_0_B_ADA"                          \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                        --marlowe-out-file tx-3.marlowe                \
                        --change-address "$COUNTERPARTY_ADDRESS"       \
                        --out-file tx-3.raw                            \
                        --print-stats                                  \
                        --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                       \
)
```

```console
Fee: Lovelace 1188153
Size: 14328 / 32768 = 43%
Execution units:
  Memory: 4659106 / 30000000 = 15%
  Steps: 1640563055 / 10000000000 = 16%
## Transaction 4. The Counter-Party Deposits their Tokens.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe                     \
                        --deposit-account "PK=$COUNTERPARTY_PUBKEYHASH" \
                        --deposit-party "PK=$COUNTERPARTY_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_B"                      \
                        --deposit-amount "$AMOUNT_B"                    \
                        --invalid-before "$NOW"                         \
                        --invalid-hereafter "$((NOW+8*HOUR))"           \
                        --out-file tx-4.marlowe                         \
                        --print-stats
```

```console
Datum size: 76
Payment 1
  Acccount: PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf"
  Payee: Party (PK "fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff")
  Ada: 0.000000
  48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423."Globe": 300
Payment 2
  Acccount: PK "fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff"
  Payee: Party (PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf")
  Ada: 0.000000
  02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659."Swan": 500
Payment 3
  Acccount: PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf"
  Payee: Party (PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf")
  Ada: 3.000000
```

Now the counter-party Thomas Kyd can submit a transaction that deposits their tokens.

```
TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-3.marlowe                 \
                        --tx-in-marlowe "$TX_3"#1                      \
                        --tx-in-collateral "$TX_3"#0                   \
                        --tx-in "$TX_3"#0                              \
                        --tx-in "$TX_0_B_TOKEN"                        \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                        --marlowe-out-file tx-4.marlowe                \
                        --change-address "$COUNTERPARTY_ADDRESS"       \
                        --out-file tx-4.raw                            \
                        --print-stats                                  \
                        --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                       \
)
```

```console
Fee: Lovelace 1272460
Size: 13767 / 32768 = 42%
Execution units:
  Memory: 5988524 / 30000000 = 19%
  Steps: 2026696835 / 10000000000 = 20%
```

The closing of the contract paid 500 `02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 300 `48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.Globe` to the counter party Thomas Kyd in the transaction `8c6a6993a8fced7568f977c0ad2f2e93d6df1066707d8abf5292fd0df1c80ac5`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the issuer party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
09c0585add0b9507d3b35ecd2ca1d4b04487903ddebe47818e29d0370a85dd43     0        145214143 lovelace + TxOutDatumNone
8c6a6993a8fced7568f977c0ad2f2e93d6df1066707d8abf5292fd0df1c80ac5     2        3000000 lovelace + 500 02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659.5377616e + TxOutDatumNone
```

Here are the UTxOs at the counter party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8c6a6993a8fced7568f977c0ad2f2e93d6df1066707d8abf5292fd0df1c80ac5     0        145846163 lovelace + TxOutDatumNone
8c6a6993a8fced7568f977c0ad2f2e93d6df1066707d8abf5292fd0df1c80ac5     1        1344798 lovelace + 300 48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.476c6f6265 + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple "${MAGIC[@]}"                                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                               --tx-in "$TX_4"#2                                           \
                               --tx-out "$COUNTERPARTY_ADDRESS+1400000+$AMOUNT_B $TOKEN_B" \
                               --required-signer "$ISSUER_PAYMENT_SKEY"                    \
                               --change-address "$ISSUER_ADDRESS"                          \
                               --out-file /dev/null                                        \
                               --submit 600
```

```console
TxId "a05ca8442b86fa4f79763c3ef013d4a0eae187cbe9595759fe86fc68285f9062"
```

marlowe-cli transaction simple "${MAGIC[@]}"                                         \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                               --tx-in "$TX_4"#0                                     \
                               --tx-in "$TX_4"#1                                     \
                               --tx-out "$ISSUER_ADDRESS+1400000+$AMOUNT_A $TOKEN_A" \
                               --required-signer "$COUNTERPARTY_PAYMENT_SKEY"        \
                               --change-address "$COUNTERPARTY_ADDRESS"              \
                               --out-file /dev/null                                  \
                               --submit 600
```

```console
TxId "1140c62ef92ce05fad2be7749fc2e0ed680bf990973cb7c39bd48e4239f781c7"
marlowe-cli util mint "${MAGIC[@]}"                             \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$ISSUER_PAYMENT_SKEY"  \
                      --change-address  "$ISSUER_ADDRESS"       \
                      --count "-$AMOUNT_A"                      \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$TOKEN_NAME_A"
```

```console
PolicyID "48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423"
marlowe-cli util mint "${MAGIC[@]}"                                  \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                      --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                      --change-address  "$COUNTERPARTY_ADDRESS"      \
                      --count "-$AMOUNT_B"                           \
                      --expires "$MINT_EXPIRES"                      \
                      --out-file /dev/null                           \
                      --submit=600                                   \
                      "$TOKEN_NAME_B"
```

```console
PolicyID "02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659"
TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$ISSUER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$ISSUER_PAYMENT_SKEY"  \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600
```

```console
TxId "be80227ebd6869cef4bf4743f969e60c0d80e919516b558db39e030cd2f7ddfb"
TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple "${MAGIC[@]}"                                  \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                               --tx-in "$TX"                                  \
                               --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                               --change-address "$FAUCET_ADDRESS"             \
                               --out-file /dev/null                           \
                               --submit 600
```

```console
TxId "ec090e648f70124e686273172f3b5e8175be0106c6ccabf17c790103bff34cfe"
