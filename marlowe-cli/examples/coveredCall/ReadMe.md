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

The tip is at slot 3418376. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3418381. Tests may fail if this is not the case.

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
ISSUER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$ISSUER_PAYMENT_VKEY" )
```

Fund the issuer's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 150000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$ISSUER_ADDRESS"
```

```console
TxId "3a61490731940f383119522c4ace4096682d4a97a3c592a3d93e6598014ad5f9"
```

The issuer mints their tokens for the contract.

```
MINT_EXPIRES=$((TIP + 1000000))
TOKEN_NAME_A=Globe
AMOUNT_A=300
CURRENCY_SYMBOL_A=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                  \
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

The issuer John Fletcher is the minimum-ADA provider and has the address `addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$ISSUER_PAYMENT_SKEY"  \
                       --change-address "$ISSUER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3b7c025f62a856fe50c69a4af1fbdf57a4f7055f9b06b4bbf3fbac6248dbb978     0        147651266 lovelace + TxOutDatumNone
3b7c025f62a856fe50c69a4af1fbdf57a4f7055f9b06b4bbf3fbac6248dbb978     1        2000000 lovelace + 300 01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3.476c6f6265 + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_A_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$ISSUER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_A_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_A"                   \
                        "$ISSUER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

John Fletcher will spend the UTxOs `3b7c025f62a856fe50c69a4af1fbdf57a4f7055f9b06b4bbf3fbac6248dbb978#0` and `3b7c025f62a856fe50c69a4af1fbdf57a4f7055f9b06b4bbf3fbac6248dbb978#1`. They will trade 300 of `01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3.Globe`.

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
COUNTERPARTY_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY" )
```

Fund the counterparty's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 150000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$COUNTERPARTY_ADDRESS"
```

```console
TxId "96f94aec16145b1009074c2072324c5e359eff612cb777e13a962ff0ae102d34"
```

The counterparty mints their tokens for the swap.

```
TOKEN_NAME_B=Swan
AMOUNT_B=500
CURRENCY_SYMBOL_B=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                       \
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

The counterparty Thomas Kyd has the address `addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                       \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                       --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                       --change-address "$COUNTERPARTY_ADDRESS"       \
                       --out-file /dev/null                           \
                       --submit=600                                   \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bcba9b4a68d0cc4a8c05f1894b532a1d2663cd25ce09ee63093fd641646a17a7     0        147651398 lovelace + TxOutDatumNone
bcba9b4a68d0cc4a8c05f1894b532a1d2663cd25ce09ee63093fd641646a17a7     1        2000000 lovelace + 500 7797b6f5effa8e8c70c2322fc2705804b958bc7c1df2ef8408029fce.5377616e + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with the newly minted native token.

```
TX_0_B_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_B_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$TOKEN_B"                   \
                        "$COUNTERPARTY_ADDRESS"                   \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Kyd will spend the UTxOs `bcba9b4a68d0cc4a8c05f1894b532a1d2663cd25ce09ee63093fd641646a17a7#0` and `bcba9b4a68d0cc4a8c05f1894b532a1d2663cd25ce09ee63093fd641646a17a7#1`. They will trade 500 of `7797b6f5effa8e8c70c2322fc2705804b958bc7c1df2ef8408029fce.Swan`.

The tip is at slot 3418376. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3418523. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
ISSUE_TIMEOUT=$((NOW+5*HOUR))
MATURITY_TIMEOUT=$((NOW+0*HOUR))
SETTLEMENT_TIMEOUT=$((NOW+12*HOUR))
```

The contract has a minimum-ADA requirement and three timeouts. It also specifies that the issuer John Fletcher will put 300 of `01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3.Globe` before Sat, 17 Sep 2022 18:32:56 +0000 into the contract and if the counter-party Thomas Kyd exercises the option for 500 of `7797b6f5effa8e8c70c2322fc2705804b958bc7c1df2ef8408029fce.Swan` after Sat, 17 Sep 2022 13:32:56 +0000 and before Sun, 18 Sep 2022 01:32:56 +0000.

We create the contract for the previously specified parameters.

```
marlowe-cli template coveredCall --minimum-ada "$MINIMUM_ADA"                  \
                                 --issuer "$ISSUER_ADDRESS"                    \
                                 --counter-party "$COUNTERPARTY_ADDRESS"       \
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

The issuer John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 217157
Size: 1159 / 16384 = 7%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the issuer John Fletcher in the transaction `0ee5587a23acc39ac6aed41d301d889c66f7d76415c67eeff761631b3cd4735f`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0ee5587a23acc39ac6aed41d301d889c66f7d76415c67eeff761631b3cd4735f     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "fdf798c6845fa0e8aac30738c8d7112eac9d186fcfcde011b6adc9b2ea63bdcf"
```

Here are the UTxOs at the issuer John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0ee5587a23acc39ac6aed41d301d889c66f7d76415c67eeff761631b3cd4735f     0        144434109 lovelace + TxOutDatumNone
```

## Transaction 2. The issuer deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe               \
                        --deposit-account "$ISSUER_ADDRESS"       \
                        --deposit-party "$ISSUER_ADDRESS"         \
                        --deposit-token "$TOKEN_A"                \
                        --deposit-amount "$AMOUNT_A"              \
                        --invalid-before "$NOW"                   \
                        --invalid-hereafter "$((NOW+1*HOUR))"     \
                        --out-file tx-2.marlowe                   \
                        --print-stats
```

```console
Datum size: 845
```

Now the issuer John Fletcher submits the transaction that deposits their tokens.

```
TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 1270465
Size: 15002 / 16384 = 91%
Execution units:
  Memory: 5596202 / 14000000 = 39%
  Steps: 1558092072 / 10000000000 = 15%
```

The contract received the deposit of 300 `01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3.Globe` in the transaction `f4f1418d01e4b27bdb5f68a575edcea07dfe26c9759cb73505dc449b6647f498`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f4f1418d01e4b27bdb5f68a575edcea07dfe26c9759cb73505dc449b6647f498     1        3000000 lovelace + 300 01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3.476c6f6265 + TxOutDatumHash ScriptDataInBabbageEra "346f670e3b0f3c0bacf410c0af7d0191d78d36caa3a87ec5e14d78c2d197be04"
```

Here is the UTxO at the issuer John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f4f1418d01e4b27bdb5f68a575edcea07dfe26c9759cb73505dc449b6647f498     0        145163644 lovelace + TxOutDatumNone
```

## Transaction 3. The Counter-Party chooses to exercise the option

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe            \
                        --choice-name "Exercise Call"          \
                        --choice-party "$COUNTERPARTY_ADDRESS" \
                        --choice-number 1                      \
                        --invalid-before "$NOW"                \
                        --invalid-hereafter "$((NOW+1*HOUR))"  \
                        --out-file tx-3.marlowe                \
                        --print-stats
```

```console
Datum size: 724
```

TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                       \
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
Fee: Lovelace 1221993
Size: 14693 / 16384 = 89%
Execution units:
  Memory: 5226750 / 14000000 = 37%
  Steps: 1431675420 / 10000000000 = 14%
## Transaction 4. The Counter-Party Deposits their Tokens.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe                     \
                        --deposit-account "$COUNTERPARTY_ADDRESS"       \
                        --deposit-party "$COUNTERPARTY_ADDRESS"         \
                        --deposit-token "$TOKEN_B"                      \
                        --deposit-amount "$AMOUNT_B"                    \
                        --invalid-before "$NOW"                         \
                        --invalid-hereafter "$((NOW+8*HOUR))"           \
                        --out-file tx-4.marlowe                         \
                        --print-stats
```

```console
Datum size: 97
Payment 1
  Acccount: Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r""
  Payee: Party (Address ""addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5"")
  Ada: Lovelace {getLovelace = 0}
  01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3."Globe": 300
Payment 2
  Acccount: Address ""addr_test1vr7n0zzth5zycuh972w7rdmh48qur4f3wu6ntn2m2h30dlcvltuy5""
  Payee: Party (Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r"")
  Ada: Lovelace {getLovelace = 0}
  7797b6f5effa8e8c70c2322fc2705804b958bc7c1df2ef8408029fce."Swan": 500
Payment 3
  Acccount: Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r""
  Payee: Party (Address ""addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r"")
  Ada: Lovelace {getLovelace = 3000000}
```

Now the counter-party Thomas Kyd can submit a transaction that deposits their tokens.

```
TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                       \
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
Fee: Lovelace 1430186
Size: 14000 / 16384 = 85%
Execution units:
  Memory: 8348222 / 14000000 = 59%
  Steps: 2182467088 / 10000000000 = 21%
```

The closing of the contract paid 500 `7797b6f5effa8e8c70c2322fc2705804b958bc7c1df2ef8408029fce.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 300 `01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3.Globe` to the counter party Thomas Kyd in the transaction `e0cd1cb99727db0547f2c4e674898e347b1c8f237d65613879bc0b59f3ada471`. There is no UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the issuer party John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e0cd1cb99727db0547f2c4e674898e347b1c8f237d65613879bc0b59f3ada471     1        3000000 lovelace + 500 7797b6f5effa8e8c70c2322fc2705804b958bc7c1df2ef8408029fce.5377616e + TxOutDatumNone
f4f1418d01e4b27bdb5f68a575edcea07dfe26c9759cb73505dc449b6647f498     0        145163644 lovelace + TxOutDatumNone
```

Here are the UTxOs at the counter party Thomas Kyd's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e0cd1cb99727db0547f2c4e674898e347b1c8f237d65613879bc0b59f3ada471     0        145960509 lovelace + TxOutDatumNone
e0cd1cb99727db0547f2c4e674898e347b1c8f237d65613879bc0b59f3ada471     2        1038710 lovelace + 300 01f12e84792c8142eb9dade9c8a3f0639237c679afe48b0132995ba3.476c6f6265 + TxOutDatumNone
```

## Clean Up

```
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
marlowe-cli transaction simple --testnet-magic "$MAGIC"                            \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"           \
                               --tx-in "$TX_2#0"                                   \
			       --tx-in "$TX_4#1"                                   \
                               --tx-out "$BURN_ADDRESS+1400000+$AMOUNT_B $TOKEN_B" \
                               --required-signer "$ISSUER_PAYMENT_SKEY"            \
                               --change-address "$FAUCET_ADDRESS"                  \
                               --out-file /dev/null                                \
                               --submit 600
```

```console
TxId "04b07639c3a3338535d5278534efb8df84486c8961ad456d924de84892c65d19"
```

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ISSUER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
marlowe-cli transaction simple --testnet-magic "$MAGIC"                            \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"           \
                               --tx-in "$TX_4#0"                                   \
			       --tx-in "$TX_4#2"                                   \
                               --tx-out "$BURN_ADDRESS+1400000+$AMOUNT_A $TOKEN_A" \
                               --required-signer "$COUNTERPARTY_PAYMENT_SKEY"      \
                               --change-address "$FAUCET_ADDRESS"                  \
                               --out-file /dev/null                                \
                               --submit 600
```

```console
TxId "f29a433f08fc80622017fde686843be5f334c2697630ee6138422d8104840425"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$COUNTERPARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
