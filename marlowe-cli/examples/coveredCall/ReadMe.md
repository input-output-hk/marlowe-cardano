# Test of a Covered Call Contract

[This option contract](../../../marlowe-contracts/src/Marlowe/Contracts/Options.hs) transfers a token if the counter-party exercises the option.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

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

The tip is at slot 4871334. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4871353. Tests may fail if this is not the case.

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
TxId "360fb70b9771d5582422fb73e456c151cdda3e616b91be089e2fa0ca0229d6ce"
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
68b679c66cf18f6ce264ab7218d921efe14775b0f07603ef4e919e04873f87c6     0        147651442 lovelace + TxOutDatumNone
68b679c66cf18f6ce264ab7218d921efe14775b0f07603ef4e919e04873f87c6     1        2000000 lovelace + 300 d4839c6fb15990b6780403574c5a53f959609db102512322662581b8.476c6f6265 + TxOutDatumNone
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

John Fletcher will spend the UTxOs `68b679c66cf18f6ce264ab7218d921efe14775b0f07603ef4e919e04873f87c6#0` and `68b679c66cf18f6ce264ab7218d921efe14775b0f07603ef4e919e04873f87c6#1`. They will trade 300 of `d4839c6fb15990b6780403574c5a53f959609db102512322662581b8.Globe`.

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
TxId "c4e2cd496a6868f65ad8ab3394a76da98599151744b63a48616df45c6356f3fa"
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

The counterparty Thomas Kyd has the address `addr_test1vrqhqzkgnzekjy30fpxff3vcap82a8vw75m30yvpv0sawgg0pqxf8` and public-key hash `c1700ac898b369122f484c94c598e84eae9d8ef53717918163e1d721`. They have the following UTxOs in their wallet:

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
99d9dc4a61883c69bf004f84a6aae66eed43c108d2f3aae6ecea33e78abdb9b0     0        147651574 lovelace + TxOutDatumNone
99d9dc4a61883c69bf004f84a6aae66eed43c108d2f3aae6ecea33e78abdb9b0     1        2000000 lovelace + 500 cfefd2163839fc940d8e483a29d67041829173f7655b13e33df09d49.5377616e + TxOutDatumNone
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

Thomas Kyd will spend the UTxOs `99d9dc4a61883c69bf004f84a6aae66eed43c108d2f3aae6ecea33e78abdb9b0#0` and `99d9dc4a61883c69bf004f84a6aae66eed43c108d2f3aae6ecea33e78abdb9b0#1`. They will trade 500 of `cfefd2163839fc940d8e483a29d67041829173f7655b13e33df09d49.Swan`.

The tip is at slot 4871334. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4871499. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
ISSUE_TIMEOUT=$((NOW+5*HOUR))
MATURITY_TIMEOUT=$((NOW+0*HOUR))
SETTLEMENT_TIMEOUT=$((NOW+12*HOUR))
```

The contract has a minimum-ADA requirement and three timeouts. It also specifies that the issuer John Fletcher will put 300 of `d4839c6fb15990b6780403574c5a53f959609db102512322662581b8.Globe` before Wed, 13 Apr 2022 03:02:54 +0000 into the contract and if the counter-party Thomas Kyd exercises the option for 500 of `cfefd2163839fc940d8e483a29d67041829173f7655b13e33df09d49.Swan` after Tue, 12 Apr 2022 22:02:54 +0000 and before Wed, 13 Apr 2022 10:02:54 +0000.

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

The contract received the minimum ADA of 3000000 lovelace from the issuer John Fletcher in the transaction `0e573624b1f9b37380f0857a0dc9b25b5363329a4cc98ab4c43b5b21ff2abeba`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0e573624b1f9b37380f0857a0dc9b25b5363329a4cc98ab4c43b5b21ff2abeba     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "8554b0fc07e429bf293eb70f353cbe87c63cf511b3443417453f6711d909871c"
```

Here are the UTxOs at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0e573624b1f9b37380f0857a0dc9b25b5363329a4cc98ab4c43b5b21ff2abeba     0        144441589 lovelace + TxOutDatumNone
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
Fee: Lovelace 1216256
Size: 14345 / 32768 = 43%
Execution units:
  Memory: 4905364 / 30000000 = 16%
  Steps: 1761265438 / 10000000000 = 17%
```

The contract received the deposit of 300 `d4839c6fb15990b6780403574c5a53f959609db102512322662581b8.Globe` in the transaction `9d45c27155a1c8b5ae596f0e5f9a469f76be19ceca2a9ff92b78ecccab032060`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d45c27155a1c8b5ae596f0e5f9a469f76be19ceca2a9ff92b78ecccab032060     1        3000000 lovelace + 300 d4839c6fb15990b6780403574c5a53f959609db102512322662581b8.476c6f6265 + TxOutDatumHash ScriptDataInAlonzoEra "c5fdd7f5c776ede89263931f9a3dde56265f997e8ea5209b54071a04effd19bb"
```

Here is the UTxO at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9d45c27155a1c8b5ae596f0e5f9a469f76be19ceca2a9ff92b78ecccab032060     0        145225333 lovelace + TxOutDatumNone
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
Fee: Lovelace 1176963
Size: 14078 / 32768 = 42%
Execution units:
  Memory: 4656706 / 30000000 = 15%
  Steps: 1639848503 / 10000000000 = 16%
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
  Payee: Party (PK "c1700ac898b369122f484c94c598e84eae9d8ef53717918163e1d721")
  Ada: 0.000000
  d4839c6fb15990b6780403574c5a53f959609db102512322662581b8."Globe": 300
Payment 2
  Acccount: PK "c1700ac898b369122f484c94c598e84eae9d8ef53717918163e1d721"
  Payee: Party (PK "1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf")
  Ada: 0.000000
  cfefd2163839fc940d8e483a29d67041829173f7655b13e33df09d49."Swan": 500
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
Fee: Lovelace 1261349
Size: 13517 / 32768 = 41%
Execution units:
  Memory: 5987124 / 30000000 = 19%
  Steps: 2026280013 / 10000000000 = 20%
```

The closing of the contract paid 500 `cfefd2163839fc940d8e483a29d67041829173f7655b13e33df09d49.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 300 `d4839c6fb15990b6780403574c5a53f959609db102512322662581b8.Globe` to the counter party Thomas Kyd in the transaction `597db5b527b7059f57b4083af967038725333b28f6a0b4c897dd302e686e8e1d`. There is no UTxO at the contract address:

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
597db5b527b7059f57b4083af967038725333b28f6a0b4c897dd302e686e8e1d     2        3000000 lovelace + 500 cfefd2163839fc940d8e483a29d67041829173f7655b13e33df09d49.5377616e + TxOutDatumNone
9d45c27155a1c8b5ae596f0e5f9a469f76be19ceca2a9ff92b78ecccab032060     0        145225333 lovelace + TxOutDatumNone
```

Here are the UTxOs at the counter party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
597db5b527b7059f57b4083af967038725333b28f6a0b4c897dd302e686e8e1d     0        145868464 lovelace + TxOutDatumNone
597db5b527b7059f57b4083af967038725333b28f6a0b4c897dd302e686e8e1d     1        1344798 lovelace + 300 d4839c6fb15990b6780403574c5a53f959609db102512322662581b8.476c6f6265 + TxOutDatumNone
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
TxId "dc51e9165e08daf9237088d98c5c997247011651804041cf0acb3885d8521aea"
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
TxId "f66dfc14b814068d77baa672d2a8d71e90126261cc942267f4b0400a33c8605c"
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
PolicyID "d4839c6fb15990b6780403574c5a53f959609db102512322662581b8"
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
PolicyID "cfefd2163839fc940d8e483a29d67041829173f7655b13e33df09d49"
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
TxId "9b1bb31cadfd3f51007f514812556b95f6e643b5078efba819038f772f4c3583"
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
TxId "fc16d0fdb712e8e6239842ea1c209e0caff444b7be3c67bed0c83fd3091fc89d"
