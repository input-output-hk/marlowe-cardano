# Test of a Covered Call Contract

[This option contract](../../../marlowe-contracts/src/Marlowe/Contracts/Options.hs) transfers a token if the counter-party exercises the option.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the two parties: to do this, set the environment variables `ISSUER_PREFIX` and `COUNTERPARTY_PREFIX` where they appear below.

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

#### The Issuer

```
ISSUER_PREFIX="$TREASURY/john-fletcher"
ISSUER_NAME="John Fletcher"
ISSUER_PAYMENT_SKEY="$ISSUER_PREFIX".skey
ISSUER_PAYMENT_VKEY="$ISSUER_PREFIX".vkey
ISSUER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$ISSUER_PAYMENT_VKEY" \
)
ISSUER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$ISSUER_PAYMENT_VKEY"
)
```

The issuer John Fletcher is the minimum-ADA provider and has the address `addr_test1vrsuucqupq5xz7dw0tnw7w3cyck5zyk2kpnr56xlhr57m3gll8m84` and public-key hash `e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5`. They have the following UTxOs in their wallet:

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
24ec470099dd87cb54e909843436af391cdb770e54b17fea5c48bd3d04c99cf3     0        12963862085 lovelace + TxOutDatumNone
24ec470099dd87cb54e909843436af391cdb770e54b17fea5c48bd3d04c99cf3     1        2000000 lovelace + 12000 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.476c6f6265 + TxOutDatumNone
24ec470099dd87cb54e909843436af391cdb770e54b17fea5c48bd3d04c99cf3     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.4a46 + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with exactly one type of native token.

```
TX_0_A_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$ISSUER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$ISSUER_ADDRESS"                                                                                                     \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[1].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-a.json
TX_0_A_TOKEN=$(jq -r '.key' utxo-0-a.json | head -n 1)
CURRENCY_SYMBOL_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-a.json | head -n 1)
TOKEN_NAME_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-a.json | head -n 1 | sed -e 's/\(.*\)/\U\1/' | basenc --decode --base16)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
AMOUNT_A=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-a.json | head -n 1)
```

John Fletcher will spend the UTxOs `24ec470099dd87cb54e909843436af391cdb770e54b17fea5c48bd3d04c99cf3#0` and `24ec470099dd87cb54e909843436af391cdb770e54b17fea5c48bd3d04c99cf3#1`. They will trade 12000 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe`.

### The Counter-party

```
COUNTERPARTY_PREFIX="$TREASURY/thomas-kyd"
COUNTERPARTY_NAME="Thomas Kyd"
COUNTERPARTY_PAYMENT_SKEY="$COUNTERPARTY_PREFIX".skey
COUNTERPARTY_PAYMENT_VKEY="$COUNTERPARTY_PREFIX".vkey
COUNTERPARTY_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                                \
                            --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY" \
)
COUNTERPARTY_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$COUNTERPARTY_PAYMENT_VKEY"
)
```

The counter party Thomas Kyd has the address `addr_test1vpkedn0l6slaj8fzkx08qyce9vpreuu3spmnlxkepx4757grxzyzf` and public-key hash `6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79`. They have the following UTxOs in their wallet:

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
32c8eb8ed6b323544ff1367ea2546c151a80c1fb199568268f6e4f2edd3a2b94     0        11983669915 lovelace + TxOutDatumNone
32c8eb8ed6b323544ff1367ea2546c151a80c1fb199568268f6e4f2edd3a2b94     1        2000000 lovelace + 4500 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.5377616e + TxOutDatumNone
```

We select the UTxO with the most ADA and another UTxO with exactly one type of native token.

```
TX_0_B_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$COUNTERPARTY_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
cardano-cli query utxo "${MAGIC[@]}"                                                                                                                   \
                       --address "$COUNTERPARTY_ADDRESS"                                                                                               \
                       --out-file /dev/stdout                                                                                                          \
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[1].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-b.json
TX_0_B_TOKEN=$(jq -r '.key' utxo-0-b.json | head -n 1)
CURRENCY_SYMBOL_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-b.json | head -n 1)
TOKEN_NAME_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-b.json | head -n 1 | sed -e 's/\(.*\)/\U\1/' | basenc --decode --base16)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
AMOUNT_B=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-b.json | head -n 1)
```

Thomas Kyd will spend the UTxOs `32c8eb8ed6b323544ff1367ea2546c151a80c1fb199568268f6e4f2edd3a2b94#0` and `32c8eb8ed6b323544ff1367ea2546c151a80c1fb199568268f6e4f2edd3a2b94#1`. They will trade 4500 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"
SECOND="1000"
```

The tip is at slot 2452255. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2452258. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
ISSUE_TIMEOUT=$((NOW+5*MINUTE-1*SECOND))
MATURITY_TIMEOUT=$((NOW+5*MINUTE))
SETTLEMENT_TIMEOUT=$((NOW+12*HOUR))
```

The contract has a minimum-ADA requirement and three timeouts. It also specifies that the issuer John Fletcher will put 12000 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` before Tue, 15 Mar 2022 22:09:54 +0000 into the contract and if the counter-party Thomas Kyd exercises the option for 4500 of `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` after Tue, 15 Mar 2022 22:09:55 +0000 and before Wed, 16 Mar 2022 10:04:55 +0000.

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
Validator size: 11932
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24384187, exBudgetMemory = ExMemory 82000}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wzw0y8gpcpx0rksg82w37ddrcntkrx94klevdf269g5news45dfzm`.

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
Fee: Lovelace 209413
Size: 985 / 32768 = 3%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the issuer  in the transaction `3fcfb24ae74583d0a458604df9d80837c1cd4c5f246ac9478c16e9a2c0f8850d`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3fcfb24ae74583d0a458604df9d80837c1cd4c5f246ac9478c16e9a2c0f8850d     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "b8b2b5af212e11391696202e665b3ab17a23d21435d4dbe84f180c40688c7b7d"
```

Here are the UTxOs at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3fcfb24ae74583d0a458604df9d80837c1cd4c5f246ac9478c16e9a2c0f8850d     0        12960652672 lovelace + TxOutDatumNone
```

## Transaction 2. The issuer deposits Tokens into the Contract.

First we compute the Marlowe input required to deposit the tokens.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe               \
                        --deposit-account "PK=$ISSUER_PUBKEYHASH" \
                        --deposit-party "PK=$ISSUER_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_A"                \
                        --deposit-amount "$AMOUNT_A"              \
                        --invalid-before "$((NOW-300000))"        \
                        --invalid-hereafter "$((NOW+5*MINUTE-2*SECOND))"     \
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
Fee: Lovelace 1154885
Size: 13917 / 32768 = 42%
Execution units:
  Memory: 4373046 / 30000000 = 14%
  Steps: 1597258917 / 10000000000 = 15%
```

The contract received the deposit of 12000 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` in the transaction `32defd1b56bf63a7ee2c64c131046b6cc6e48f3544fa60c57dafc4b4afc98dc4`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
32defd1b56bf63a7ee2c64c131046b6cc6e48f3544fa60c57dafc4b4afc98dc4     1        3000000 lovelace + 12000 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.476c6f6265 + TxOutDatumHash ScriptDataInAlonzoEra "24381d43dfaac28b1f137c516ab48ebf278ba2564eed1c813ae8c33d82364b88"
```

Here is the UTxO at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
32defd1b56bf63a7ee2c64c131046b6cc6e48f3544fa60c57dafc4b4afc98dc4     0        12961497787 lovelace + TxOutDatumNone
```

## Transaction 3. The Counter-Party chooses to exercise the option

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                   \
                        --choice-name "Exercise Call"                 \
                        --choice-party "PK=$COUNTERPARTY_PUBKEYHASH"  \
                        --choice-number 1                             \
                        --invalid-before "$((NOW-300000))"        \
                        --invalid-hereafter "$((NOW+5*MINUTE-2*SECOND))"     \
                        --out-file tx-3.marlowe                       \
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
Fee: Lovelace 1143281
Size: 13664 / 32768 = 41%
Execution units:
  Memory: 4461202 / 30000000 = 14%
  Steps: 1581810625 / 10000000000 = 15%
## Wait until settlement (5 minutes) and advance contract

```
sleep 5m
```

## Transaction 4. The Counter-Party Deposits their Tokens.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe                     \
                        --deposit-account "PK=$COUNTERPARTY_PUBKEYHASH" \
                        --deposit-party "PK=$COUNTERPARTY_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_B"                      \
                        --deposit-amount "$AMOUNT_B"                    \
                        --invalid-before "$((NOW+5*MINUTE+1*SECOND))"   \
                        --invalid-hereafter "$((NOW+8*HOUR))"           \
                        --out-file tx-4.marlowe                         \
                        --print-stats
```

```console
Datum size: 76
Payment 1
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")
  Ada: 0.000000
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Globe": 12000
Payment 2
  Acccount: PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
  Ada: 0.000000
  8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d."Swan": 4500
Payment 3
  Acccount: PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5"
  Payee: Party (PK "e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5")
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
Fee: Lovelace 1228006
Size: 13103 / 32768 = 39%
Execution units:
  Memory: 5796810 / 30000000 = 19%
  Steps: 1968778631 / 10000000000 = 19%
```

The closing of the contract paid 4500 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 12000 `8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe` to the counter party Thomas Kyd in the transaction `bd8589340529b1dfedc9371229a941bfde249b1dcf0a0fb9480df51282805ac7`. There is no UTxO at the contract address:

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
32defd1b56bf63a7ee2c64c131046b6cc6e48f3544fa60c57dafc4b4afc98dc4     0        12961497787 lovelace + TxOutDatumNone
bd8589340529b1dfedc9371229a941bfde249b1dcf0a0fb9480df51282805ac7     2        3000000 lovelace + 4500 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.5377616e + TxOutDatumNone
```

Here are the UTxOs at the counter party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bd8589340529b1dfedc9371229a941bfde249b1dcf0a0fb9480df51282805ac7     0        11981953830 lovelace + TxOutDatumNone
bd8589340529b1dfedc9371229a941bfde249b1dcf0a0fb9480df51282805ac7     1        1344798 lovelace + 12000 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.476c6f6265 + TxOutDatumNone
```

