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

The issuer John Fletcher is the minimum-ADA provider and has the address `addr_test1vp29vpdavyag98wwtv6qlpvxshpljxda08dcvg3kwzjy4ls2qrl4k` and public-key hash `545605bd613a829dce5b340f858685c3f919bd79db86223670a44afe`. They have the following UTxOs in their wallet:

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
2d8c66ee11fa8b7e3bfcef08928c746746546859401e52e88a4bac3c0da0674e     0        99646609870 lovelace + TxOutDatumNone
2d8c66ee11fa8b7e3bfcef08928c746746546859401e52e88a4bac3c0da0674e     1        2000000 lovelace + 2 81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.JF + TxOutDatumNone
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
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[0].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-a.json
TX_0_A_TOKEN=$(jq -r '.key' utxo-0-a.json | head -n 1)
CURRENCY_SYMBOL_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-a.json | head -n 1)
TOKEN_NAME_A=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-a.json | head -n 1)
TOKEN_A="$CURRENCY_SYMBOL_A.$TOKEN_NAME_A"
AMOUNT_A=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-a.json | head -n 1)
```

John Fletcher will spend the UTxOs `2d8c66ee11fa8b7e3bfcef08928c746746546859401e52e88a4bac3c0da0674e#0` and `2d8c66ee11fa8b7e3bfcef08928c746746546859401e52e88a4bac3c0da0674e#1`. They will trade 2 of `81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.JF`.

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

The counter party Thomas Kyd has the address `addr_test1vqp8wzj0pxpxra2fsq7mm7nwkctkcya9pvtk66vxg90ka9qfj8eem` and public-key hash `02770a4f098261f549803dbdfa6eb6176c13a50b176d6986415f6e94`. They have the following UTxOs in their wallet:

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
7960cabb771f809b0631868a1cce277aa525a5063c4958d9a3973eb9fef6a86e     0        99953609920 lovelace + TxOutDatumNone
7960cabb771f809b0631868a1cce277aa525a5063c4958d9a3973eb9fef6a86e     1        2000000 lovelace + 2 81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.TK + TxOutDatumNone
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
| jq '. | to_entries | .[] | select((.value.value | length) == 2) | select((.value.value | to_entries | .[0].value | to_entries | .[0] | .value) > 1)' \
> utxo-0-b.json
TX_0_B_TOKEN=$(jq -r '.key' utxo-0-b.json | head -n 1)
CURRENCY_SYMBOL_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .key' utxo-0-b.json | head -n 1)
TOKEN_NAME_B=$(jq -r '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .key' utxo-0-b.json | head -n 1)
TOKEN_B="$CURRENCY_SYMBOL_B.$TOKEN_NAME_B"
AMOUNT_B=$(jq '.value.value | to_entries | .[] | select(.key != "lovelace") | .value | to_entries | .[] | .value' utxo-0-b.json | head -n 1)
```

Thomas Kyd will spend the UTxOs `7960cabb771f809b0631868a1cce277aa525a5063c4958d9a3973eb9fef6a86e#0` and `7960cabb771f809b0631868a1cce277aa525a5063c4958d9a3973eb9fef6a86e#1`. They will trade 2 of `81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.TK`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
MINUTE="$((60*1000))"
SECOND="1000"
```

The tip is at slot 2419234. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2419236. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
ISSUE_TIMEOUT=$((NOW+2*MINUTE-1*SECOND))
MATURITY_TIMEOUT=$((NOW+2*MINUTE))
SETTLEMENT_TIMEOUT=$((NOW+12*HOUR))
```

The contract has a minimum-ADA requirement and three timeouts. It also specifies that the issuer John Fletcher will put 2 of `81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.JF` before Tue, 15 Mar 2022 12:56:33 +0000 into the contract and if the counter-party Thomas Kyd exercises the option for 2 of `81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.TK` after Tue, 15 Mar 2022 12:56:34 +0000 and before Wed, 16 Mar 2022 00:54:34 +0000.

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

The contract received the minimum ADA of 3000000 lovelace from the issuer  in the transaction `b098761a13a2f8f1a58c7e08241f8b5cfb9fdb1b80383750d5fed1109bc0cde3`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b098761a13a2f8f1a58c7e08241f8b5cfb9fdb1b80383750d5fed1109bc0cde3     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "9f6537b43e02c67e4a00d723147131bd68c9a4589c55341006d2d5023bd891d6"
```

Here are the UTxOs at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b098761a13a2f8f1a58c7e08241f8b5cfb9fdb1b80383750d5fed1109bc0cde3     0        99643400457 lovelace + TxOutDatumNone
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
                        --invalid-hereafter "$((NOW+2*MINUTE-2*SECOND))"     \
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
Fee: Lovelace 1158179
Size: 13909 / 32768 = 42%
Execution units:
  Memory: 4414780 / 30000000 = 14%
  Steps: 1614429908 / 10000000000 = 16%
```

The contract received the deposit of 2 `81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.JF` in the transaction `e6dcae680a989bcdbdea1598aa38cee60881f00abf8e7e63a2aea04e6c81f2fd`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e6dcae680a989bcdbdea1598aa38cee60881f00abf8e7e63a2aea04e6c81f2fd     1        3000000 lovelace + 2 81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.JF + TxOutDatumHash ScriptDataInAlonzoEra "6e974bcdf073b9c500e2d6181936ef1de80a503f811bfc00217975a3123e8023"
```

Here is the UTxO at the issuer John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e6dcae680a989bcdbdea1598aa38cee60881f00abf8e7e63a2aea04e6c81f2fd     0        99644242278 lovelace + TxOutDatumNone
```

## Transaction 3. Wait until expiry (2 minutes) and advance contract

```
sleep 2m
TX_1_A_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$ISSUER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
```

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --invalid-before "$((NOW+2*MINUTE))"  \
                        --invalid-hereafter "$((NOW+8*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats
```

```console
Datum size: 684
```

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_1_A_ADA"          \
                        --tx-in "$TX_1_A_ADA"                     \
                        --required-signer "$ISSUER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$ISSUER_ADDRESS"        \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1128047
Size: 13691 / 32768 = 41%
Execution units:
  Memory: 4249040 / 30000000 = 14%
  Steps: 1523824157 / 10000000000 = 15%
```

## Transaction 4. The Counter-Party chooses to exercise the option

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe                   \
                        --choice-name "Exercise Call"                 \
                        --choice-party "PK=$COUNTERPARTY_PUBKEYHASH"  \
                        --choice-number 1                             \
                        --invalid-before "$((NOW+2*MINUTE+1*SECOND))" \
                        --invalid-hereafter "$((NOW+8*HOUR))"         \
                        --out-file tx-4.marlowe                       \
                        --print-stats
```

```console
Datum size: 577
```

TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-3.marlowe                 \
                        --tx-in-marlowe "$TX_3"#1                      \
                        --tx-in-collateral "$TX_0_B_ADA"               \
                        --tx-in "$TX_0_B_ADA"                          \
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
Fee: Lovelace 1141573
Size: 13632 / 32768 = 41%
Execution units:
  Memory: 4457402 / 30000000 = 14%
  Steps: 1580679251 / 10000000000 = 15%
## Transaction 5. The Counter-Party Deposits their Tokens.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe                     \
                        --deposit-account "PK=$COUNTERPARTY_PUBKEYHASH" \
                        --deposit-party "PK=$COUNTERPARTY_PUBKEYHASH"   \
                        --deposit-token "$TOKEN_B"                      \
                        --deposit-amount "$AMOUNT_B"                    \
                        --invalid-before "$((NOW+2*MINUTE+1*SECOND))"   \
                        --invalid-hereafter "$((NOW+8*HOUR))"           \
                        --out-file tx-5.marlowe                         \
                        --print-stats
```

```console
Datum size: 76
Payment 1
  Acccount: PK "545605bd613a829dce5b340f858685c3f919bd79db86223670a44afe"
  Payee: Party (PK "02770a4f098261f549803dbdfa6eb6176c13a50b176d6986415f6e94")
  Ada: 0.000000
  81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612."JF": 2
Payment 2
  Acccount: PK "02770a4f098261f549803dbdfa6eb6176c13a50b176d6986415f6e94"
  Payee: Party (PK "545605bd613a829dce5b340f858685c3f919bd79db86223670a44afe")
  Ada: 0.000000
  81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612."TK": 2
Payment 3
  Acccount: PK "545605bd613a829dce5b340f858685c3f919bd79db86223670a44afe"
  Payee: Party (PK "545605bd613a829dce5b340f858685c3f919bd79db86223670a44afe")
  Ada: 3.000000
```

Now the counter-party Thomas Kyd can submit a transaction that deposits their tokens.

```
TX_4_B_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$COUNTERPARTY_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_5=$(
marlowe-cli run execute "${MAGIC[@]}"                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                        --marlowe-in-file tx-4.marlowe                 \
                        --tx-in-marlowe "$TX_4"#1                      \
                        --tx-in-collateral "$TX_4_B_ADA"               \
                        --tx-in "$TX_4_B_ADA"                          \
                        --tx-in "$TX_0_B_TOKEN"                        \
                        --required-signer "$COUNTERPARTY_PAYMENT_SKEY" \
                        --marlowe-out-file tx-5.marlowe                \
                        --change-address "$COUNTERPARTY_ADDRESS"       \
                        --out-file tx-5.raw                            \
                        --print-stats                                  \
                        --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                       \
)
```

```console
Fee: Lovelace 1227251
Size: 13079 / 32768 = 39%
Execution units:
  Memory: 5800610 / 30000000 = 19%
  Steps: 1969910005 / 10000000000 = 19%
```

The closing of the contract paid 2 `81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.TK` to the first party John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract, and it paid 2 `81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.JF` to the counter party Thomas Kyd in the transaction `0630905ebdfc69f7c022e656171750fb44297906bcd69cdc67994bb67b51fd2f`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the issuer party John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ISSUER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ae87467e693e12d91740ac0f59f06bbff8e710255b0cc5373529683c60c38ad3     0        99643114231 lovelace + TxOutDatumNone
```

Here are the UTxOs at the counter party Thomas Kyd's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$COUNTERPARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0630905ebdfc69f7c022e656171750fb44297906bcd69cdc67994bb67b51fd2f     0        99951896298 lovelace + TxOutDatumNone
0630905ebdfc69f7c022e656171750fb44297906bcd69cdc67994bb67b51fd2f     1        1344798 lovelace + 2 81ce51ecf10df499a855479b38bf8662ee44c758b09483ab23597612.JF + TxOutDatumNone
```

