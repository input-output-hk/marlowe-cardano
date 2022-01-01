# Test of a Zero-Coupon Bond

[This zero-coupon bond](../../src/Language/Marlowe/CLI/Examples/ZeroCouponBond.hs) has one party borrow and another pay back with interest. It uses role tokens.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed
* xargs

Signing and verification keys must be provided below for the two parties: to do this, set the environment variables `LENDER_PREFIX` and `BORROWER_PREFIX` where they appear below.

The two parties' wallets must have exactly one UTxO with their role token. The currency symbol for the role tokens must be set below in `ROLE_CURRENCY`.

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

### Role Currency

Set the role currency for the validator.

```
ROLE_CURRENCY=8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d
```

#### The Lender

```
LENDER_PREFIX="$TREASURY/john-fletcher"
LENDER_NAME="John Fletcher"
LENDER_ROLE=JohnFletcher
LENDER_TOKEN="$ROLE_CURRENCY.$LENDER_ROLE"
LENDER_PAYMENT_SKEY="$LENDER_PREFIX".skey
LENDER_PAYMENT_VKEY="$LENDER_PREFIX".vkey
LENDER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$LENDER_PAYMENT_VKEY" \
)
LENDER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$LENDER_PAYMENT_VKEY"
)
```

The lender John Fletcher is the minimum-ADA provider and has the address `addr_test1vrsuucqupq5xz7dw0tnw7w3cyck5zyk2kpnr56xlhr57m3gll8m84` and public-key hash `e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5`. They have the following UTxOs in their wallet:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c0ae34d96c1eb9b8dd9b7eb6dee764a17d3b5dd208d0d6d87be31365ae07a66d     2        3000000 lovelace + 100 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
cc36a68d8862096774b9ea49cd1fe0c0787cd3d0c4d98085e5662727f527db45     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JohnFletcher + TxOutDatumNone
e9abbcd0ca815bbbf37e9c92712db751a610c612f93520b6118781cbdac1d346     0        1017907012 lovelace + TxOutDatumNone
```

We select the UTxO with the lender John Fletcher's role token.

```
TX_0_LENDER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$LENDER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_LENDER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                      \
                       --address "$LENDER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                             \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$LENDER_ROLE"'" == 1) | .key' \
)
```

John Fletcher will spend the UTxOs `e9abbcd0ca815bbbf37e9c92712db751a610c612f93520b6118781cbdac1d346#0` and `cc36a68d8862096774b9ea49cd1fe0c0787cd3d0c4d98085e5662727f527db45#2`.

### The Borrower

```
BORROWER_PREFIX="$TREASURY/thomas-middleton"
BORROWER_NAME="Thomas Middleton"
BORROWER_ROLE=ThomasMiddleton
BORROWER_TOKEN="$ROLE_CURRENCY.$BORROWER_ROLE"
BORROWER_PAYMENT_SKEY="$BORROWER_PREFIX".skey
BORROWER_PAYMENT_VKEY="$BORROWER_PREFIX".vkey
BORROWER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                            \
                            --payment-verification-key-file "$BORROWER_PAYMENT_VKEY" \
)
BORROWER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$BORROWER_PAYMENT_VKEY"
)
```

The borrower Thomas Middleton has the address `` and public-key hash `90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9`. They have the following UTxOs in their wallet:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c0ae34d96c1eb9b8dd9b7eb6dee764a17d3b5dd208d0d6d87be31365ae07a66d     1        1344798 lovelace + 200 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Swan + TxOutDatumNone
f66075e9a8037337364db714760b64bfe12e7aa17e8496899af5d4da20c1c0f5     0        1015798743 lovelace + TxOutDatumNone
f66075e9a8037337364db714760b64bfe12e7aa17e8496899af5d4da20c1c0f5     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
```

We select the UTxO with the borrower Thomas Middleton's role token.

```
TX_0_BORROWER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$BORROWER_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_BORROWER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                      \
                       --address "$BORROWER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                             \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$BORROWER_ROLE"'" == 1) | .key' \
)
```

Thomas Middleton will spend the UTxOs `f66075e9a8037337364db714760b64bfe12e7aa17e8496899af5d4da20c1c0f5#0` and `f66075e9a8037337364db714760b64bfe12e7aa17e8496899af5d4da20c1c0f5#2`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 2858891. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2858892. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
PRINCIPAL=100000000
INTEREST=5000000
LENDING_DEADLINE=$(($TIP+12*3600))
REPAYMENT_DEADLINE=$(($TIP+24*3600))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 100000000 ADA before slot 2902091 and the borrower will repay the principal and interest of 5000000 ADA before slot .

We create the contract for the previously specified parameters.

```
marlowe-cli template zcb --minimum-ada "$MINIMUM_ADA"               \
                         --lender "Role=$LENDER_ROLE"               \
                         --borrower "Role=$BORROWER_ROLE"           \
                         --principal "$PRINCIPAL"                   \
                         --interest "$INTEREST"                     \
                         --lending-deadline "$LENDING_DEADLINE"     \
                         --repayment-deadline "$REPAYMENT_DEADLINE" \
                         --out-contract-file tx-1.contract          \
                         --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA.

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

```
marlowe-cli run initialize "${MAGIC[@]}"                     \
                           --slot-length "$SLOT_LENGTH"      \
                           --slot-offset "$SLOT_OFFSET"      \
                           --roles-currency "$ROLE_CURRENCY" \
                           --contract-file tx-1.contract     \
                           --state-file    tx-1.state        \
                           --out-file      tx-1.marlowe      \
                           --print-stats
```

```console
Validator size: 13877
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 37484307, exBudgetMemory = ExMemory 126000}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wpyajj4ln6eg93ktpqxlxag0ax4u9zsz9dhp47e3aa3ph7ceer3z9`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpt3m3hnfzystzp6x3n5v4q0jsj5hca4z5drs30us07pe8sslxxqd`.

The lender John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace requiredd for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --tx-in "$TX_0_LENDER_ADA"                \
                        --required-signer "$LENDER_PAYMENT_SKEY"  \
                        --marlowe-out-file tx-1.marlowe           \
                        --change-address "$LENDER_ADDRESS"        \
                        --out-file tx-1.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 191241
Size: 572 / 32768 = 1%
Execution units:
  Memory: 0 / 10000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the lender John Fletcher in the transaction `bf0fdd4a85bbc88013ffaa4f3147b2c3880019ee916b9864e1d370209f182939`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bf0fdd4a85bbc88013ffaa4f3147b2c3880019ee916b9864e1d370209f182939     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "16d5f32debbe35d9d140f452d2055f45dc2a164af6f3b2bd45a90bb75c8eef37"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bf0fdd4a85bbc88013ffaa4f3147b2c3880019ee916b9864e1d370209f182939     0        1014715771 lovelace + TxOutDatumNone
```

## Transaction 2. Lender Deposits the Loan Amount

First we compute the Marlowe input required to deposit the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe            \
                        --deposit-account "Role=$LENDER_ROLE"  \
                        --deposit-party "Role=$LENDER_ROLE"    \
                        --deposit-amount "$PRINCIPAL"          \
                        --invalid-before "$TIP"                \
                        --invalid-hereafter "$(($TIP+4*3600))" \
                        --out-file tx-2.marlowe                \
                        --print-stats
```

```console
Datum size: 214
Payment 1
  Acccount: "JohnFletcher"
  Payee: Party "ThomasMiddleton"
  Ada: 100.000000
```

Now the lender John Fletcher submits the transaction that deposits the loan amount.

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                                           \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                        --marlowe-in-file tx-1.marlowe                          \
                        --tx-in-marlowe "$TX_1"#1                               \
                        --tx-in-collateral "$TX_1"#0                            \
                        --tx-in "$TX_1"#0                                       \
                        --tx-in "$TX_0_LENDER_TOKEN"                            \
                        --required-signer "$LENDER_PAYMENT_SKEY"                \
                        --marlowe-out-file tx-2.marlowe                         \
                        --tx-out "$LENDER_ADDRESS+$MINIMUM_ADA+1 $LENDER_TOKEN" \
                        --change-address "$LENDER_ADDRESS"                      \
                        --out-file tx-2.raw                                     \
                        --print-stats                                           \
                        --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                \
)
```

```console
Fee: Lovelace 1366805
Size: 15024 / 32768 = 45%
Execution units:
  Memory: 6443014 / 10000000 = 64%
  Steps: 2204398755 / 10000000000 = 22%
```

The contract passed the deposit of 100000000 ADA in the transaction `41aab54b5f6d32dbd57b8bad7ff7dd6528bc0d1e3e0b6ff9b352907315f914be` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
41aab54b5f6d32dbd57b8bad7ff7dd6528bc0d1e3e0b6ff9b352907315f914be     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "6d273c2489c6f962e495576214557e0896489311e96813c4c4fc07bbff3e9d35"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
41aab54b5f6d32dbd57b8bad7ff7dd6528bc0d1e3e0b6ff9b352907315f914be     0        913348966 lovelace + TxOutDatumNone
41aab54b5f6d32dbd57b8bad7ff7dd6528bc0d1e3e0b6ff9b352907315f914be     3        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JohnFletcher + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
41aab54b5f6d32dbd57b8bad7ff7dd6528bc0d1e3e0b6ff9b352907315f914be     2        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "64fc25b1768e6adb1ca8995c543e73122af99c55de14b822df78a2723ce82390"
```

## Transaction 3. Lender Withdraws Loan.

The lender John Fletcher submits a transaction to withdraw the loan from the role address.

```
TX_3=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                               \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                         --marlowe-file tx-2.marlowe                                 \
                         --role-name "$BORROWER_ROLE"                                \
                         --tx-in "$TX_0_BORROWER_TOKEN"                              \
                         --tx-in "$TX_0_BORROWER_ADA"                                \
                         --tx-in-collateral "$TX_0_BORROWER_ADA"                     \
                         --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                         --change-address "$BORROWER_ADDRESS"                        \
                         --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                         --out-file tx-3.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)
```

```console
Fee: Lovelace 456949
Size: 3314 / 32768 = 10%
Execution units:
  Memory: 1607210 / 10000000 = 16%
  Steps: 601218385 / 10000000000 = 6%
```

There is no UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the borrower Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a743a4093613a0fbcd4eee46df97a9063f4da8577ff2153de2217e34728fc4b2     0        1015341794 lovelace + TxOutDatumNone
a743a4093613a0fbcd4eee46df97a9063f4da8577ff2153de2217e34728fc4b2     1        100000000 lovelace + TxOutDatumNone
a743a4093613a0fbcd4eee46df97a9063f4da8577ff2153de2217e34728fc4b2     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
```

## Transaction 4. Borrower Repays the Loan's Principal and Interest

First we compute the Marlowe input required to replay the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                  \
                        --deposit-account "Role=$BORROWER_ROLE"      \
                        --deposit-party "Role=$BORROWER_ROLE"        \
                        --deposit-amount "$(($PRINCIPAL+$INTEREST))" \
                        --invalid-before "$TIP"                      \
                        --invalid-hereafter "$(($TIP+4*3600))"       \
                        --out-file tx-4.marlowe                      \
                        --print-stats
```

```console
Datum size: 19
Payment 1
  Acccount: "ThomasMiddleton"
  Payee: Party "JohnFletcher"
  Ada: 105.000000
Payment 2
  Acccount: "JohnFletcher"
  Payee: Party "JohnFletcher"
  Ada: 3.000000
```

Now the borrower Thomas Middleton submits a transaction that repays the loan.

```
TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                                               \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                        --marlowe-in-file tx-2.marlowe                              \
                        --tx-in-marlowe "$TX_2"#1                                   \
                        --tx-in "$TX_3"#0                                           \
                        --tx-in "$TX_3"#1                                           \
                        --tx-in "$TX_3"#2                                           \
                        --tx-in-collateral "$TX_3"#0                                \
                        --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                        --marlowe-out-file tx-4.marlowe                             \
                        --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                        --change-address "$BORROWER_ADDRESS"                        \
                        --out-file tx-4.raw                                         \
                        --print-stats                                               \
                        --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                    \
)
```

```console
Fee: Lovelace 1269216
Size: 14654 / 32768 = 44%
Execution units:
  Memory: 5408586 / 10000000 = 54%
  Steps: 1842871626 / 10000000000 = 18%
```

The closing of the contract paid in the transaction `8c17ba7f2d9b160fb66cca9435c8e80af53b1fa1955836535a5acbb14639971a` the 100000000 ADA principal and 5000000 ADA interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8c17ba7f2d9b160fb66cca9435c8e80af53b1fa1955836535a5acbb14639971a     0        1009072578 lovelace + TxOutDatumNone
8c17ba7f2d9b160fb66cca9435c8e80af53b1fa1955836535a5acbb14639971a     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8c17ba7f2d9b160fb66cca9435c8e80af53b1fa1955836535a5acbb14639971a     1        108000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7df601bb5e85d1516107955e830fa45feb8b4f0ee4a3c05bec3cce74db9a8a2c"
```

## Transaction 5. Lender Withdraws Repayment.

The lender John Fletcher submits a transaction to withdraw the repayment from the role address.

```
TX_5=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                           \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-4.marlowe                             \
                         --role-name "$LENDER_ROLE"                              \
                         --tx-in "$TX_2"#0                                       \
                         --tx-in "$TX_2"#3                                       \
                         --tx-in-collateral "$TX_2"#0                            \
                         --tx-out "$LENDER_ADDRESS+$MINIMUM_ADA+1 $LENDER_TOKEN" \
                         --change-address "$LENDER_ADDRESS"                      \
                         --required-signer "$LENDER_PAYMENT_SKEY"                \
                         --out-file tx-5.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)
```

```console
Fee: Lovelace 451497
Size: 3308 / 32768 = 10%
Execution units:
  Memory: 1541710 / 10000000 = 15%
  Steps: 581669862 / 10000000000 = 5%
```

There is no UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
14079959ed0c9aa8ef74dff349bc920b1fd9c375622c23329f59847425693f21     0        912897469 lovelace + TxOutDatumNone
14079959ed0c9aa8ef74dff349bc920b1fd9c375622c23329f59847425693f21     1        108000000 lovelace + TxOutDatumNone
14079959ed0c9aa8ef74dff349bc920b1fd9c375622c23329f59847425693f21     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JohnFletcher + TxOutDatumNone
```

## Clean Up

It's convenient to consolidate the UTxOs for the lender.

```
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX_5"#0                         \
                               --tx-in "$TX_5"#1                         \
                               --required-signer "$LENDER_PAYMENT_SKEY"  \
                               --change-address "$LENDER_ADDRESS"        \
                               --out-file tx-4.raw                       \
                               --submit=600                              \
> /dev/null
