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
if true
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
LENDER_ROLE=JF
LENDER_TOKEN="$ROLE_CURRENCY.$LENDER_ROLE"
LENDER_PAYMENT_SKEY="$LENDER_PREFIX".skey
LENDER_PAYMENT_VKEY="$LENDER_PREFIX".vkey
LENDER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$LENDER_PAYMENT_VKEY" \
)
```

The lender John Fletcher is the minimum-ADA provider and has the address `addr_test1vrsuucqupq5xz7dw0tnw7w3cyck5zyk2kpnr56xlhr57m3gll8m84` and role token named `JF`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$LENDER_PAYMENT_SKEY"  \
                       --change-address "$LENDER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fec3d985baafae0d923fca3481d4eb28dd3e95c4ed48736b916aa5d6c8baaf7e     0        3006078428 lovelace + TxOutDatumNone
fec3d985baafae0d923fca3481d4eb28dd3e95c4ed48736b916aa5d6c8baaf7e     1        2000000 lovelace + 800 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.Globe + TxOutDatumNone
fec3d985baafae0d923fca3481d4eb28dd3e95c4ed48736b916aa5d6c8baaf7e     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JF + TxOutDatumNone
fec3d985baafae0d923fca3481d4eb28dd3e95c4ed48736b916aa5d6c8baaf7e     3        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JohnFletcher + TxOutDatumNone
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

John Fletcher will spend the UTxOs `fec3d985baafae0d923fca3481d4eb28dd3e95c4ed48736b916aa5d6c8baaf7e#0` and `fec3d985baafae0d923fca3481d4eb28dd3e95c4ed48736b916aa5d6c8baaf7e#2`.

### The Borrower

```
BORROWER_PREFIX="$TREASURY/thomas-middleton"
BORROWER_NAME="Thomas Middleton"
BORROWER_ROLE=TM
BORROWER_TOKEN="$ROLE_CURRENCY.$BORROWER_ROLE"
BORROWER_PAYMENT_SKEY="$BORROWER_PREFIX".skey
BORROWER_PAYMENT_VKEY="$BORROWER_PREFIX".vkey
BORROWER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                            \
                            --payment-verification-key-file "$BORROWER_PAYMENT_VKEY" \
)
```

The borrower Thomas Middleton has the address `addr_test1vzgrqnlp6elmettvuelx5vkn0uxhtu2ewqdhx297ukgjmjgpss5k0` and role token named `TM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                              \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$BORROWER_PAYMENT_SKEY" \
                       --change-address "$BORROWER_ADDRESS"       \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
505d8b81b3523e0da27e35585faf3b21c629c86e74cd5c720a1edfa89e201c95     0        2914383748 lovelace + TxOutDatumNone
505d8b81b3523e0da27e35585faf3b21c629c86e74cd5c720a1edfa89e201c95     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
505d8b81b3523e0da27e35585faf3b21c629c86e74cd5c720a1edfa89e201c95     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
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
cardano-cli query utxo "${MAGIC[@]}"                                                                        \
                       --address "$BORROWER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                               \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$BORROWER_ROLE"'" == 1) | .key' \
)
```

Thomas Middleton will spend the UTxOs `505d8b81b3523e0da27e35585faf3b21c629c86e74cd5c720a1edfa89e201c95#0` and `505d8b81b3523e0da27e35585faf3b21c629c86e74cd5c720a1edfa89e201c95#1`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 50681117. The current POSIX time implies that the tip of the blockchain should be slightly before slot 50681119. Tests may fail if this is not the case.

## The Contract

```
MINIMUM_ADA=3000000
PRINCIPAL=100000000
INTEREST=5000000
LENDING_DEADLINE=$((TIP+12*3600))
REPAYMENT_DEADLINE=$((TIP+24*3600))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 100000000 lovelace before slot 50724317 and the borrower will repay the principal and interest of 5000000 lovelace before slot 50767517.

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
Validator size: 14339
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 40342515, exBudgetMemory = ExMemory 135600}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wqkj9530xdhg20vjc9jgrtv9atq58ylj4vv96mush546sqg4js3dg`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpdsreg339dsku87hskeenlwcaalhdu32dd85rrrjwr002g3c0yy3`.

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
Fee: Lovelace 186753
Size: 470 / 16384 = 2%
Execution units:
  Memory: 0 / 16000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the lender John Fletcher in the transaction `083bcc9e7c45425b2fe730d34962bb56ffc72d5f24dc083592356379ded3bcf6`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
083bcc9e7c45425b2fe730d34962bb56ffc72d5f24dc083592356379ded3bcf6     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "d051962bc814f669747219d0d2b7a5cfb635a25581881146b5b667615b5e3ab6"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
083bcc9e7c45425b2fe730d34962bb56ffc72d5f24dc083592356379ded3bcf6     0        3002891675 lovelace + TxOutDatumNone
```

## Transaction 2. Lender Deposits the Loan Amount

First we compute the Marlowe input required to deposit the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$LENDER_ROLE" \
                        --deposit-party "Role=$LENDER_ROLE"   \
                        --deposit-amount "$PRINCIPAL"         \
                        --invalid-before "$TIP"               \
                        --invalid-hereafter "$((TIP+4*3600))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 155
Payment 1
  Acccount: "JF"
  Payee: Party "TM"
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
Fee: Lovelace 1433381
Size: 15286 / 16384 = 93%
Execution units:
  Memory: 7130660 / 16000000 = 44%
  Steps: 2417586756 / 10000000000 = 24%
```

The contract passed the deposit of 100000000 ADA in the transaction `1fd1d42aa30a5ac548b13ee49fc0f39eb1c66c8a7d3060912ee835f4fee55d98` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1fd1d42aa30a5ac548b13ee49fc0f39eb1c66c8a7d3060912ee835f4fee55d98     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "52cd5424e42079e03e72e82a1d8918f3353f418e86e87553c8451add9a18b1fe"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1fd1d42aa30a5ac548b13ee49fc0f39eb1c66c8a7d3060912ee835f4fee55d98     0        2900458294 lovelace + TxOutDatumNone
1fd1d42aa30a5ac548b13ee49fc0f39eb1c66c8a7d3060912ee835f4fee55d98     3        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JF + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1fd1d42aa30a5ac548b13ee49fc0f39eb1c66c8a7d3060912ee835f4fee55d98     2        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
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
                         --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                         --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                         --change-address "$BORROWER_ADDRESS"                        \
                         --out-file tx-3.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)
```

```console
Fee: Lovelace 451385
Size: 3187 / 16384 = 19%
Execution units:
  Memory: 1607510 / 16000000 = 10%
  Steps: 601307457 / 10000000000 = 6%
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
bcd61bad885241c597e2c044a9ba9e23e8bdf58b0edbfed11da8d18b08ecbc46     0        2912932363 lovelace + TxOutDatumNone
bcd61bad885241c597e2c044a9ba9e23e8bdf58b0edbfed11da8d18b08ecbc46     1        100000000 lovelace + TxOutDatumNone
bcd61bad885241c597e2c044a9ba9e23e8bdf58b0edbfed11da8d18b08ecbc46     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
```

## Transaction 4. Borrower Repays the Loan's Principal and Interest

First we compute the Marlowe input required to replay the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "Role=$BORROWER_ROLE"    \
                        --deposit-party "Role=$BORROWER_ROLE"      \
                        --deposit-amount "$((PRINCIPAL+INTEREST))" \
                        --invalid-before "$TIP"                    \
                        --invalid-hereafter "$((TIP+4*3600))"      \
                        --out-file tx-4.marlowe                    \
                        --print-stats
```

```console
Datum size: 19
Payment 1
  Acccount: "TM"
  Payee: Party "JF"
  Ada: 105.000000
Payment 2
  Acccount: "JF"
  Payee: Party "JF"
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
Fee: Lovelace 1293599
Size: 15012 / 16384 = 91%
Execution units:
  Memory: 5513212 / 16000000 = 34%
  Steps: 1878853146 / 10000000000 = 18%
```

The closing of the contract paid in the transaction `5b219f54ebd7d4a692abcd85025a5d498f11c847fe9bc4c8ab4042d514748fd1` the 100000000 lovelace principal and 5000000 lovelace interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
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
5b219f54ebd7d4a692abcd85025a5d498f11c847fe9bc4c8ab4042d514748fd1     0        2906638764 lovelace + TxOutDatumNone
5b219f54ebd7d4a692abcd85025a5d498f11c847fe9bc4c8ab4042d514748fd1     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5b219f54ebd7d4a692abcd85025a5d498f11c847fe9bc4c8ab4042d514748fd1     1        108000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "506ad3a3267fc31ec816a97469a7ae6c63f4838f38d2d692fa428fcd219bde29"
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
                         --required-signer "$LENDER_PAYMENT_SKEY"                \
                         --tx-out "$LENDER_ADDRESS+$MINIMUM_ADA+1 $LENDER_TOKEN" \
                         --change-address "$LENDER_ADDRESS"                      \
                         --out-file tx-5.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)
```

```console
Fee: Lovelace 446196
Size: 3187 / 16384 = 19%
Execution units:
  Memory: 1542010 / 16000000 = 9%
  Steps: 581758934 / 10000000000 = 5%
```

There is no UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
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
1046c7ea2b3a8c0b9b0de67e85beb4190d75f4ca493df53c9385ecdb19d2edd9     0        2900012098 lovelace + TxOutDatumNone
1046c7ea2b3a8c0b9b0de67e85beb4190d75f4ca493df53c9385ecdb19d2edd9     1        108000000 lovelace + TxOutDatumNone
1046c7ea2b3a8c0b9b0de67e85beb4190d75f4ca493df53c9385ecdb19d2edd9     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.JF + TxOutDatumNone
```

