# Test of a Zero-Coupon Bond

[This zero-coupon bond](../../src/Language/Marlowe/CLI/Examples/ZeroCouponBond.hs) has one party borrow and another pay back with interest. It uses role tokens.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables `LENDER_PREFIX` and `BORROWER_PREFIX` where they appear below.

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

The tip is at slot 4849880. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4849885. Tests may fail if this is not the case.

### Participants

#### The Lender

```
LENDER_PREFIX="$TREASURY/john-fletcher"
LENDER_NAME="John Fletcher"
LENDER_ROLE=JF
LENDER_PAYMENT_SKEY="$LENDER_PREFIX".skey
LENDER_PAYMENT_VKEY="$LENDER_PREFIX".vkey
```

Create the lender's keys, if necessary.

```
if [[ ! -e "$LENDER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$LENDER_PAYMENT_SKEY"      \
                              --verification-key-file "$LENDER_PAYMENT_VKEY"
fi
LENDER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$LENDER_PAYMENT_VKEY")
```

Fund the lender's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 150000000                      \
                        "$LENDER_ADDRESS"
```

```console
TxId "a98b9d6c17d41960b26ac4398d089da76f10b8996fbd936cd5ed3053ae2de6b5"
```

### The Borrower

```
BORROWER_PREFIX="$TREASURY/thomas-middleton"
BORROWER_NAME="Thomas Middleton"
BORROWER_ROLE=TM
BORROWER_PAYMENT_SKEY="$BORROWER_PREFIX".skey
BORROWER_PAYMENT_VKEY="$BORROWER_PREFIX".vkey
```

Create the borrower's keys, if necessary.

```
if [[ ! -e "$BORROWER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$BORROWER_PAYMENT_SKEY"      \
                              --verification-key-file "$BORROWER_PAYMENT_VKEY"
fi
BORROWER_ADDRESS=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file "$BORROWER_PAYMENT_VKEY")
```

Fund the borrower's address.

```
marlowe-cli util faucet "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        "$BORROWER_ADDRESS"
```

```console
TxId "8e6da7db3f1d3270431a1b5e7c9dba6d6d4dd2fb80b683439901e214d8698786"
```

### Role Tokens

The lender mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$LENDER_PAYMENT_SKEY"  \
                      --change-address  "$LENDER_ADDRESS"       \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$LENDER_ROLE" "$BORROWER_ROLE"           \
| sed -e 's/^PolicyID "\(.*\)"$/\1/'                            \
)
LENDER_TOKEN="$ROLE_CURRENCY.$LENDER_ROLE"
BORROWER_TOKEN="$ROLE_CURRENCY.$BORROWER_ROLE"
```

Find the transaction output with the borrower's role token.

```
TX_MINT_BORROWER=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BORROWER_TOKEN"            \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the borrower their role token.

```
marlowe-cli transaction simple "${MAGIC[@]}"                                          \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"              \
                               --tx-in "$TX_MINT_BORROWER"                            \
                               --tx-out "$BORROWER_ADDRESS+2000000+1 $BORROWER_TOKEN" \
                               --required-signer "$LENDER_PAYMENT_SKEY"               \
                               --change-address "$LENDER_ADDRESS"                     \
                               --out-file /dev/null                                   \
                               --submit 600
```

```console
TxId "b7e21a89dac9065f9377961344ba436c4b1246f789aa1cef0ad29e613d8423df"
```

### Available UTxOs

The lender John Fletcher is the minimum-ADA provider and has the address `addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r` and role token named `JF`. They have the following UTxOs in their wallet:

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
64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb     0        185379593 lovelace + TxOutDatumNone
64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb     1        2000000 lovelace + 1 2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847.4a46 + TxOutDatumNone
64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb     2        2000000 lovelace + 500 357bc35cce53a1bd85fa37fe50c335da555c7855d285dd77cb564693.5377616e + TxOutDatumNone
64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb     3        2000000 lovelace + 300 6312ed95a755c99a22d4641899a70451ae2b893086f0cad446e294bb.476c6f6265 + TxOutDatumNone
64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb     4        2000000 lovelace + 300 7bd1eb01abaf6f825e20069db08d39f3335e8c623554164f6b859f00.476c6f6265 + TxOutDatumNone
64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb     5        2000000 lovelace + 1 830ce5deeb541779233daaf00502988fe29a929368726f309624f270.4a46 + TxOutDatumNone
64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb     6        2000000 lovelace + 1 830ce5deeb541779233daaf00502988fe29a929368726f309624f270.544d + TxOutDatumNone
```

We select the UTxO with the lender John Fletcher's role token.

```
TX_0_LENDER_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 120000000                 \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_LENDER_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$LENDER_TOKEN"              \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

John Fletcher will spend the UTxOs `64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb#0` and `64e3ac1f778040036ddc094c39a3ca95d60cd73c87b4bde8cfe6e9286ebd39fb#1`.

The borrower Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `TM`. They have the following UTxOs in their wallet:

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
310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75     0        49787859 lovelace + TxOutDatumNone
310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75     1        2000000 lovelace + 1 268151f9f1daad385660eaeda71b36e61f9a5e1ee169146ac60e263d.544d + TxOutDatumNone
310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75     2        2000000 lovelace + 1 2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847.544d + TxOutDatumNone
310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75     3        2000000 lovelace + 1 505ef85ecce95bd887dc79a37ae20617ac65dee7937c4a8320f4a07c.544d + TxOutDatumNone
310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75     4        2000000 lovelace + 1 76079153ff960a9c0e5c031f40ada5a3f6c79118a34403fe317aaeb9.544d + TxOutDatumNone
310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75     5        2000000 lovelace + 1 ba894fc186dc5d021d4033daa2e5ffcd632d2fe1645377abd8415cdb.544d + TxOutDatumNone
```

We select the UTxO with the lender Thomas Middleton's role token.

```
TX_0_BORROWER_ADA=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BORROWER_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_BORROWER_TOKEN=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BORROWER_TOKEN"            \
                        "$BORROWER_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Middleton will spend the UTxOs `310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75#0` and `310c6364987dae91cad2b0f5d3e818d76da910a617cad63bf087a5fcccfd3a75#2`.

## The Contract

```
MINIMUM_ADA=3000000
PRINCIPAL=100000000
INTEREST=5000000
LENDING_DEADLINE=$((NOW+12*HOUR))
REPAYMENT_DEADLINE=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 100000000 lovelace before Wed, 13 Apr 2022 04:05:20 +0000 and the borrower will repay the principal and interest of 5000000 lovelace before Wed, 13 Apr 2022 16:05:20 +0000.

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

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require.

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
Validator size: 12379
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wrteqehwwauwv4pmp0slu5n28ehxh2jv9fklm52v7xdyuyqtz4976`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wp3mn3wsyhwxtn75j0yg4u075znp3cafjzlxlfmeesjn4uq8vjaxk`.

The lender John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

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
Fee: Lovelace 187105
Size: 478 / 32768 = 1%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the lender John Fletcher in the transaction `cc8cd33ae495b48c976449212b3242d8b8d0ef28e1101767c05e0cd237116fa3`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cc8cd33ae495b48c976449212b3242d8b8d0ef28e1101767c05e0cd237116fa3     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "79ad3c808189d4da82bb8c15e8a68d4535a3c5cd880751e8518d74a151928a91"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cc8cd33ae495b48c976449212b3242d8b8d0ef28e1101767c05e0cd237116fa3     0        182192488 lovelace + TxOutDatumNone
```

## Transaction 2. Lender Deposits the Loan Amount

First we compute the Marlowe input required to deposit the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$LENDER_ROLE" \
                        --deposit-party "Role=$LENDER_ROLE"   \
                        --deposit-amount "$PRINCIPAL"         \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 163
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
Fee: Lovelace 1349199
Size: 13342 / 32768 = 40%
Execution units:
  Memory: 7146814 / 30000000 = 23%
  Steps: 2423443153 / 10000000000 = 24%
```

The contract passed the deposit of 100000000 ADA in the transaction `0fc312d3406fb4502dd6ef133df33f8a6f9f2d7f185bf41d0f5894d1800f0c98` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0fc312d3406fb4502dd6ef133df33f8a6f9f2d7f185bf41d0f5894d1800f0c98     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "c642cf569538dd96001d9f2fc161c53c732e59a75939bfa619415de71f7b0eb9"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0fc312d3406fb4502dd6ef133df33f8a6f9f2d7f185bf41d0f5894d1800f0c98     0        79843289 lovelace + TxOutDatumNone
0fc312d3406fb4502dd6ef133df33f8a6f9f2d7f185bf41d0f5894d1800f0c98     3        3000000 lovelace + 1 2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847.4a46 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0fc312d3406fb4502dd6ef133df33f8a6f9f2d7f185bf41d0f5894d1800f0c98     2        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
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
Fee: Lovelace 426563
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1461810 / 30000000 = 4%
  Steps: 557930172 / 10000000000 = 5%
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
fbdbae92575db348c41a52e6b96494cc8fb5b8c483804e703d240070c5d74959     0        48361296 lovelace + TxOutDatumNone
fbdbae92575db348c41a52e6b96494cc8fb5b8c483804e703d240070c5d74959     1        100000000 lovelace + TxOutDatumNone
fbdbae92575db348c41a52e6b96494cc8fb5b8c483804e703d240070c5d74959     2        3000000 lovelace + 1 2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847.544d + TxOutDatumNone
```

## Transaction 4. Borrower Repays the Loan's Principal and Interest

First we compute the Marlowe input required to replay the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "Role=$BORROWER_ROLE"    \
                        --deposit-party "Role=$BORROWER_ROLE"      \
                        --deposit-amount "$((PRINCIPAL+INTEREST))" \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-4.marlowe                    \
                        --print-stats
```

```console
Datum size: 23
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
Fee: Lovelace 1164162
Size: 13060 / 32768 = 39%
Execution units:
  Memory: 4966388 / 30000000 = 16%
  Steps: 1712447258 / 10000000000 = 17%
```

The closing of the contract paid in the transaction `eeb373bb0882ccd7404fba28669b9ffe009cd2fbc0f4b6d46eaa60370bfa656b` the 100000000 lovelace principal and 5000000 lovelace interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

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
eeb373bb0882ccd7404fba28669b9ffe009cd2fbc0f4b6d46eaa60370bfa656b     0        42197134 lovelace + TxOutDatumNone
eeb373bb0882ccd7404fba28669b9ffe009cd2fbc0f4b6d46eaa60370bfa656b     2        3000000 lovelace + 1 2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847.544d + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
eeb373bb0882ccd7404fba28669b9ffe009cd2fbc0f4b6d46eaa60370bfa656b     1        108000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "506ad3a3267fc31ec816a97469a7ae6c63f4838f38d2d692fa428fcd219bde29"
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
Fee: Lovelace 422221
Size: 2885 / 32768 = 8%
Execution units:
  Memory: 1407010 / 30000000 = 4%
  Steps: 541567360 / 10000000000 = 5%
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
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
efc0eac0eb481a7395fa39d4b8015332ac6f5a940eb7ef912d3bc1d9bb84dc62     0        79421068 lovelace + TxOutDatumNone
efc0eac0eb481a7395fa39d4b8015332ac6f5a940eb7ef912d3bc1d9bb84dc62     1        108000000 lovelace + TxOutDatumNone
efc0eac0eb481a7395fa39d4b8015332ac6f5a940eb7ef912d3bc1d9bb84dc62     2        3000000 lovelace + 1 2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847.4a46 + TxOutDatumNone
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
eeb373bb0882ccd7404fba28669b9ffe009cd2fbc0f4b6d46eaa60370bfa656b     0        42197134 lovelace + TxOutDatumNone
eeb373bb0882ccd7404fba28669b9ffe009cd2fbc0f4b6d46eaa60370bfa656b     2        3000000 lovelace + 1 2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847.544d + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple "${MAGIC[@]}"                                        \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"            \
                               --tx-in "$TX_4"#0                                    \
                               --tx-in "$TX_4"#2                                    \
                               --tx-out "$LENDER_ADDRESS+1400000+1 $BORROWER_TOKEN" \
                               --required-signer "$BORROWER_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                   \
                               --out-file /dev/null                                 \
                               --submit 600
```

```console
TxId "ac7974c45002204078d1078eec493f8c012e36376b5dd1e9d417ec1203824e33"
```

marlowe-cli util mint "${MAGIC[@]}" \
                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                      --required-signer "$LENDER_PAYMENT_SKEY"  \
                      --change-address  "$LENDER_ADDRESS"       \
                      --count -1                                \
                      --expires "$MINT_EXPIRES"                 \
                      --out-file /dev/null                      \
                      --submit=600                              \
                      "$LENDER_ROLE" "$BORROWER_ROLE"
```

```console
PolicyID "2d3a9726291d536b03ba1d6559cbf9844e5f7c999dfc430a0ced2847"
TX=$(
marlowe-cli util select "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$LENDER_PAYMENT_SKEY"  \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600
```

```console
TxId "86c294948f229eea1f18be62c49def9f686970db6f1f6718fcdf97ad659a4607"
