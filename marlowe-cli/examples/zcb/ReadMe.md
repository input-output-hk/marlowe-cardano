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

The tip is at slot 4858655. The current POSIX time implies that the tip of the blockchain should be slightly before slot 4858657. Tests may fail if this is not the case.

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
TxId "42619f9f06817e86c8afe8abc665242e13bb1b147abeef3233b238ad55b77ca4"
```

#### The Borrower

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
TxId "8d7a725426914d9235efac9537b46204e2b2815b67880b76e87b84e948be80d0"
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
TxId "64efd7687c7e6d163f3bade78ec8e07e71492175abe650f388ab7b5ff259288a"
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
b50945ba6594e8d7a679fca6d07520d81d9097bbdd05e68a2c8b71be54ca5313     0        145473665 lovelace + TxOutDatumNone
b50945ba6594e8d7a679fca6d07520d81d9097bbdd05e68a2c8b71be54ca5313     1        2000000 lovelace + 1 327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556.4a46 + TxOutDatumNone
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

John Fletcher will spend the UTxOs `b50945ba6594e8d7a679fca6d07520d81d9097bbdd05e68a2c8b71be54ca5313#0` and `b50945ba6594e8d7a679fca6d07520d81d9097bbdd05e68a2c8b71be54ca5313#1`.

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
b873ef80e9644c87aba335141233234e44a686bbdc04085bec8818c9cf300d48     0        49824995 lovelace + TxOutDatumNone
b873ef80e9644c87aba335141233234e44a686bbdc04085bec8818c9cf300d48     1        2000000 lovelace + 1 327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556.544d + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `b873ef80e9644c87aba335141233234e44a686bbdc04085bec8818c9cf300d48#0` and `b873ef80e9644c87aba335141233234e44a686bbdc04085bec8818c9cf300d48#1`.

## The Contract

```
MINIMUM_ADA=3000000
PRINCIPAL=100000000
INTEREST=5000000
LENDING_DEADLINE=$((NOW+12*HOUR))
REPAYMENT_DEADLINE=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 100000000 lovelace before Wed, 13 Apr 2022 06:31:35 +0000 and the borrower will repay the principal and interest of 5000000 lovelace before Wed, 13 Apr 2022 18:31:35 +0000.

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

The Marlowe contract resides at address `addr_test1wzkrnqwe7exczt0lz29ddq8vtaasjwdh6h6kdnhtumledxqmv80wt`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wzv7u7hlks6ml5rrghuf4tcm2pdlz9q4lcsam6f698xunmc8wv65s`.

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

The contract received the minimum ADA of 3000000 lovelace from the lender John Fletcher in the transaction `b43ca5243322a0cdc79b5879fafe9bbbaa3876e94a29730878fc5c829f857eef`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b43ca5243322a0cdc79b5879fafe9bbbaa3876e94a29730878fc5c829f857eef     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "bc3e13025a6ca4df1a170f106ebdb616d44e87255f4231585a75cab722a1e9c7"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b43ca5243322a0cdc79b5879fafe9bbbaa3876e94a29730878fc5c829f857eef     0        142286560 lovelace + TxOutDatumNone
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
Fee: Lovelace 1359408
Size: 13342 / 32768 = 40%
Execution units:
  Memory: 7275418 / 30000000 = 24%
  Steps: 2462125649 / 10000000000 = 24%
```

The contract passed the deposit of 100000000 ADA in the transaction `986ab9dd3a286c479668d6e1f67a92abf8f0272b651c809b967d23a66bcb3311` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
986ab9dd3a286c479668d6e1f67a92abf8f0272b651c809b967d23a66bcb3311     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "e6330de400327c838a7316cc05cfe4417ab6e5749622bce23f0618fb960ae775"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
986ab9dd3a286c479668d6e1f67a92abf8f0272b651c809b967d23a66bcb3311     0        39927152 lovelace + TxOutDatumNone
986ab9dd3a286c479668d6e1f67a92abf8f0272b651c809b967d23a66bcb3311     3        3000000 lovelace + 1 327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556.4a46 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
986ab9dd3a286c479668d6e1f67a92abf8f0272b651c809b967d23a66bcb3311     2        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d6b817dd540b8b4358a4f439ac7eda7a9877fe9f02f6244348d80542cb761b7"
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
e97fc162448dffe53a1e2419fda61203de40acac85a725585c77ae6f5017f0db     0        48398432 lovelace + TxOutDatumNone
e97fc162448dffe53a1e2419fda61203de40acac85a725585c77ae6f5017f0db     1        100000000 lovelace + TxOutDatumNone
e97fc162448dffe53a1e2419fda61203de40acac85a725585c77ae6f5017f0db     2        3000000 lovelace + 1 327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556.544d + TxOutDatumNone
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

The closing of the contract paid in the transaction `a9eab178a69517cb89f591c66a084f30d01656584d0afe36019ee70d37f666a9` the 100000000 lovelace principal and 5000000 lovelace interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

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
a9eab178a69517cb89f591c66a084f30d01656584d0afe36019ee70d37f666a9     0        42234270 lovelace + TxOutDatumNone
a9eab178a69517cb89f591c66a084f30d01656584d0afe36019ee70d37f666a9     2        3000000 lovelace + 1 327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556.544d + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a9eab178a69517cb89f591c66a084f30d01656584d0afe36019ee70d37f666a9     1        108000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "506ad3a3267fc31ec816a97469a7ae6c63f4838f38d2d692fa428fcd219bde29"
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
bbd6338c61b12d47c861986e248bf1b5bb384ef35a629c69d5bae79bef647da6     0        39504931 lovelace + TxOutDatumNone
bbd6338c61b12d47c861986e248bf1b5bb384ef35a629c69d5bae79bef647da6     1        108000000 lovelace + TxOutDatumNone
bbd6338c61b12d47c861986e248bf1b5bb384ef35a629c69d5bae79bef647da6     2        3000000 lovelace + 1 327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556.4a46 + TxOutDatumNone
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a9eab178a69517cb89f591c66a084f30d01656584d0afe36019ee70d37f666a9     0        42234270 lovelace + TxOutDatumNone
a9eab178a69517cb89f591c66a084f30d01656584d0afe36019ee70d37f666a9     2        3000000 lovelace + 1 327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556.544d + TxOutDatumNone
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
TxId "e605bc08c059978219469fb3513c00b62d5f6b5d23ba64716c8bd71ce5026313"
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
PolicyID "327e4f0bca9b2cb8bfd2fa10b973094e88e9e267e90092198b2d2556"
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
TxId "8114df10f9743729b86f7c1e0ca135487820a0e0ed3ba67dcfd1f70bcf20d7f8"
