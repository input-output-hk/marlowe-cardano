# Test of a Zero-Coupon Bond

[This zero-coupon bond](https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-contracts/src/Marlowe/Contracts/ZeroCouponBond.hs) has one party borrow and another pay back with interest. It uses role tokens.

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.
See below for how to set `MAGIC` to select the network.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the two parties, or they will be created automatically: to do this, set the environment variables `LENDER_PREFIX` and `BORROWER_PREFIX` where they appear below.

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

The tip is at slot 3421605. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3421608. Tests may fail if this is not the case.

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
LENDER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$LENDER_PAYMENT_VKEY")
```

Fund the lender's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 150000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$LENDER_ADDRESS"
```

```console
TxId "2e292bb6add2069568e44ed193619cd2bae8c24e7bd07c06360c80b8157d3e11"
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
BORROWER_ADDRESS=$(cardano-cli address build --testnet-magic "$MAGIC" --payment-verification-key-file "$BORROWER_PAYMENT_VKEY")
```

Fund the borrower's address.

```
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$BORROWER_ADDRESS"
```

```console
TxId "8a60bb27552f768962728fb005ef4c6ff94efeb391f87d2a68e93662626b2c2c"
```

### Role Tokens

The lender mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC"                  \
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
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BORROWER_TOKEN"            \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Send the borrower their role token.

```
marlowe-cli transaction simple --testnet-magic "$MAGIC"                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"              \
                               --tx-in "$TX_MINT_BORROWER"                            \
                               --tx-out "$BORROWER_ADDRESS+2000000+1 $BORROWER_TOKEN" \
                               --required-signer "$LENDER_PAYMENT_SKEY"               \
                               --change-address "$LENDER_ADDRESS"                     \
                               --out-file /dev/null                                   \
                               --submit 600
```

```console
TxId "ef57951fd6be99ed97ff6e2c33abe55890209fb943864de4f46959be8a7412e5"
```

### Available UTxOs

The lender John Fletcher is the minimum-ADA provider and has the address `addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r` and role token named `JF`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$LENDER_PAYMENT_SKEY"  \
                       --change-address "$LENDER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
554e7aaa54eeb2c868cf877d6a254a0de258e8680b08783864a63e8efd454b9c     0        145473313 lovelace + TxOutDatumNone
554e7aaa54eeb2c868cf877d6a254a0de258e8680b08783864a63e8efd454b9c     1        2000000 lovelace + 1 c0f7cec958fbe16c80de911567d87f1ad026a12b8fa25db78a9194eb.4a46 + TxOutDatumNone
```

We select the UTxO with the lender John Fletcher's role token.

```
TX_0_LENDER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 120000000                 \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_LENDER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$LENDER_TOKEN"              \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

John Fletcher will spend the UTxOs `554e7aaa54eeb2c868cf877d6a254a0de258e8680b08783864a63e8efd454b9c#0` and `554e7aaa54eeb2c868cf877d6a254a0de258e8680b08783864a63e8efd454b9c#1`.

The borrower Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `TM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                   \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$BORROWER_PAYMENT_SKEY" \
                       --change-address "$BORROWER_ADDRESS"       \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
54c504994e5d828fdfc414a2037e1c5273d67b67f866fbe0e4f678b55cbc6dae     0        49824907 lovelace + TxOutDatumNone
54c504994e5d828fdfc414a2037e1c5273d67b67f866fbe0e4f678b55cbc6dae     1        2000000 lovelace + 1 c0f7cec958fbe16c80de911567d87f1ad026a12b8fa25db78a9194eb.544d + TxOutDatumNone
```

We select the UTxO with the lender Thomas Middleton's role token.

```
TX_0_BORROWER_ADA=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BORROWER_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
TX_0_BORROWER_TOKEN=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --asset-only "$BORROWER_TOKEN"            \
                        "$BORROWER_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
```

Thomas Middleton will spend the UTxOs `54c504994e5d828fdfc414a2037e1c5273d67b67f866fbe0e4f678b55cbc6dae#0` and `54c504994e5d828fdfc414a2037e1c5273d67b67f866fbe0e4f678b55cbc6dae#1`.

## The Contract

```
MINIMUM_ADA=3000000
PRINCIPAL=100000000
INTEREST=5000000
LENDING_DEADLINE=$((NOW+12*HOUR))
REPAYMENT_DEADLINE=$((NOW+24*HOUR))
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 100000000 lovelace before Sun, 18 Sep 2022 02:26:45 +0000 and the borrower will repay the principal and interest of 5000000 lovelace before Sun, 18 Sep 2022 14:26:45 +0000.

We create the contract for the previously specified parameters.

```
marlowe-cli template zcb --minimum-ada "$MINIMUM_ADA"               \
                         --lender "$LENDER_ROLE"                    \
                         --borrower "$BORROWER_ROLE"                \
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
marlowe-cli run initialize --testnet-magic "$MAGIC"                  \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --roles-currency "$ROLE_CURRENCY"         \
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

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.tx.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpkmnxz4aylglk57j9mf90r5dj0kmde7n6frfgatam4fw8qyrah58`.

The lender John Fletcher submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 188909
Size: 517 / 16384 = 3%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the lender John Fletcher in the transaction `ce61649176b5b44597eb4fc6518b65f48691e5cb9683e8aeb8a4ca0851cc2198`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ce61649176b5b44597eb4fc6518b65f48691e5cb9683e8aeb8a4ca0851cc2198     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "79f7160932124dae99fcb5032e61a758cd3f1076ffedba3b43b16fc72a2758f6"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ce61649176b5b44597eb4fc6518b65f48691e5cb9683e8aeb8a4ca0851cc2198     0        142284404 lovelace + TxOutDatumNone
```

## Transaction 2. Lender Deposits the Loan Amount

First we compute the Marlowe input required to deposit the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "$LENDER_ROLE"      \
                        --deposit-party "$LENDER_ROLE"        \
                        --deposit-amount "$PRINCIPAL"         \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 199
Payment 1
  Acccount: "JF"
  Payee: Party "TM"
  Ada: Lovelace {getLovelace = 100000000}
```

Now the lender John Fletcher submits the transaction that deposits the loan amount.

```
TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                \
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
Fee: Lovelace 1345468
Size: 13747 / 16384 = 83%
Execution units:
  Memory: 7375728 / 14000000 = 52%
  Steps: 1940119903 / 10000000000 = 19%
```

The contract passed the deposit of 100000000 ADA in the transaction `2fca92c5d8e348afcb02ceab32e2120b057771c9d3df7315cacf5dd10f2495bb` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2fca92c5d8e348afcb02ceab32e2120b057771c9d3df7315cacf5dd10f2495bb     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "052fe7284d69d53aef6e762c7d5fc72651d613d4c3941faac56f706d608fdff6"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2fca92c5d8e348afcb02ceab32e2120b057771c9d3df7315cacf5dd10f2495bb     0        39938936 lovelace + TxOutDatumNone
2fca92c5d8e348afcb02ceab32e2120b057771c9d3df7315cacf5dd10f2495bb     3        3000000 lovelace + 1 c0f7cec958fbe16c80de911567d87f1ad026a12b8fa25db78a9194eb.4a46 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2fca92c5d8e348afcb02ceab32e2120b057771c9d3df7315cacf5dd10f2495bb     2        100000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "60bae8c762c06fae005a78ed1fa61a1404bfd24fc409d1f62e287758644afb2c"
```

## Transaction 3. Lender Withdraws Loan.

The lender John Fletcher submits a transaction to withdraw the loan from the role address.

```
TX_3=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                    \
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
Fee: Lovelace 436285
Size: 3083 / 16384 = 18%
Execution units:
  Memory: 1606962 / 14000000 = 11%
  Steps: 454555011 / 10000000000 = 4%
```

There is no UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the borrower Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2988bc791f934ecc0bc310fa7747087fcfc72d4fa5c6a6edc6a9cbae31f84ee1     0        48388622 lovelace + TxOutDatumNone
2988bc791f934ecc0bc310fa7747087fcfc72d4fa5c6a6edc6a9cbae31f84ee1     1        100000000 lovelace + TxOutDatumNone
2988bc791f934ecc0bc310fa7747087fcfc72d4fa5c6a6edc6a9cbae31f84ee1     2        3000000 lovelace + 1 c0f7cec958fbe16c80de911567d87f1ad026a12b8fa25db78a9194eb.544d + TxOutDatumNone
```

## Transaction 4. Borrower Repays the Loan's Principal and Interest

First we compute the Marlowe input required to replay the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                \
                        --deposit-account "$BORROWER_ROLE"         \
                        --deposit-party "$BORROWER_ROLE"           \
                        --deposit-amount "$((PRINCIPAL+INTEREST))" \
                        --invalid-before "$NOW"                    \
                        --invalid-hereafter "$((NOW+4*HOUR))"      \
                        --out-file tx-4.marlowe                    \
                        --print-stats
```

```console
Datum size: 59
Payment 1
  Acccount: "TM"
  Payee: Party "JF"
  Ada: Lovelace {getLovelace = 105000000}
Payment 2
  Acccount: "JF"
  Payee: Party "JF"
  Ada: Lovelace {getLovelace = 3000000}
```

Now the borrower Thomas Middleton submits a transaction that repays the loan.

```
TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                    \
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
Fee: Lovelace 1262559
Size: 13426 / 16384 = 81%
Execution units:
  Memory: 6429034 / 14000000 = 45%
  Steps: 1682088784 / 10000000000 = 16%
```

The closing of the contract paid in the transaction `cffe979663c6ac199b05369cc169aacd3c47dad89db061f529e688a94b5da2fe` the 100000000 lovelace principal and 5000000 lovelace interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cffe979663c6ac199b05369cc169aacd3c47dad89db061f529e688a94b5da2fe     0        42126063 lovelace + TxOutDatumNone
cffe979663c6ac199b05369cc169aacd3c47dad89db061f529e688a94b5da2fe     2        3000000 lovelace + 1 c0f7cec958fbe16c80de911567d87f1ad026a12b8fa25db78a9194eb.544d + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cffe979663c6ac199b05369cc169aacd3c47dad89db061f529e688a94b5da2fe     1        108000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "c8524fd8a73149bd3705e0abc98d35acdfaf0917afafc0d4d156b2dc75b3b8c7"
```

## Transaction 5. Lender Withdraws Repayment.

The lender John Fletcher submits a transaction to withdraw the repayment from the role address.

```
TX_5=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                \
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
Fee: Lovelace 432223
Size: 3083 / 16384 = 18%
Execution units:
  Memory: 1552162 / 14000000 = 11%
  Steps: 442070090 / 10000000000 = 4%
```

There is no UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
882da38a19764a84b5961cedc152a13c3d9a362dce633972221ef5c36a76e309     0        39506713 lovelace + TxOutDatumNone
882da38a19764a84b5961cedc152a13c3d9a362dce633972221ef5c36a76e309     1        108000000 lovelace + TxOutDatumNone
882da38a19764a84b5961cedc152a13c3d9a362dce633972221ef5c36a76e309     2        3000000 lovelace + 1 c0f7cec958fbe16c80de911567d87f1ad026a12b8fa25db78a9194eb.4a46 + TxOutDatumNone
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
cffe979663c6ac199b05369cc169aacd3c47dad89db061f529e688a94b5da2fe     0        42126063 lovelace + TxOutDatumNone
cffe979663c6ac199b05369cc169aacd3c47dad89db061f529e688a94b5da2fe     2        3000000 lovelace + 1 c0f7cec958fbe16c80de911567d87f1ad026a12b8fa25db78a9194eb.544d + TxOutDatumNone
```

## Clean Up

```
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
marlowe-cli transaction simple --testnet-magic "$MAGIC"                           \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"          \
                               --tx-in "$TX_4"#0                                  \
                               --tx-in "$TX_4"#2                                  \
                               --tx-out "$BURN_ADDRESS+1400000+1 $BORROWER_TOKEN" \
                               --required-signer "$BORROWER_PAYMENT_SKEY"         \
                               --change-address "$FAUCET_ADDRESS"                 \
                               --out-file /dev/null                               \
                               --submit 600
```

```console
TxId "2fb5d00572e47c4f1bd95b1240f51c3344cc7e904148fa942a542c4881cdede7"
```

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
marlowe-cli transaction simple --testnet-magic "$MAGIC"                         \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"        \
                               --tx-in "$TX_5"#0                                \
                               --tx-in "$TX_5"#1                                \
                               --tx-in "$TX_5"#2                                \
                               --tx-out "$BURN_ADDRESS+1400000+1 $LENDER_TOKEN" \
                               --required-signer "$LENDER_PAYMENT_SKEY"         \
                               --change-address "$FAUCET_ADDRESS"               \
                               --out-file /dev/null                             \
                               --submit 600
```

```console
TxId "cf9fce79ea47a9fdff010480211dd6922bd84428ef369582fde32726f36406a3"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
