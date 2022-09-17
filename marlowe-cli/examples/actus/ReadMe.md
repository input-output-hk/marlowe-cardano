# Test of an ACTUS Contract (Zero-Coupon Bond)

This zero-coupon bond has one party borrow and another pay back with interest. It uses role tokens.

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

The tip is at slot 3417705. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3417710. Tests may fail if this is not the case.

### Participants

#### The Lender

```
LENDER_PREFIX="$TREASURY/john-fletcher"
LENDER_NAME="John Fletcher"
LENDER_ROLE=PA
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
TxId "ab5ad3ce4139552fd6a50b85f34cb741bf1e94274f1c9b0ac83a3734f521d0f2"
```

#### The Borrower

```
BORROWER_PREFIX="$TREASURY/thomas-middleton"
BORROWER_NAME="Thomas Middleton"
BORROWER_ROLE=CP
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
                        --lovelace 250000000                      \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$BORROWER_ADDRESS"
```

```console
TxId "60866925f85e99bdfec12290ba8add7e4c7b803a1cb278c8fbea56fdcec23f7f"
```

### Role Tokens

The lender mints the role tokens.

```
MINT_EXPIRES=$((TIP + 1000000))
ROLE_CURRENCY=$(
marlowe-cli util mint --testnet-magic "$MAGIC" \
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
TxId "c1c011a794f50d4420bad22d6a5d4c9865a12577d9297bc42ef8295147232a3f"
```

### Available UTxOs

The lender John Fletcher is the minimum-ADA provider and has the address `addr_test1vqwt2xlr4d8yk4qws675exlqy6pdhq2s76wrehkjggkvr0cerfe8r` and role token named `PA`. They have the following UTxOs in their wallet:

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
33784195608255f9c786b50e9bb36a514aa7ad9feda38020d1fed1cc570242e4     0        145473313 lovelace + TxOutDatumNone
33784195608255f9c786b50e9bb36a514aa7ad9feda38020d1fed1cc570242e4     1        2000000 lovelace + 1 5799b669a944f6ecabac80a84e465e44172310251d9a0b3979c47872.5041 + TxOutDatumNone
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

John Fletcher will spend the UTxOs `33784195608255f9c786b50e9bb36a514aa7ad9feda38020d1fed1cc570242e4#0` and `33784195608255f9c786b50e9bb36a514aa7ad9feda38020d1fed1cc570242e4#1`.

The borrower Thomas Middleton has the address `addr_test1vqetzradrerxgqu6xcuk35qckxkl4hwdz8h82zpld226t8ce30xxn` and role token named `CP`. They have the following UTxOs in their wallet:

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
42368212cb53d4e0496c18eba7679845866b051b10ff771ca82323d02add5854     0        249824907 lovelace + TxOutDatumNone
42368212cb53d4e0496c18eba7679845866b051b10ff771ca82323d02add5854     1        2000000 lovelace + 1 5799b669a944f6ecabac80a84e465e44172310251d9a0b3979c47872.4350 + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `42368212cb53d4e0496c18eba7679845866b051b10ff771ca82323d02add5854#0` and `42368212cb53d4e0496c18eba7679845866b051b10ff771ca82323d02add5854#1`.

## The Contract

```
MINIMUM_ADA=3000000
FIXED_POINT=1000000
PRINCIPAL=100
INTEREST_RATE=0.02
INTEREST=$(jq -n $PRINCIPAL*$INTEREST_RATE)
STATUS_DATE=$(date -d "$(date -u -R -d @$((NOW/1000)))" +"%Y-%m-%dT00:00:00")
INITIAL_EXCHANGE_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 1 year" +"%Y-01-01T00:00:00")
MATURITY_DATE=$(date -d "$(date -u -R -d @$((NOW/1000))) + 2 year" +"%Y-01-01T00:00:00")
LENDING_DEADLINE=$((NOW+12*HOUR))
REPAYMENT_DEADLINE=$((NOW+24*HOUR))
ACTUS_TERMS_FILE=$(mktemp)
cat > "$ACTUS_TERMS_FILE" << EOF
{
  "scheduleConfig": {
    "businessDayConvention": "NULL",
    "endOfMonthConvention": "EOM",
    "calendar": "NC"
  },
  "maturityDate": "$MATURITY_DATE",
  "contractId": "0",
  "enableSettlement": false,
  "initialExchangeDate": "$INITIAL_EXCHANGE_DATE",
  "contractRole": "RPA",
  "penaltyType": "O",
  "cycleAnchorDateOfInterestPayment": "$INITIAL_EXCHANGE_DATE",
  "contractType": "PAM",
  "notionalPrincipal": $PRINCIPAL,
  "contractPerformance": "PF",
  "collateralAmount": 0,
  "dayCountConvention": "30E360",
  "accruedInterest": 0,
  "statusDate": "$STATUS_DATE",
  "cycleOfInterestPayment": "P1YL1",
  "prepaymentEffect": "N",
  "nominalInterestRate": $INTEREST_RATE,
  "interestCalculationBase": "NT"
}
EOF
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 100 ada before Sun, 18 Sep 2022 01:21:45 +0000 and the borrower will repay the principal and interest of 2 ada before Sun, 18 Sep 2022 13:21:45 +0000.

We create the contract for the previously specified parameters.

```
marlowe-cli template actus --minimum-ada "$MINIMUM_ADA"               \
                           --party "$LENDER_ROLE"                     \
                           --counter-party "$BORROWER_ROLE"           \
                           --actus-terms-file "$ACTUS_TERMS_FILE"     \
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
Fee: Lovelace 192165
Size: 591 / 16384 = 3%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA amount of 3000000 lovelace from the lender John Fletcher in the transaction `a53662eeb90db69e90c7c1024dc5d469f977849245bb33c7ded49009ce559801`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a53662eeb90db69e90c7c1024dc5d469f977849245bb33c7ded49009ce559801     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "1a3f6b5ab188409b1481ba0b960ab6e006ba0530062a8dae74fe503d415a42d9"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
a53662eeb90db69e90c7c1024dc5d469f977849245bb33c7ded49009ce559801     0        142281148 lovelace + TxOutDatumNone
```

## Transaction 2. Lender Deposits the Loan Amount

First we compute the Marlowe input required to deposit the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe                   \
                        --deposit-account "$LENDER_ROLE"              \
                        --deposit-party "$LENDER_ROLE"                \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$NOW"                       \
                        --invalid-hereafter "$((NOW+4*HOUR))"         \
                        --out-file tx-2.marlowe                       \
                        --print-stats
```

```console
Datum size: 265
Payment 1
  Acccount: "PA"
  Payee: Party "CP"
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
Fee: Lovelace 1369984
Size: 13887 / 16384 = 84%
Execution units:
  Memory: 7603516 / 14000000 = 54%
  Steps: 2012416967 / 10000000000 = 20%
```

The contract passed the deposit of 100 ADA in the transaction `31b53b8136b57a7ebdb6e3ab0746f60e480528b8c17dcfe02d25c549b76784d8` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
31b53b8136b57a7ebdb6e3ab0746f60e480528b8c17dcfe02d25c549b76784d8     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "af557e3dc0c736b5e5ee809567d584619ab46ef4f9b60779b600e852c41f6b06"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
31b53b8136b57a7ebdb6e3ab0746f60e480528b8c17dcfe02d25c549b76784d8     0        39911164 lovelace + TxOutDatumNone
31b53b8136b57a7ebdb6e3ab0746f60e480528b8c17dcfe02d25c549b76784d8     3        3000000 lovelace + 1 5799b669a944f6ecabac80a84e465e44172310251d9a0b3979c47872.5041 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
31b53b8136b57a7ebdb6e3ab0746f60e480528b8c17dcfe02d25c549b76784d8     2        100000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "d92ad8c7a154b5b0f3fb77b264613894ee930f648677be0fb8a05a01d0cce8df"
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
2e2891c8f1a581b0ba784da606c3574e60549ea55f0da74ae675ab3169e7f45c     0        248388622 lovelace + TxOutDatumNone
2e2891c8f1a581b0ba784da606c3574e60549ea55f0da74ae675ab3169e7f45c     1        100000000 lovelace + TxOutDatumNone
2e2891c8f1a581b0ba784da606c3574e60549ea55f0da74ae675ab3169e7f45c     2        3000000 lovelace + 1 5799b669a944f6ecabac80a84e465e44172310251d9a0b3979c47872.4350 + TxOutDatumNone
```

## Transaction 4. Borrower Repays the Loan's Interest

First we compute the Marlowe input required to repay the intest of the loan.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                  \
                        --deposit-account "$BORROWER_ROLE"           \
                        --deposit-party "$BORROWER_ROLE"             \
                        --deposit-amount "$((INTEREST*FIXED_POINT))" \
                        --invalid-before "$NOW"                      \
                        --invalid-hereafter "$((NOW+4*HOUR))"        \
                        --out-file tx-4.marlowe                      \
                        --print-stats
```

```console
Datum size: 173
Payment 1
  Acccount: "CP"
  Payee: Party "PA"
  Ada: Lovelace {getLovelace = 2000000}
```

Now the borrower Thomas Middleton submits a transaction that repays the interest of the loan.

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
Fee: Lovelace 1398951
Size: 13739 / 16384 = 83%
Execution units:
  Memory: 8041490 / 14000000 = 57%
  Steps: 2092364427 / 10000000000 = 20%
```

## Transaction 5. Borrower Repays the Loan's Principal

First we compute the Marlowe input required to repay the principal of the loan.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe                   \
                        --deposit-account "$BORROWER_ROLE"            \
                        --deposit-party "$BORROWER_ROLE"              \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$NOW"                       \
                        --invalid-hereafter "$((NOW+4*HOUR))"         \
                        --out-file tx-5.marlowe                       \
                        --print-stats
```

```console
Datum size: 59
Payment 1
  Acccount: "CP"
  Payee: Party "PA"
  Ada: Lovelace {getLovelace = 100000000}
Payment 2
  Acccount: "PA"
  Payee: Party "PA"
  Ada: Lovelace {getLovelace = 3000000}
```

Now the borrower Thomas Middleton submits a transaction that repays the loan.

```
TX_5=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                                    \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                        --marlowe-in-file tx-4.marlowe                              \
                        --tx-in-marlowe "$TX_4"#1                                   \
                        --tx-in "$TX_4"#0                                           \
                        --tx-in "$TX_4"#3                                           \
                        --tx-in-collateral "$TX_4"#0                                \
                        --required-signer "$BORROWER_PAYMENT_SKEY"                  \
                        --marlowe-out-file tx-5.marlowe                             \
                        --tx-out "$BORROWER_ADDRESS+$MINIMUM_ADA+1 $BORROWER_TOKEN" \
                        --change-address "$BORROWER_ADDRESS"                        \
                        --out-file tx-5.raw                                         \
                        --print-stats                                               \
                        --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                    \
)
```

```console
Fee: Lovelace 1193245
Size: 13364 / 16384 = 81%
Execution units:
  Memory: 5618116 / 14000000 = 40%
  Steps: 1469159290 / 10000000000 = 14%
```

The closing of the contract paid in the transaction `09e454928aa33bf90d0a103e50ad9217cec90ce853e8005216dbbc0d02ae32f5` the 100 ada principal and 2 ada interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
09e454928aa33bf90d0a103e50ad9217cec90ce853e8005216dbbc0d02ae32f5     0        243796426 lovelace + TxOutDatumNone
09e454928aa33bf90d0a103e50ad9217cec90ce853e8005216dbbc0d02ae32f5     2        3000000 lovelace + 1 5799b669a944f6ecabac80a84e465e44172310251d9a0b3979c47872.4350 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
09e454928aa33bf90d0a103e50ad9217cec90ce853e8005216dbbc0d02ae32f5     1        103000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "e46d1251937442cebee047c1b2248b90f283bc1d48411eb44df729ca7d525b35"
```

## Transaction 6. Lender Withdraws Repayment.

The lender John Fletcher submits a transaction to withdraw the repayment from the role address.

```
TX_6=$(
marlowe-cli run withdraw --testnet-magic "$MAGIC"                                \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-5.marlowe                             \
                         --role-name "$LENDER_ROLE"                              \
                         --tx-in "$TX_2"#0                                       \
                         --tx-in "$TX_2"#3                                       \
                         --tx-in-collateral "$TX_2"#0                            \
                         --required-signer "$LENDER_PAYMENT_SKEY"                \
                         --tx-out "$LENDER_ADDRESS+$MINIMUM_ADA+1 $LENDER_TOKEN" \
                         --change-address "$LENDER_ADDRESS"                      \
                         --out-file tx-6.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)
```

```console
Fee: Lovelace 619909
Size: 5768 / 16384 = 35%
Execution units:
  Memory: 3939076 / 14000000 = 28%
  Steps: 1102672138 / 10000000000 = 11%
```

There is no UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3e7211018b6f77c289f93a08825b7800fc344edb6db3b39316c4cbb0b323bfcc     0        39291255 lovelace + TxOutDatumNone
3e7211018b6f77c289f93a08825b7800fc344edb6db3b39316c4cbb0b323bfcc     1        105000000 lovelace + TxOutDatumNone
3e7211018b6f77c289f93a08825b7800fc344edb6db3b39316c4cbb0b323bfcc     2        3000000 lovelace + 1 5799b669a944f6ecabac80a84e465e44172310251d9a0b3979c47872.5041 + TxOutDatumNone
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
09e454928aa33bf90d0a103e50ad9217cec90ce853e8005216dbbc0d02ae32f5     0        243796426 lovelace + TxOutDatumNone
09e454928aa33bf90d0a103e50ad9217cec90ce853e8005216dbbc0d02ae32f5     2        3000000 lovelace + 1 5799b669a944f6ecabac80a84e465e44172310251d9a0b3979c47872.4350 + TxOutDatumNone
```

## Clean Up

```
BURN_ADDRESS=addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7
marlowe-cli transaction simple --testnet-magic "$MAGIC"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"            \
                               --tx-in "$TX_5"#0                                    \
                               --tx-in "$TX_5"#2                                    \
                               --tx-out "$BURN_ADDRESS+1400000+1 $BORROWER_TOKEN" \
                               --required-signer "$BORROWER_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                   \
                               --out-file /dev/null                                 \
                               --submit 600
```

```console
TxId "6d9f68dccc2aa1987af50ffdde17accdb0f036da3ee84f1ebd56b41684ff0b84"
```

cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
marlowe-cli transaction simple --testnet-magic "$MAGIC"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"            \
                               --tx-in "$TX_6"#0                                    \
                               --tx-in "$TX_6"#1                                    \
                               --tx-in "$TX_6"#2                                    \
                               --tx-out "$BURN_ADDRESS+1400000+1 $LENDER_TOKEN" \
                               --required-signer "$LENDER_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                   \
                               --out-file /dev/null                                 \
                               --submit 600
```

```console
TxId "86cc768c42a907e7298c6e533dc4bb119548505046e5273cc0c0acec9ae3739f"
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
