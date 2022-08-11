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

### Select Network

```
if [[ -z "$MAGIC" ]]
then
  MAGIC=1567
fi
```

MAGIC=1566

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

The tip is at slot 10271870. The current POSIX time implies that the tip of the blockchain should be slightly before slot 10271903. Tests may fail if this is not the case.

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
                        --lovelace 15000000000                    \
                        "$LENDER_ADDRESS"
```

```console
TxId "f5ff564f0d73715db3b8a1f07ced7d697af07ba9e7ca56a492ec75505eac9b29"
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
                        "$BORROWER_ADDRESS"
```

```console
TxId "36be04c7b71aec788b1e67d4221b82a415f7786b22d034dcab0453749fdb97e7"
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
TxId "787ca5036fff362ade62b7c14cbffab11f5d60cb37bb6e588cfb3cfcf3a77a53"
```

### Available UTxOs

The lender John Fletcher is the minimum-ADA provider and has the address `addr_test1vr2fl7n77kshgjuz8ckde7rzh3yze2ndyfr974pl98plh0q2yu67x` and role token named `PA`. They have the following UTxOs in their wallet:

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
fb3cdb35ac2b26c044a8826f1bf0f3530e3433171cacbc5efbfb6458bd80b8de     0        15011436177 lovelace + TxOutDatumNone
fb3cdb35ac2b26c044a8826f1bf0f3530e3433171cacbc5efbfb6458bd80b8de     1        2000000 lovelace + 1 23155fe6a2d195227447a4078bd4d2ec4f5f2e18bf966fdb27515c7c.5041 + TxOutDatumNone
fb3cdb35ac2b26c044a8826f1bf0f3530e3433171cacbc5efbfb6458bd80b8de     2        2000000 lovelace + 1 a17e7b8b6e467276a61570fc460ebe6caa1f13f659ade4db87b169b8.5041 + TxOutDatumNone
fb3cdb35ac2b26c044a8826f1bf0f3530e3433171cacbc5efbfb6458bd80b8de     3        2000000 lovelace + 1 c0b50e79f05efbd7cd336878bff0e59917deed7cac2520f341e4d121.5041 + TxOutDatumNone
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

John Fletcher will spend the UTxOs `fb3cdb35ac2b26c044a8826f1bf0f3530e3433171cacbc5efbfb6458bd80b8de#0` and `fb3cdb35ac2b26c044a8826f1bf0f3530e3433171cacbc5efbfb6458bd80b8de#1`.

The borrower Thomas Middleton has the address `addr_test1vqaxyj4l7hc0upzrkvfgctg583zav4s9ahlpepx7eu8trasgdeeqe` and role token named `CP`. They have the following UTxOs in their wallet:

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
88c4ef34de95bc34e66718c1644f028bf9053a5c639801d701e5a4a80460e3d8     0        249806427 lovelace + TxOutDatumNone
88c4ef34de95bc34e66718c1644f028bf9053a5c639801d701e5a4a80460e3d8     1        2000000 lovelace + 1 23155fe6a2d195227447a4078bd4d2ec4f5f2e18bf966fdb27515c7c.4350 + TxOutDatumNone
88c4ef34de95bc34e66718c1644f028bf9053a5c639801d701e5a4a80460e3d8     2        2000000 lovelace + 1 a17e7b8b6e467276a61570fc460ebe6caa1f13f659ade4db87b169b8.4350 + TxOutDatumNone
88c4ef34de95bc34e66718c1644f028bf9053a5c639801d701e5a4a80460e3d8     3        2000000 lovelace + 1 c0b50e79f05efbd7cd336878bff0e59917deed7cac2520f341e4d121.4350 + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `88c4ef34de95bc34e66718c1644f028bf9053a5c639801d701e5a4a80460e3d8#0` and `88c4ef34de95bc34e66718c1644f028bf9053a5c639801d701e5a4a80460e3d8#1`.

## The Contract

```
MINIMUM_ADA=3000000
FIXED_POINT=1000000
PRINCIPAL=10000
INTEREST_RATE=1/50
INTEREST=$((PRINCIPAL*$INTEREST_RATE))
STATUS_DATE=$(date -d "$(date -u -R -d @$(($NOW/1000)))" +"%Y-%m-%dT00:00:00")
INITIAL_EXCHANGE_DATE=$(date -d "$(date -u -R -d @$(($NOW/1000))) + 1 year" +"%Y-01-01T00:00:00")
MATURITY_DATE=$(date -d "$(date -u -R -d @$(($NOW/1000))) + 2 year" +"%Y-01-01T00:00:00")
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
  "nominalInterestRate": `jq -n $INTEREST_RATE`,
  "interestCalculationBase": "NT"
}
EOF
```

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 10000 ada before Fri, 12 Aug 2022 08:11:01 +0000 and the borrower will repay the principal and interest of 200 ada before Fri, 12 Aug 2022 20:11:01 +0000.

We create the contract for the previously specified parameters.

```
marlowe-cli template actus --minimum-ada "$MINIMUM_ADA"               \
                           --party "Role=$LENDER_ROLE"                \
                           --counter-party "Role=$BORROWER_ROLE"      \
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
Validator size: 12611
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 24562825, exBudgetMemory = ExMemory 82600}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wq07qd5ph5l49y89wfx00k8f9tsndwy9rgre95cg30eutschr4le5`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wr6m6dv8v75gl7hfmnzaltlrph8wr90vqg0m72a4gwrzphcd7qsj4`.

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
Fee: Lovelace 191241
Size: 572 / 32768 = 1%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the lender John Fletcher in the transaction `06ee9291aada5c6d8a6f4a29646f5c0d4619697a5f9ccd0de9313256f02635f1`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
06ee9291aada5c6d8a6f4a29646f5c0d4619697a5f9ccd0de9313256f02635f1     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "2bea72ae5aba7c5f1e4df9ebec7c34dd25be9ee377a0a9523b3c14a51b81a7b1"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
06ee9291aada5c6d8a6f4a29646f5c0d4619697a5f9ccd0de9313256f02635f1     0        15008244936 lovelace + TxOutDatumNone
```

## Transaction 2. Lender Deposits the Loan Amount

First we compute the Marlowe input required to deposit the funds for the loan.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe                   \
                        --deposit-account "Role=$LENDER_ROLE"         \
                        --deposit-party "Role=$LENDER_ROLE"           \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$NOW"                       \
                        --invalid-hereafter "$((NOW+4*HOUR))"         \
                        --out-file tx-2.marlowe                       \
                        --print-stats
```

```console
Datum size: 237
Payment 1
  Acccount: "PA"
  Payee: Party "CP"
  Ada: 10000.000000
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
Fee: Lovelace 1397789
Size: 13750 / 32768 = 41%
Execution units:
  Memory: 7501806 / 30000000 = 25%
  Steps: 2564286091 / 10000000000 = 25%
```

The contract passed the deposit of 10000 ADA in the transaction `ddbfdb164c204f627f8ab7f849c96a901901d0b6fecb2307eda60ca1f921d5ef` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ddbfdb164c204f627f8ab7f849c96a901901d0b6fecb2307eda60ca1f921d5ef     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "7817a4b67666c2d695e6bcdec26ed67f72f51c89f99d72a6a7d6502290ae3c3e"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ddbfdb164c204f627f8ab7f849c96a901901d0b6fecb2307eda60ca1f921d5ef     0        5005847147 lovelace + TxOutDatumNone
ddbfdb164c204f627f8ab7f849c96a901901d0b6fecb2307eda60ca1f921d5ef     3        3000000 lovelace + 1 23155fe6a2d195227447a4078bd4d2ec4f5f2e18bf966fdb27515c7c.5041 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ddbfdb164c204f627f8ab7f849c96a901901d0b6fecb2307eda60ca1f921d5ef     2        10000000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "548825393db344988b4584e9ed11919767629cd6857aff1bf425223371e36f65"
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
Fee: Lovelace 422397
Size: 2889 / 32768 = 8%
Execution units:
  Memory: 1407010 / 30000000 = 4%
  Steps: 541567360 / 10000000000 = 5%
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
3edb13c32a491a25a53d0de4a85bb742aec1163e452c8f92518400668037024e     0        248384030 lovelace + TxOutDatumNone
3edb13c32a491a25a53d0de4a85bb742aec1163e452c8f92518400668037024e     1        10000000000 lovelace + TxOutDatumNone
3edb13c32a491a25a53d0de4a85bb742aec1163e452c8f92518400668037024e     2        3000000 lovelace + 1 23155fe6a2d195227447a4078bd4d2ec4f5f2e18bf966fdb27515c7c.4350 + TxOutDatumNone
```

## Transaction 4. Borrower Repays the Loan's Interest

First we compute the Marlowe input required to repay the intest of the loan.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe                  \
                        --deposit-account "Role=$BORROWER_ROLE"      \
                        --deposit-party "Role=$BORROWER_ROLE"        \
                        --deposit-amount "$((INTEREST*FIXED_POINT))" \
                        --invalid-before "$NOW"                      \
                        --invalid-hereafter "$((NOW+4*HOUR))"        \
                        --out-file tx-4.marlowe                      \
                        --print-stats
```

```console
Datum size: 145
Payment 1
  Acccount: "CP"
  Payee: Party "PA"
  Ada: 200.000000
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
Fee: Lovelace 1413201
Size: 13586 / 32768 = 41%
Execution units:
  Memory: 7768984 / 30000000 = 25%
  Steps: 2602674203 / 10000000000 = 26%
```

## Transaction 5. Borrower Repays the Loan's Principal

First we compute the Marlowe input required to repay the principal of the loan.

```
marlowe-cli run prepare --marlowe-file tx-4.marlowe                   \
                        --deposit-account "Role=$BORROWER_ROLE"       \
                        --deposit-party "Role=$BORROWER_ROLE"         \
                        --deposit-amount "$((PRINCIPAL*FIXED_POINT))" \
                        --invalid-before "$NOW"                       \
                        --invalid-hereafter "$((NOW+4*HOUR))"         \
                        --out-file tx-5.marlowe                       \
                        --print-stats
```

```console
Datum size: 23
Payment 1
  Acccount: "CP"
  Payee: Party "PA"
  Ada: 10000.000000
Payment 2
  Acccount: "PA"
  Payee: Party "PA"
  Ada: 3.000000
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
Fee: Lovelace 1112645
Size: 13246 / 32768 = 40%
Execution units:
  Memory: 4300844 / 30000000 = 14%
  Steps: 1478686295 / 10000000000 = 14%
```

The closing of the contract paid in the transaction `20f1986772267260e2c3ef4dd5f5c78b94ab41b97385dff8452818860bdf47cd` the 10000 ada principal and 200 ada interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

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
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
75214b3aadf3cb639a6462e10ec3906985fffc2e3c7fef89a25019d90dd466f8     2        200000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "e8011272a762a3197516f70018f50fe8c574c942edc9da49499f72570652b7de"
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
Fee: Lovelace 606895
Size: 5420 / 32768 = 16%
Execution units:
  Memory: 3567988 / 30000000 = 11%
  Steps: 1336323020 / 10000000000 = 13%
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
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
20f1986772267260e2c3ef4dd5f5c78b94ab41b97385dff8452818860bdf47cd     0        45858184 lovelace + TxOutDatumNone
20f1986772267260e2c3ef4dd5f5c78b94ab41b97385dff8452818860bdf47cd     2        3000000 lovelace + 1 23155fe6a2d195227447a4078bd4d2ec4f5f2e18bf966fdb27515c7c.4350 + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
marlowe-cli transaction simple --testnet-magic "$MAGIC"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"            \
                               --tx-in "$TX_5"#0                                    \
                               --tx-in "$TX_5"#2                                    \
                               --tx-out "$LENDER_ADDRESS+1400000+1 $BORROWER_TOKEN" \
                               --required-signer "$BORROWER_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                   \
                               --out-file /dev/null                                 \
                               --submit 600
```

```console
TxId "4fbfc410ff93c37e5b8ac3d0e7774d6e56d7bc58561f462dc8369c60a59a1250"
```

marlowe-cli util mint --testnet-magic "$MAGIC" \
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
PolicyID "23155fe6a2d195227447a4078bd4d2ec4f5f2e18bf966fdb27515c7c"
TX=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$LENDER_ADDRESS"                         \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'         \
)
marlowe-cli transaction simple --testnet-magic "$MAGIC"                  \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX"                             \
                               --required-signer "$LENDER_PAYMENT_SKEY"  \
                               --change-address "$FAUCET_ADDRESS"        \
                               --out-file /dev/null                      \
                               --submit 600
```

```console
TxId "e8d159bc514c4fb07292576144558567bf86641763e152f220057cd72911f6ca"
