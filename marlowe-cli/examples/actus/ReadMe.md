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

The tip is at slot 10675237. The current POSIX time implies that the tip of the blockchain should be slightly before slot 10675252. Tests may fail if this is not the case.

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
                        "$LENDER_ADDRESS"
```

```console
TxId "e79fa9d95ce3b7fcf3359c6c03bdafeafb44113a98bab8ad54d72d576cc53b5c"
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
TxId "3cf580e8aedb95dc4e8bc014150e2d33d10107e7cc5317a01be8a8477b33a4d8"
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
TxId "c827f02102d268ae062eadf4c60eae1434403351323a49d6391651508a638560"
```

### Available UTxOs

The lender John Fletcher is the minimum-ADA provider and has the address `addr_test1vrc65aeujcsnpk38j2lc8le0p24zz5t64r7xk8nta4u2vhsqhxhqh` and role token named `PA`. They have the following UTxOs in their wallet:

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
1e056c47072aeba39259a35a0a11380873fafc2db869e1916d4132adb678f032     0        145473665 lovelace + TxOutDatumNone
1e056c47072aeba39259a35a0a11380873fafc2db869e1916d4132adb678f032     1        2000000 lovelace + 1 13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd.5041 + TxOutDatumNone
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

John Fletcher will spend the UTxOs `1e056c47072aeba39259a35a0a11380873fafc2db869e1916d4132adb678f032#0` and `1e056c47072aeba39259a35a0a11380873fafc2db869e1916d4132adb678f032#1`.

The borrower Thomas Middleton has the address `addr_test1vrf342njgcnnnshdcx9da77fzpmeer087mdqnm3slf3xrgge6h72g` and role token named `CP`. They have the following UTxOs in their wallet:

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
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     0        249722871 lovelace + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     1        2000000 lovelace + 1 13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     2        2000000 lovelace + 1 217a06c898adca668cf634e0e5724d24a88a2bca4b4290fa87309320.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     3        2000000 lovelace + 1 3bf1011efc991c0502ac69638c4bc3dc0a2f4953744299ee353a1548.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     4        2000000 lovelace + 1 419bc79e34c72fe9245af18254eecb8f183dd02815204b33d5cfa99c.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     5        2000000 lovelace + 1 7c9c9aad62cef4358cf33890f49cf07b2a214d37f5f31467fa858d6f.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     6        2000000 lovelace + 1 7f7c0faf3b5b991e32a6deb4a1ed8029eb441e7a987f37975d680242.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     7        2000000 lovelace + 1 c7b8a70c81230f18d8ccc6597a6ea5bd04e3c2c6e43fe2e2f3f97996.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     8        2000000 lovelace + 1 cbb3c000e35b41259ecb106e93d7b09f265c462c68eeeb7438782135.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b     9        2000000 lovelace + 1 d99a71370ac53673d7b58949dff4f5b5b50e26e53ed81eb82e1ca0f9.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b    10        2000000 lovelace + 1 d9adad62b2ef2906e0f5abe92988ec1392c407d5fac23bddc700caff.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b    11        2000000 lovelace + 1 e3ecd7de22079678914061c7c9a4be4c3c515e753c2e7e6610b4426c.4350 + TxOutDatumNone
7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b    12        2000000 lovelace + 1 fc877cb55e0711bf85543f60f73341bb790f49530074666bbef9e776.4350 + TxOutDatumNone
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

Thomas Middleton will spend the UTxOs `7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b#0` and `7e804cdf43faeb7ac504e0941d756ca2ab31b819124b89137340e1bb9f8caa2b#1`.

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

The contract has a minimum-ADA requirement and two timeouts. It also specifies that the lender John Fletcher will pay principal of 100 ada before Wed, 17 Aug 2022 00:13:48 +0000 and the borrower will repay the principal and interest of 2 ada before Wed, 17 Aug 2022 12:13:48 +0000.

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

The Marlowe contract resides at address `addr_test1wr0vh6makp7kvazy3tqwamqghrq5d668mcjcvfwykxcshxshjhj4y`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wq489r08f9mk2xa7429x83m3ezf6k6g2kekxu5p98ndhdsc8p2u9l`.

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
Fee: Lovelace 190361
Size: 552 / 32768 = 1%
Execution units:
  Memory: 0 / 30000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA amount of 3000000 lovelace from the lender John Fletcher in the transaction `75a366a34d21d586148bd49397a334779f8645920d70f4e895692bb5c33c5fac`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
75a366a34d21d586148bd49397a334779f8645920d70f4e895692bb5c33c5fac     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "79f10664aa0bee4b3efeee1ddfd9bbc14fcfe6073d00168ad2aa9506bdeb6ea1"
```

Here are the UTxOs at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
75a366a34d21d586148bd49397a334779f8645920d70f4e895692bb5c33c5fac     0        142283304 lovelace + TxOutDatumNone
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
Datum size: 229
Payment 1
  Acccount: "PA"
  Payee: Party "CP"
  Ada: 100.000000
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
Fee: Lovelace 1385995
Size: 13714 / 32768 = 41%
Execution units:
  Memory: 7373202 / 30000000 = 24%
  Steps: 2525603595 / 10000000000 = 25%
```

The contract passed the deposit of 100 ADA in the transaction `c855150235d5dce00c14d96e17965b27b43a4b3b9331f4c417219e55f3dfff6d` from the lender to the role address, for the benefit of the borrower. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c855150235d5dce00c14d96e17965b27b43a4b3b9331f4c417219e55f3dfff6d     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "15c2087d4f298d6302f66a7d956e32a4664fafdf073b532a4f92c9ce51b5cc64"
```

Here is the UTxO at the lender John Fletcher's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$LENDER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c855150235d5dce00c14d96e17965b27b43a4b3b9331f4c417219e55f3dfff6d     0        39897309 lovelace + TxOutDatumNone
c855150235d5dce00c14d96e17965b27b43a4b3b9331f4c417219e55f3dfff6d     3        3000000 lovelace + 1 13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd.5041 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c855150235d5dce00c14d96e17965b27b43a4b3b9331f4c417219e55f3dfff6d     2        100000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "548825393db344988b4584e9ed11919767629cd6857aff1bf425223371e36f65"
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
Fee: Lovelace 422221
Size: 2885 / 32768 = 8%
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
bf62ba32f0fbbefde8f69fb12687f9550d1bdc7bde019d41b465c0c464ff1e4c     0        248300650 lovelace + TxOutDatumNone
bf62ba32f0fbbefde8f69fb12687f9550d1bdc7bde019d41b465c0c464ff1e4c     1        100000000 lovelace + TxOutDatumNone
bf62ba32f0fbbefde8f69fb12687f9550d1bdc7bde019d41b465c0c464ff1e4c     2        3000000 lovelace + 1 13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd.4350 + TxOutDatumNone
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
Datum size: 137
Payment 1
  Acccount: "CP"
  Payee: Party "PA"
  Ada: 2.000000
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
Fee: Lovelace 1412646
Size: 13566 / 32768 = 41%
Execution units:
  Memory: 7772986 / 30000000 = 25%
  Steps: 2603978648 / 10000000000 = 26%
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
  Ada: 100.000000
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
Fee: Lovelace 1111941
Size: 13230 / 32768 = 40%
Execution units:
  Memory: 4300844 / 30000000 = 14%
  Steps: 1478686295 / 10000000000 = 14%
```

The closing of the contract paid in the transaction `0024e2509a44acdb2e06a3682b13e7aa635aa965eea10f7a5d6c27eeb4df073e` the 100 ada principal and 2 ada interest to the role address for the benefit of the lender John Fletcher, along with the minimum ADA 3000000 lovelace that they deposited when creating the contract. There is no UTxO at the contract address:

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
0024e2509a44acdb2e06a3682b13e7aa635aa965eea10f7a5d6c27eeb4df073e     0        243776063 lovelace + TxOutDatumNone
0024e2509a44acdb2e06a3682b13e7aa635aa965eea10f7a5d6c27eeb4df073e     2        3000000 lovelace + 1 13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd.4350 + TxOutDatumNone
```

Here is the UTxO at the role address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0024e2509a44acdb2e06a3682b13e7aa635aa965eea10f7a5d6c27eeb4df073e     1        103000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "e8011272a762a3197516f70018f50fe8c574c942edc9da49499f72570652b7de"
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
Fee: Lovelace 606543
Size: 5412 / 32768 = 16%
Execution units:
  Memory: 3567988 / 30000000 = 11%
  Steps: 1336323020 / 10000000000 = 13%
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
8098bf7d83d87f0e30ac62b5ef4dc3b3a87b0518a0c565aba05c165a46e6e5cf     0        39290766 lovelace + TxOutDatumNone
8098bf7d83d87f0e30ac62b5ef4dc3b3a87b0518a0c565aba05c165a46e6e5cf     1        105000000 lovelace + TxOutDatumNone
8098bf7d83d87f0e30ac62b5ef4dc3b3a87b0518a0c565aba05c165a46e6e5cf     2        3000000 lovelace + 1 13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd.5041 + TxOutDatumNone
```

Here are the UTxOs at the borrower Thomas Middleton's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BORROWER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p;/$TX_6/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0024e2509a44acdb2e06a3682b13e7aa635aa965eea10f7a5d6c27eeb4df073e     0        243776063 lovelace + TxOutDatumNone
0024e2509a44acdb2e06a3682b13e7aa635aa965eea10f7a5d6c27eeb4df073e     2        3000000 lovelace + 1 13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd.4350 + TxOutDatumNone
```

## Clean Up

```
FAUCET_ADDRESS=addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8
TX_7=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 1                         \
                        "$BORROWER_ADDRESS"                       \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1/;p}'            \
)
marlowe-cli transaction simple --testnet-magic "$MAGIC"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"            \
                               --tx-in "$TX_7"#0                                    \
                               --tx-in "$TX_7"#2                                    \
                               --tx-out "$LENDER_ADDRESS+1400000+1 $BORROWER_TOKEN" \
                               --required-signer "$BORROWER_PAYMENT_SKEY"           \
                               --change-address "$FAUCET_ADDRESS"                   \
                               --out-file /dev/null                                 \
                               --submit 600
```

```console
TxId "dc7c5a8d3ddfef80ebd9686d9f994cfaba910b409de1860e376ee3c6e94ed02c"
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
PolicyID "13be73c09530a0d40bf464b5cf9954722e6a059adeb8311612ebf1fd"
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
TxId "255e41c7b10e46eeb33edd99629f4eb0d904be1f9aed926c82d9319544633add"
