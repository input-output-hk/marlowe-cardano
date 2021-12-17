# Test of a Trivial Contract

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the echo path to the cardano node's socket.

The following tools must be on the PATH:
* marlowe-cli
* cardano-cli
* sed
* jq
* xargs

Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `BYSTANDER_PREFIX` and `PARTY_PREFIX` where they appear below.

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

### Select Parties

#### The Bystander

The bystander simply provides the minimum ada to be held in the contract while it is active.

```
BYSTANDER_PREFIX="$TREASURY/christopher-marlowe"
BYSTANDER_NAME="Christopher Marlowe"
BYSTANDER_PAYMENT_SKEY="$BYSTANDER_PREFIX".skey
BYSTANDER_PAYMENT_VKEY="$BYSTANDER_PREFIX".vkey
BYSTANDER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                             \
                            --payment-verification-key-file "$BYSTANDER_PAYMENT_VKEY" \
)
BYSTANDER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$BYSTANDER_PAYMENT_VKEY"
)
```

The bystander Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vqhqudxtwqcpjqesns79hqgqq2q0xx5q0hnzz5es9492yaqpxltpy` and public-key hash `2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274`. They have the following UTxOs in their wallet:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
751b05f06c1242c62c7e9f9f05058e83de840257e0c4cbfa3529ba81dfa3edbe     0        8968183459 lovelace + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_BYSTANDER=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$BYSTANDER_ADDRESS"                  \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
```

Christopher Marlowe will spend the UTxO `751b05f06c1242c62c7e9f9f05058e83de840257e0c4cbfa3529ba81dfa3edbe#0`.

### The Party

The party deposits and removes funds from the contract.

```
PARTY_PREFIX="$TREASURY/francis-beaumont"
PARTY_NAME="Francis Beaumont"
PARTY_PAYMENT_SKEY="$PARTY_PREFIX".skey
PARTY_PAYMENT_VKEY="$PARTY_PREFIX".vkey
PARTY_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                         \
                            --payment-verification-key-file "$PARTY_PAYMENT_VKEY" \
)
PARTY_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_PAYMENT_VKEY"
)
```

The party Francis Beaumont has the address `addr_test1vrtntkszteptml4e9ce9l3fsmgavwv4ywunvdnhxv6nw5ksq6737a` and the public-key hash `d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a`. They have the following UTxOs in their wallet:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d20fee3be431b378dadcb9d3558b1a16a8d19b67683e2744d746f51be0c8c4eb     0        17927003312 lovelace + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_PARTY=$(
cardano-cli query utxo "${MAGIC[@]}"                                   \
                       --address "$PARTY_ADDRESS"                      \
                       --out-file /dev/stdout                          \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[0].key' \
)
```

Francis Beaumont will spend the UTxO `d20fee3be431b378dadcb9d3558b1a16a8d19b67683e2744d746f51be0c8c4eb#0`.

### Validator Script and Address

The contract has a validator script and address. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

```
CONTRACT_ADDRESS=$(
marlowe-cli export-address "${MAGIC[@]}" \
            --slot-length "$SLOT_LENGTH" \
            --slot-offset "$SLOT_OFFSET" \
)
marlowe-cli export-validator "${MAGIC[@]}"                \
                             --slot-length "$SLOT_LENGTH" \
                             --slot-offset "$SLOT_OFFSET" \
                             --out-file trivial.plutus    \
                             --print-stats
```

```console
addr_test1wq3n0x7khejv6r0k3rzjw3tfwjjxlmvc7e5zxa8z4jwc57ca4jg8m

Validator size: 13848
Validator cost: ExBudget {exBudgetCPU = ExCPU 37484307, exBudgetMemory = ExMemory 126000}
```

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 1560735. The current POSIX time implies that the tip of the blockchain should be slightly before slot 1560746. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and a timeout:

```
MINIMUM_SLOT="$TIP"
TIMEOUT_SLOT="$(($TIP+10*24*3600))"
```

The contract starts no sooner than slot 1560735 and will automatically close at slot 2424735.

The contract also involves various payments:

```
MINIMUM_ADA=3000000
DEPOSIT_LOVELACE=12000000
WITHDRAWAL_LOVELACE=5000000
CLOSE_LOVELACE=$(($DEPOSIT_LOVELACE-$WITHDRAWAL_LOVELACE))
```

The bystander Christopher Marlowe will provide 3000000 lovelace during the contract's operation, so that it satisfies the minimmum-ADA requirement. The party Francis Beaumont will deposit 12000000 lovelace at the start of the contract. They will wait until notified to withdraw 5000000 lovelace. After another notification, the party Francis Beaumont will withdrawn the remaining 7000000 lovelace and the bystander Christopher Marlowe will receive their 3000000 lovelace back. This is expressed in the Marlowe language [here](../../src/Language/Marlowe/CLI/Examples/Trivial.hs).

## Transaction 1. Create the Contract by Providing the Minimum ADA

We create the contract for the previously specified parameters.

```
marlowe-cli contract-trivial --bystander "PK=$BYSTANDER_PUBKEYHASH"       \
                             --minimum-ada "$MINIMUM_ADA"                 \
                             --minimum-slot "$MINIMUM_SLOT"               \
                             --party "PK=$PARTY_PUBKEYHASH"               \
                             --deposit-lovelace "$DEPOSIT_LOVELACE"       \
                             --withdrawal-lovelace "$WITHDRAWAL_LOVELACE" \
                             --timeout "$TIMEOUT_SLOT"                    \
                             --out-file tx-1.marlowe
```

We extract the initial state and full contract from the `.marlowe`file that contains comprehensive information.

```
jq '.marloweState'    tx-1.marlowe > tx-1.state
jq '.marloweContract' tx-1.marlowe > tx-1.contract
```

For each transaction, we construct the output datum. Here is its size and hash:

```
marlowe-cli export-datum --contract-file tx-1.contract \
                         --state-file    tx-1.state    \
                         --out-file      tx-1.datum    \
                         --print-stats
```

```console
Datum size: 316
a4beacab292925e080880b4a4608eb394445bc37f6fa130fe1114a8fa67cdecb
```


The bystander Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli transaction-create "${MAGIC[@]}"                               \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                               --tx-in "$TX_0_BYSTANDER"                   \
                               --change-address "$BYSTANDER_ADDRESS"       \
                               --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                               --script-address "$CONTRACT_ADDRESS"        \
                               --tx-out-datum-file tx-1.datum              \
                               --tx-out-marlowe "$MINIMUM_ADA"             \
                               --out-file tx-1.raw                         \
                               --print-stats                               \
                               --submit=600                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                           \
)
```

```console

Fee: Lovelace 184553
Size: 521 / 32768 = 1%
Execution units:
  Memory: 0 / 10000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the bystander Christopher Marlowe in the transaction `fceb8553f8c754e81c5a74edb0dbe85f5bf12e9c35f8724a3661b84ffb02e568`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fceb8553f8c754e81c5a74edb0dbe85f5bf12e9c35f8724a3661b84ffb02e568     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "a4beacab292925e080880b4a4608eb394445bc37f6fa130fe1114a8fa67cdecb"
```

Here is the UTxO at Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fceb8553f8c754e81c5a74edb0dbe85f5bf12e9c35f8724a3661b84ffb02e568     0        8964998906 lovelace + TxOutDatumNone
```

## Transaction 2. Make the Initial Deposit

First we compute the Marlowe input required to make the initial deposit by the party.

```
marlowe-cli input-deposit --deposit-account "PK=$PARTY_PUBKEYHASH" \
                          --deposit-party "PK=$PARTY_PUBKEYHASH"   \
                          --deposit-amount "$DEPOSIT_LOVELACE"     \
                          --out-file tx-2.input
```

Next we compute the transition caused by that input to the contract.

```
marlowe-cli compute --contract-file tx-1.contract          \
                    --state-file    tx-1.state             \
                    --input-file    tx-2.input             \
                    --out-file      tx-2.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats
```

```console

Datum size: 260
```

As in the first transaction, we compute the new state and contract.

```
jq '.state'    tx-2.marlowe > tx-2.state
jq '.contract' tx-2.marlowe > tx-2.contract
```

Because this transaction spends from the script address, it also needs a redeemer:

```
marlowe-cli export-redeemer --input-file tx-2.input    \
                            --out-file   tx-2.redeemer \
                            --print-stats
```

```console
Redeemer size: 85
```

As in the first transaction, we compute the datum and its hash:

```
marlowe-cli export-datum --contract-file tx-2.contract \
                         --state-file    tx-2.state    \
                         --out-file      tx-2.datum    \
                         --print-stats
```

```console

Datum size: 260
a16fd63a67a835591ea62bfa868b067b51f05f772d76c988537bd08c9fcbaa7e
```

The value held at the contract address must match that required by its state.

```
CONTRACT_VALUE_2=$(jq '.accounts | [.[][1]] | add' tx-2.state)
```

Now the party Francis Beaumont submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                --script-address "$CONTRACT_ADDRESS"      \
                                --tx-in-marlowe "$TX_1"#1                 \
                                --tx-in-script-file trivial.plutus        \
                                --tx-in-datum-file tx-1.datum             \
                                --tx-in-redeemer-file tx-2.redeemer       \
                                --tx-in "$TX_0_PARTY"                     \
                                --tx-in-collateral "$TX_0_PARTY"          \
                                --required-signer "$PARTY_PAYMENT_SKEY"   \
                                --tx-out-marlowe "$CONTRACT_VALUE_2"      \
                                --tx-out-datum-file tx-2.datum            \
                                --tx-out "$PARTY_ADDRESS+$MINIMUM_ADA"    \
                                --change-address "$PARTY_ADDRESS"         \
                                --invalid-before "$TIP"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"    \
                                --out-file tx-2.raw                       \
                                --print-stats                             \
                                --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                          \
)
```

```console
Fee: Lovelace 1106307
Size: 14884 / 32768 = 45%
Execution units:
  Memory: 3355810 / 10000000 = 33%
  Steps: 1209094962 / 10000000000 = 12%
```

The contract received the deposit of 12000000 lovelace from the party Francis Beaumont in the transaction `90503007175155334e1e3d9bc65d68025808193a2620d9373f10f9a8be538afa`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
90503007175155334e1e3d9bc65d68025808193a2620d9373f10f9a8be538afa     1        15000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "a16fd63a67a835591ea62bfa868b067b51f05f772d76c988537bd08c9fcbaa7e"
```

Here is the UTxO at Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
90503007175155334e1e3d9bc65d68025808193a2620d9373f10f9a8be538afa     0        17910897005 lovelace + TxOutDatumNone
90503007175155334e1e3d9bc65d68025808193a2620d9373f10f9a8be538afa     2        3000000 lovelace + TxOutDatumNone
```

## Transaction 3. Make the First Withdrawal

First we compute the input for the contract to transition forward:

```
marlowe-cli input-notify --out-file tx-3.input
```

As in the second transaction we compute the contract's transition, its new state, the redeemer, the datum, and the value.

```
marlowe-cli compute --contract-file tx-2.contract          \
                    --state-file    tx-2.state             \
                    --input-file    tx-3.input             \
                    --out-file      tx-3.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats
```

```console

Payment 1
  Acccount: PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"
  Payee: Party (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
  Ada: 5.000000
Datum size: 143
```

```
jq '.state'    tx-3.marlowe > tx-3.state
jq '.contract' tx-3.marlowe > tx-3.contract
marlowe-cli export-redeemer --input-file tx-3.input    \
                            --out-file   tx-3.redeemer \
                            --print-stats
```

```console
Redeemer size: 5
```

```
marlowe-cli export-datum --contract-file tx-3.contract \
                         --state-file    tx-3.state    \
                         --out-file      tx-3.datum    \
                         --print-stats
```

```console
Datum size: 143
4b2d4d43dbdbf534fa593e7427d654d02ade0ddff3de28213cf64e2031ddb23e
```


```
CONTRACT_VALUE_3=$(jq '.accounts | [.[][1]] | add' tx-3.state)
```

Now the party Francis Beaumont can submit a transaction to withdraw funds:

```
TX_3=$(
marlowe-cli transaction-advance "${MAGIC[@]}"                                  \
                                --socket-path "$CARDANO_NODE_SOCKET_PATH"      \
                                --script-address "$CONTRACT_ADDRESS"           \
                                --tx-in-marlowe "$TX_2"#1                      \
                                --tx-in-script-file trivial.plutus             \
                                --tx-in-datum-file tx-2.datum                  \
                                --tx-in-redeemer-file tx-3.redeemer            \
                                --tx-in "$TX_2"#0                              \
                                --tx-in-collateral "$TX_2"#0                   \
                                --required-signer "$PARTY_PAYMENT_SKEY"        \
                                --tx-out-marlowe "$CONTRACT_VALUE_3"           \
                                --tx-out-datum-file tx-3.datum                 \
                                --tx-out "$PARTY_ADDRESS+$WITHDRAWAL_LOVELACE" \
                                --change-address "$PARTY_ADDRESS"              \
                                --invalid-before "$(($TIP))"                   \
                                --invalid-hereafter "$(($TIP+4*3600))"         \
                                --out-file tx-3.raw                            \
                                --print-stats                                  \
                                --submit=600                                   \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```


```console
Fee: Lovelace 1237082
Size: 14631 / 32768 = 44%
Execution units:
  Memory: 5169076 / 10000000 = 51%
  Steps: 1726164016 / 10000000000 = 17%
```

The contract made a payment of 5000000 lovelace to the party Francis Beaumont in the transaction `333fa387519e2f967ad4dad68ec5df1b8776e37c8a8814415d9fa3c9ddc54b7c`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
333fa387519e2f967ad4dad68ec5df1b8776e37c8a8814415d9fa3c9ddc54b7c     1        10000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "4b2d4d43dbdbf534fa593e7427d654d02ade0ddff3de28213cf64e2031ddb23e"
```

Here is the UTxO at Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
333fa387519e2f967ad4dad68ec5df1b8776e37c8a8814415d9fa3c9ddc54b7c     0        17909659923 lovelace + TxOutDatumNone
333fa387519e2f967ad4dad68ec5df1b8776e37c8a8814415d9fa3c9ddc54b7c     2        5000000 lovelace + TxOutDatumNone
```

## Transaction 4. Close the contract

As in the third transaction, we compute the input for the contract to transition forward:

```
marlowe-cli input-notify --out-file tx-4.input
```

Once again we compute the transition, new state and contract, and redeemer. Because we are closing the contract, we don't need a new datum.

```
marlowe-cli compute --contract-file tx-3.contract          \
                    --state-file    tx-3.state             \
                    --input-file    tx-4.input             \
                    --out-file      tx-4.marlowe           \
                    --invalid-before "$TIP"                \
                    --invalid-hereafter "$(($TIP+4*3600))" \
                    --print-stats
```

```console
Payment 1
  Acccount: PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274"
  Payee: Party (PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274")
  Ada: 3.000000
Payment 2
  Acccount: PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"
  Payee: Party (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
  Ada: 7.000000
Datum size: 19
```

```
jq '.state'    tx-4.marlowe > tx-4.state
jq '.contract' tx-4.marlowe > tx-4.contract
marlowe-cli export-redeemer --input-file tx-4.input    \
                            --out-file   tx-4.redeemer \
                            --print-stats
```

```console
Redeemer size: 5
```

```
TX_4=$(
marlowe-cli transaction-close "${MAGIC[@]}"                              \
                              --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                              --tx-in-marlowe "$TX_3"#1                  \
                              --tx-in-script-file trivial.plutus         \
                              --tx-in-datum-file tx-3.datum              \
                              --tx-in-redeemer-file tx-4.redeemer        \
                              --tx-in "$TX_3"#0                          \
                              --tx-in-collateral "$TX_3"#0               \
                              --required-signer "$PARTY_PAYMENT_SKEY"    \
                              --tx-out "$PARTY_ADDRESS+$CLOSE_LOVELACE"  \
                              --change-address "$PARTY_ADDRESS"          \
                              --tx-out "$BYSTANDER_ADDRESS+$MINIMUM_ADA" \
                              --invalid-before "$TIP"                    \
                              --invalid-hereafter "$(($TIP+4*3600))"     \
                              --out-file tx-4.raw                        \
                              --print-stats                              \
                              --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                         \
)
```

```console
Fee: Lovelace 1078228
Size: 14337 / 32768 = 43%
Execution units:
  Memory: 3370016 / 10000000 = 33%
  Steps: 1142085457 / 10000000000 = 11%
```

The closing of the contract paid 7000000 lovelace to the Francis Beaumont and 3000000 lovelace to the bystander Christopher Marlowe in the transaction `64a9d7d41b57e12ef4cfcdc2a0ff89c977ddf4defcebbf0bcd1acbe3229f5c92`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the bystander Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
64a9d7d41b57e12ef4cfcdc2a0ff89c977ddf4defcebbf0bcd1acbe3229f5c92     2        3000000 lovelace + TxOutDatumNone
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
64a9d7d41b57e12ef4cfcdc2a0ff89c977ddf4defcebbf0bcd1acbe3229f5c92     0        17908581695 lovelace + TxOutDatumNone
64a9d7d41b57e12ef4cfcdc2a0ff89c977ddf4defcebbf0bcd1acbe3229f5c92     1        7000000 lovelace + TxOutDatumNone
```

## Clean Up Wallets

It's convenient to consolidate all of the UTxOs into single ones.

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS" --out-file /dev/stdout \
| jq '. | to_entries[] | .key'                                                             \
| sed -e 's/"//g;s/^/--tx-in /'                                                            \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                               \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                                               --change-address "$BYSTANDER_ADDRESS"       \
                                               --out-file tx-5.raw                         \
                                               --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                                               --submit=600                                \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" --out-file /dev/stdout   \
| jq '. | to_entries[] | .key'                                                           \
| sed -e 's/"//g;s/^/--tx-in /'                                                          \
| xargs -n 9999 marlowe-cli transaction-simple "${MAGIC[@]}"                             \
                                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                               --change-address "$PARTY_ADDRESS"         \
                                               --out-file tx-6.raw                       \
                                               --required-signer "$PARTY_PAYMENT_SKEY"   \
                                               --submit=600                              \
> /dev/null
```
