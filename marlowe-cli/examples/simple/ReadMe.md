# Test of a Simple Contract

[This simple contract](../../src/Language/Marlowe/CLI/Examples/Trivial.hs) takes as deposit, waits for a notification, makes a payment, waits for another notification, and then closes the contract:

```
When
    [Case
        (Deposit
            (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
            (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
            (Token "" "")
            (Constant 12)
        )
        (When
            [Case
                (Notify TrueObs)
                (Pay
                    (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
                    (Party (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"))
                    (Token "" "")
                    (Constant 5)
                    (When
                        [Case
                            (Notify TrueObs)
                            Close ]
                        2582625 Close 
                    )
                )]
            2582624 Close 
        )]
    2582623 Close 
```

![Simple Marlowe Contract](simple-0.png)

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `BYSTANDER_PREFIX` and `PARTY_PREFIX` where they appear below.

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
  SLOT_OFFSET=1644929640000
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
marlowe-cli util clean "${MAGIC[@]}"                               \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                       --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                       --change-address "$BYSTANDER_ADDRESS"       \
                       --out-file /dev/null                        \
                       --submit=600                                \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
fe206385b746896227f5ca159c3269e13e23a1c630127e10d4332251756d5712     0        1940658139 lovelace + TxOutDatumNone
fe206385b746896227f5ca159c3269e13e23a1c630127e10d4332251756d5712     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
fe206385b746896227f5ca159c3269e13e23a1c630127e10d4332251756d5712     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ChristopherMarlowe + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_BYSTANDER=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$BYSTANDER_ADDRESS"                                                           \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)
```

Christopher Marlowe will spend the UTxO `fe206385b746896227f5ca159c3269e13e23a1c630127e10d4332251756d5712#0`.

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
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_PAYMENT_SKEY"   \
                       --change-address "$PARTY_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9b93c82ec67302afdc15c0b41ce65dbfff1c822da74cd61489ae6a3c4d59eec8     0        2962594900 lovelace + TxOutDatumNone
9b93c82ec67302afdc15c0b41ce65dbfff1c822da74cd61489ae6a3c4d59eec8     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
9b93c82ec67302afdc15c0b41ce65dbfff1c822da74cd61489ae6a3c4d59eec8     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FrancisBeaumont + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_PARTY=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$PARTY_ADDRESS"                                                               \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1                                                                                                     \
)
```

Francis Beaumont will spend the UTxO `9b93c82ec67302afdc15c0b41ce65dbfff1c822da74cd61489ae6a3c4d59eec8#0`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
```

The tip is at slot 50842249. The current POSIX time implies that the tip of the blockchain should be slightly before slot 50842250. Tests may fail if this is not the case.

## The Contract

The contract has a minimum time and a timeout.

```
MINIMUM_TIME="$NOW"
TIMEOUT_TIME="$((NOW+24*HOUR))"
```

The contract starts no sooner than Fri, 18 Feb 2022 19:11:05 +0000 and will automatically close at Sat, 19 Feb 2022 19:11:05 +0000.

The contract also involves various payments.

```
MINIMUM_ADA=3000000
DEPOSIT_LOVELACE=12000000
WITHDRAWAL_LOVELACE=5000000
CLOSE_LOVELACE=$((DEPOSIT_LOVELACE-WITHDRAWAL_LOVELACE))
```

The bystander Christopher Marlowe will provide 3000000 lovelace during the contract's operation, so that it satisfies the minimmum-ADA requirement. The party Francis Beaumont will deposit 12000000 lovelace at the start of the contract. They will wait until notified to withdraw 5000000 lovelace. After another notification, the party Francis Beaumont will withdrawn the remaining 7000000 lovelace and the bystander Christopher Marlowe will receive their 3000000 lovelace back. This is expressed in the Marlowe language [here](../../src/Language/Marlowe/CLI/Examples/Trivial.hs).

We create the contract for the previously specified parameters.

```
marlowe-cli template simple --bystander "PK=$BYSTANDER_PUBKEYHASH"       \
                            --minimum-ada "$MINIMUM_ADA"                 \
                            --minimum-time "$MINIMUM_TIME"               \
                            --party "PK=$PARTY_PUBKEYHASH"               \
                            --deposit-lovelace "$DEPOSIT_LOVELACE"       \
                            --withdrawal-lovelace "$WITHDRAWAL_LOVELACE" \
                            --timeout "$TIMEOUT_TIME"                    \
                            --out-contract-file tx-1.contract            \
                            --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

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
Validator size: 13876
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 38794319, exBudgetMemory = ExMemory 130400}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wp6wcsaydajh99vkam49tgstxhjhxy0wvz8u2mr3rdrwugqnsnda9`.

The bystander Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                               \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                        --tx-in "$TX_0_BYSTANDER"                   \
                        --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe             \
                        --change-address "$BYSTANDER_ADDRESS"       \
                        --out-file tx-1.raw                         \
                        --print-stats                               \
                        --submit=600                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                    \
)
```

```console
Fee: Lovelace 190845
Size: 563 / 16384 = 3%
Execution units:
  Memory: 0 / 16000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the bystander Christopher Marlowe in the transaction `1e2be699849010511b20c8a3bfc68913f3e30faad3cc1371c1fcbf2ebddc10a0`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1e2be699849010511b20c8a3bfc68913f3e30faad3cc1371c1fcbf2ebddc10a0     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "60bf9f33fef1bf0611893c4c6513e9f18cfb6317909c532d01aa1a3632d939f3"
```

Here is the UTxO at the bystander Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1e2be699849010511b20c8a3bfc68913f3e30faad3cc1371c1fcbf2ebddc10a0     0        1937467294 lovelace + TxOutDatumNone
```

## Transaction 2. Make the Initial Deposit

First we compute the Marlowe input required to make the initial deposit by the party.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe              \
                        --deposit-account "PK=$PARTY_PUBKEYHASH" \
                        --deposit-party "PK=$PARTY_PUBKEYHASH"   \
                        --deposit-amount "$DEPOSIT_LOVELACE"     \
                        --invalid-before "$NOW"                  \
                        --invalid-hereafter "$((NOW+4*HOUR))"    \
                        --out-file tx-2.marlowe                  \
                        --print-stats
```

```console
Datum size: 272
```

Now the party Francis Beaumont submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-1.marlowe            \
                        --tx-in-marlowe "$TX_1"#1                 \
                        --tx-in-collateral "$TX_0_PARTY"          \
                        --tx-in "$TX_0_PARTY"                     \
                        --required-signer "$PARTY_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-2.marlowe           \
                        --change-address "$PARTY_ADDRESS"         \
                        --out-file tx-2.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1147667
Size: 14903 / 16384 = 90%
Execution units:
  Memory: 3872316 / 16000000 = 24%
  Steps: 1357796929 / 10000000000 = 13%
```

The contract received the deposit of 12000000 lovelace from the party Francis Beaumont in the transaction `afd7ee722a15061ea6a42eccbb8e98982f4b7501da2db2b489dcd0f522a99dbb`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
afd7ee722a15061ea6a42eccbb8e98982f4b7501da2db2b489dcd0f522a99dbb     1        15000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "84cc79968a27310d81a96ae59f327f05ee87fed3c32bc17409880b6f207c2478"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
afd7ee722a15061ea6a42eccbb8e98982f4b7501da2db2b489dcd0f522a99dbb     0        2949447233 lovelace + TxOutDatumNone
```

## Transaction 3. Make the First Withdrawal

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --notify                              \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats
```

```console
Datum size: 151
Payment 1
  Acccount: PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"
  Payee: Party (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
  Ada: 5.000000
```

Now the party Francis Beaumont can submit a transaction to withdraw funds:

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-2.marlowe            \
                        --tx-in-marlowe "$TX_2"#1                 \
                        --tx-in-collateral "$TX_2"#0              \
                        --tx-in "$TX_2"#0                         \
                        --required-signer "$PARTY_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-3.marlowe           \
                        --change-address "$PARTY_ADDRESS"         \
                        --out-file tx-3.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1286401
Size: 14679 / 16384 = 89%
Execution units:
  Memory: 5761584 / 16000000 = 36%
  Steps: 1906742859 / 10000000000 = 19%
```

The contract made a payment of 5000000 lovelace to the party Francis Beaumont in the transaction `f854f1ed478b67cdcc73719b331d3a96d7aa0a5e5f311c51ecc669b50f95513e`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f854f1ed478b67cdcc73719b331d3a96d7aa0a5e5f311c51ecc669b50f95513e     1        10000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "dffd93d26ad372e7f3bed10b6641182da9aba2688c6f48c74c0799402f50b036"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
f854f1ed478b67cdcc73719b331d3a96d7aa0a5e5f311c51ecc669b50f95513e     0        2948160832 lovelace + TxOutDatumNone
f854f1ed478b67cdcc73719b331d3a96d7aa0a5e5f311c51ecc669b50f95513e     2        5000000 lovelace + TxOutDatumNone
```

## Transaction 4. Close the contract

As in the third transaction, we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-3.marlowe           \
                        --notify                              \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-4.marlowe               \
                        --print-stats
```

```console
Datum size: 23
Payment 1
  Acccount: PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274"
  Payee: Party (PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274")
  Ada: 3.000000
Payment 2
  Acccount: PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a"
  Payee: Party (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")
  Ada: 7.000000
```

Now the party Francis Beaumont can submit a transaction to close the contract and disperse the remaining funds:

```
TX_4=$(
marlowe-cli run execute "${MAGIC[@]}"                             \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --marlowe-in-file tx-3.marlowe            \
                        --tx-in-marlowe "$TX_3"#1                 \
                        --tx-in-collateral "$TX_3"#0              \
                        --tx-in "$TX_3"#0                         \
                        --tx-in "$TX_3"#2                         \
                        --required-signer "$PARTY_PAYMENT_SKEY"   \
                        --marlowe-out-file tx-4.marlowe           \
                        --change-address "$PARTY_ADDRESS"         \
                        --out-file tx-4.raw                       \
                        --print-stats                             \
                        --submit=600                              \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
)
```

```console
Fee: Lovelace 1096089
Size: 14409 / 16384 = 87%
Execution units:
  Memory: 3486482 / 16000000 = 21%
  Steps: 1191033931 / 10000000000 = 11%
```

The closing of the contract paid 7000000 lovelace to the the party Francis Beaumont and 3000000 lovelace to the bystander Christopher Marlowe in the transaction `0d79b210389d6a9359d707efb1c9612a66c476ea2a18729545eb7f347517d597`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
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
0d79b210389d6a9359d707efb1c9612a66c476ea2a18729545eb7f347517d597     1        3000000 lovelace + TxOutDatumNone
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0d79b210389d6a9359d707efb1c9612a66c476ea2a18729545eb7f347517d597     0        2952064743 lovelace + TxOutDatumNone
0d79b210389d6a9359d707efb1c9612a66c476ea2a18729545eb7f347517d597     2        7000000 lovelace + TxOutDatumNone
```

