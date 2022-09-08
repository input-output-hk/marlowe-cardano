# Test of a Simple Contract

[This simple contract](../../../marlowe-contracts/src/Marlowe/Contracts/Trivial.hs) takes as deposit, waits for a notification, makes a payment, waits for another notification, and then closes the contract:

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
See below for how to set `MAGIC` to select the network.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the bystander and party roles, or they will be created automatically: to do this, set the environment variables `BYSTANDER_PREFIX` and `PARTY_PREFIX` where they appear below.

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

### Participants

#### The Bystander

The bystander simply provides the minimum ada to be held in the contract while it is active.

```
BYSTANDER_PREFIX="$TREASURY/christopher-marlowe"
BYSTANDER_PREFIX="$TREASURY/christopher-marlowe"
BYSTANDER_NAME="Christopher Marlowe"
BYSTANDER_PAYMENT_SKEY="$BYSTANDER_PREFIX".skey
BYSTANDER_PAYMENT_VKEY="$BYSTANDER_PREFIX".vkey
if [[ ! -e "$BYSTANDER_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$BYSTANDER_PAYMENT_SKEY"      \
                              --verification-key-file "$BYSTANDER_PAYMENT_VKEY"
fi
BYSTANDER_ADDRESS=$(
  cardano-cli address build --testnet-magic "$MAGIC"                                  \
                            --payment-verification-key-file "$BYSTANDER_PAYMENT_VKEY" \
)
BYSTANDER_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$BYSTANDER_PAYMENT_VKEY"
)
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$BYSTANDER_ADDRESS"
```

```console
TxId "ebfede10128a867747fa97ad65db5676f15d1569dab9055430fdc0264a463e96"
```

The bystander Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vzt56ln99prhleyjj94gdlwszuwngdky2ppwsh708ncx0csf4zc8r` and public-key hash `974d7e6528477fe492916a86fdd0171d3436c45042e85fcf3cf067e2`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                    \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
                       --required-signer "$BYSTANDER_PAYMENT_SKEY" \
                       --change-address "$BYSTANDER_ADDRESS"       \
                       --out-file /dev/null                        \
                       --submit=600                                \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2f35943e4511eaf9ba6763c864217da2c6bb44e42152a86ba58f7c8f531220f5     0        49834279 lovelace + TxOutDatumNone
```

We select a UTxO with sufficient funds to use in executing the contract.

```
TX_0_BYSTANDER=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$BYSTANDER_ADDRESS"                      \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'
)
```

Christopher Marlowe will spend the UTxO `2f35943e4511eaf9ba6763c864217da2c6bb44e42152a86ba58f7c8f531220f5#0`.

#### The Party

The party deposits and removes funds from the contract.

```
PARTY_PREFIX="$TREASURY/francis-beaumont"
PARTY_NAME="Francis Beaumont"
PARTY_PAYMENT_SKEY="$PARTY_PREFIX".skey
PARTY_PAYMENT_VKEY="$PARTY_PREFIX".vkey
if [[ ! -e "$PARTY_PAYMENT_SKEY" ]]
then
  cardano-cli address key-gen --signing-key-file "$PARTY_PAYMENT_SKEY"      \
                              --verification-key-file "$PARTY_PAYMENT_VKEY"
fi
PARTY_ADDRESS=$(
  cardano-cli address build --testnet-magic "$MAGIC"                              \
                            --payment-verification-key-file "$PARTY_PAYMENT_VKEY" \
)
PARTY_PUBKEYHASH=$(
  cardano-cli address key-hash --payment-verification-key-file "$PARTY_PAYMENT_VKEY"
)
marlowe-cli util faucet --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --out-file /dev/null                      \
                        --submit 600                              \
                        --lovelace 50000000                       \
                        --faucet-address "$FAUCET_ADDRESS"        \
                        --required-signer "$FAUCET_SKEY_FILE"     \
                        "$PARTY_ADDRESS"
```

```console
TxId "05eb0e84a979b961670ec110b541c0442c3847ffd20fa14f781ae3dd603d8497"
```

The party Francis Beaumont has the address `addr_test1vplnyghksch70t8dqzm2zvugkz468gpn76g3ht6tn2jgcrgc5ydf4` and the public-key hash `7f3222f6862fe7aced00b6a13388b0aba3a033f6911baf4b9aa48c0d`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean --testnet-magic "$MAGIC"                  \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$PARTY_PAYMENT_SKEY"   \
                       --change-address "$PARTY_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
66445d0c7fc61071230bca48d8e612e6f1270c0e06f919ca60ba6cf50189de20     0        49834279 lovelace + TxOutDatumNone
```

We select the UTxO with the most funds to use in executing the contract.

```
TX_0_PARTY=$(
marlowe-cli util select --testnet-magic "$MAGIC"                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                        --lovelace-only 20000000                  \
                        "$PARTY_ADDRESS"                          \
| sed -n -e '1{s/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/;p}'
)
```

Francis Beaumont will spend the UTxO `66445d0c7fc61071230bca48d8e612e6f1270c0e06f919ca60ba6cf50189de20#0`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip --testnet-magic "$MAGIC" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
```

The tip is at slot 2663850. The current POSIX time implies that the tip of the blockchain should be slightly before slot 2663852. Tests may fail if this is not the case.

## The Contract

The contract has a minimum time and a timeout.

```
TIMEOUT_TIME="$((NOW+24*HOUR))"
```

The contract will automatically close at Fri, 09 Sep 2022 19:57:30 +0000.

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
                            --party "PK=$PARTY_PUBKEYHASH"               \
                            --deposit-lovelace "$DEPOSIT_LOVELACE"       \
                            --withdrawal-lovelace "$WITHDRAWAL_LOVELACE" \
                            --timeout "$TIMEOUT_TIME"                    \
                            --out-contract-file tx-1.contract            \
                            --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it will require.

```
marlowe-cli run initialize --testnet-magic "$MAGIC"                  \
                           --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                           --contract-file tx-1.contract             \
                           --state-file    tx-1.state                \
                           --out-file      tx-1.marlowe              \
                           --merkleize                               \
                           --print-stats
```

```console
Validator size: 12383
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 18768100, exBudgetMemory = ExMemory 81700}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.tx.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wz0znl0lfq7jqmgduzgsu7ndnjt4npgeg5a75j3upvn7dsq3dwfjv`.

The bystander Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
# TX_1=$(
# marlowe-cli run execute --testnet-magic "$MAGIC"                    \
#                         --socket-path "$CARDANO_NODE_SOCKET_PATH"   \
#                         --tx-in "$TX_0_BYSTANDER"                   \
#                         --required-signer "$BYSTANDER_PAYMENT_SKEY" \
#                         --marlowe-out-file tx-1.marlowe             \
#                         --change-address "$BYSTANDER_ADDRESS"       \
#                         --out-file tx-1.raw                         \
#                         --print-stats                               \
#                         --submit=600                                \
# | sed -e 's/^TxId "\(.*\)"$/\1/'                                    \
# )
TX_1=$(
marlowe-cli run auto-execute \
  --testnet-magic "$MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --required-signer "$BYSTANDER_PAYMENT_SKEY" \
  --marlowe-out-file tx-1.marlowe \
  --change-address "$BYSTANDER_ADDRESS" \
  --out-file tx-1.raw \
  --print-stats \
  --submit=600 \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                    \
)
```

```console
Fee: Lovelace 191637
Size: 478 / 16384 = 2%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the bystander Christopher Marlowe in the transaction `d7317cab508e22c859dce2d9fbbfeab599ee87d4db7b5609729123b4243a3b76`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d7317cab508e22c859dce2d9fbbfeab599ee87d4db7b5609729123b4243a3b76     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "3eb452718bc448cdaca5cf68fe92b34220b3bb3ad8cd65f8f3e4c2671121c2f2"
```

Here is the UTxO at the bystander Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d7317cab508e22c859dce2d9fbbfeab599ee87d4db7b5609729123b4243a3b76     0        46642642 lovelace + TxOutDatumNone
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
Datum size: 184
```

Now the party Francis Beaumont submits the transaction along with their deposit:

```
# TX_2=$(
# marlowe-cli run execute --testnet-magic "$MAGIC"                  \
#                         --socket-path "$CARDANO_NODE_SOCKET_PATH" \
#                         --marlowe-in-file tx-1.marlowe            \
#                         --tx-in-marlowe "$TX_1"#1                 \
#                         --tx-in-collateral "$TX_0_PARTY"          \
#                         --tx-in "$TX_0_PARTY"                     \
#                         --required-signer "$PARTY_PAYMENT_SKEY"   \
#                         --marlowe-out-file tx-2.marlowe           \
#                         --change-address "$PARTY_ADDRESS"         \
#                         --out-file tx-2.raw                       \
#                         --print-stats                             \
#                         --submit=600                              \
# | sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
# )
TX_2=$(
marlowe-cli run auto-execute \
  --testnet-magic "$MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --marlowe-in-file tx-1.marlowe \
  --tx-in-marlowe "$TX_1"#1 \
  --required-signer "$PARTY_PAYMENT_SKEY" \
  --marlowe-out-file tx-2.marlowe \
  --change-address "$PARTY_ADDRESS" \
  --out-file tx-2.raw \
  --print-stats \
  --submit=600 \
| sed -e 's/^TxId "\(.*\)"$/\1/' \
)
```

```console
Fee: Lovelace 1070512
Size: 13373 / 16384 = 81%
Execution units:
  Memory: 4056512 / 14000000 = 28%
  Steps: 1072753590 / 10000000000 = 10%
```

The contract received the deposit of 12000000 lovelace from the party Francis Beaumont in the transaction `d5a9fce3ec93c6483245a26d8da6d4357269905a4c26c870c21d4a12cfdbe4f4`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d5a9fce3ec93c6483245a26d8da6d4357269905a4c26c870c21d4a12cfdbe4f4     1        15000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "1002978802bbca009791514c2b852578d4c3d696928db46ca34704e903dc62d5"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
d5a9fce3ec93c6483245a26d8da6d4357269905a4c26c870c21d4a12cfdbe4f4     0        35263767 lovelace + TxOutDatumNone
d5a9fce3ec93c6483245a26d8da6d4357269905a4c26c870c21d4a12cfdbe4f4     2        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "e26d52c7c7d6d70f333d39391c68e94b5680e2fe54b55a358c67f7be7f8fa269"
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
Datum size: 153
Payment 1
  Acccount: PK "7f3222f6862fe7aced00b6a13388b0aba3a033f6911baf4b9aa48c0d"
  Payee: Party (PK "7f3222f6862fe7aced00b6a13388b0aba3a033f6911baf4b9aa48c0d")
  Ada: 5.000000
```

Now the party Francis Beaumont can submit a transaction to withdraw funds:

```
# TX_3=$(
# marlowe-cli run execute --testnet-magic "$MAGIC"                  \
#                         --socket-path "$CARDANO_NODE_SOCKET_PATH" \
#                         --marlowe-in-file tx-2.marlowe            \
#                         --tx-in-marlowe "$TX_2"#1                 \
#                         --tx-in-collateral "$TX_2"#0              \
#                         --tx-in "$TX_2"#0                         \
#                         --required-signer "$PARTY_PAYMENT_SKEY"   \
#                         --marlowe-out-file tx-3.marlowe           \
#                         --change-address "$PARTY_ADDRESS"         \
#                         --out-file tx-3.raw                       \
#                         --print-stats                             \
#                         --submit=600                              \
# | sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
# )
TX_3=$(
marlowe-cli run auto-execute \
  --testnet-magic "$MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --marlowe-in-file tx-2.marlowe \
  --tx-in-marlowe "$TX_2"#1 \
  --required-signer "$PARTY_PAYMENT_SKEY" \
  --marlowe-out-file tx-3.marlowe \
  --change-address "$PARTY_ADDRESS" \
  --out-file tx-3.raw \
  --print-stats \
  --submit=600 \
| sed -e 's/^TxId "\(.*\)"$/\1/' \
)
```

```console
Fee: Lovelace 1257175
Size: 13339 / 16384 = 81%
Execution units:
  Memory: 6525470 / 14000000 = 46%
  Steps: 1706598106 / 10000000000 = 17%
```

The contract made a payment of 5000000 lovelace to the party Francis Beaumont in the transaction `c640b9c558f4d33832ad663bd342732484dd257a8e73d9e750a717d712038504`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c640b9c558f4d33832ad663bd342732484dd257a8e73d9e750a717d712038504     1        10000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "f039de4b17aa5db716fd5b22c003a50509d08b5748b45b3d2a1d54915704ed95"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c640b9c558f4d33832ad663bd342732484dd257a8e73d9e750a717d712038504     0        32506592 lovelace + TxOutDatumNone
c640b9c558f4d33832ad663bd342732484dd257a8e73d9e750a717d712038504     2        5000000 lovelace + TxOutDatumNone
c640b9c558f4d33832ad663bd342732484dd257a8e73d9e750a717d712038504     3        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "92687a9fe252d1b0cbc990b6edcf5f6969ea70fe944f0059088f9d6cc5bdcc08"
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
Datum size: 25
Payment 1
  Acccount: PK "974d7e6528477fe492916a86fdd0171d3436c45042e85fcf3cf067e2"
  Payee: Party (PK "974d7e6528477fe492916a86fdd0171d3436c45042e85fcf3cf067e2")
  Ada: 3.000000
Payment 2
  Acccount: PK "7f3222f6862fe7aced00b6a13388b0aba3a033f6911baf4b9aa48c0d"
  Payee: Party (PK "7f3222f6862fe7aced00b6a13388b0aba3a033f6911baf4b9aa48c0d")
  Ada: 7.000000
```

Now the party Francis Beaumont can submit a transaction to close the contract and disperse the remaining funds:

```
# TX_4=$(
# marlowe-cli run execute --testnet-magic "$MAGIC"                  \
#                         --socket-path "$CARDANO_NODE_SOCKET_PATH" \
#                         --marlowe-in-file tx-3.marlowe            \
#                         --tx-in-marlowe "$TX_3"#1                 \
#                         --tx-in-collateral "$TX_3"#0              \
#                         --tx-in "$TX_3"#0                         \
#                         --tx-in "$TX_3"#2                         \
#                         --required-signer "$PARTY_PAYMENT_SKEY"   \
#                         --marlowe-out-file tx-4.marlowe           \
#                         --change-address "$PARTY_ADDRESS"         \
#                         --out-file tx-4.raw                       \
#                         --print-stats                             \
#                         --submit=600                              \
# | sed -e 's/^TxId "\(.*\)"$/\1/'                                  \
# )
TX_4=$(
marlowe-cli run auto-execute \
  --testnet-magic "$MAGIC" \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --marlowe-in-file tx-3.marlowe \
  --tx-in-marlowe "$TX_3"#1 \
  --required-signer "$PARTY_PAYMENT_SKEY" \
  --marlowe-out-file tx-4.marlowe \
  --change-address "$PARTY_ADDRESS" \
  --out-file tx-4.raw \
  --print-stats \
  --submit=600 \
| sed -e 's/^TxId "\(.*\)"$/\1/' \
)
```

```console
Fee: Lovelace 974028
Size: 12886 / 16384 = 78%
Execution units:
  Memory: 3092916 / 14000000 = 22%
  Steps: 802896537 / 10000000000 = 8%
```

The closing of the contract paid 7000000 lovelace to the the party Francis Beaumont and 3000000 lovelace to the bystander Christopher Marlowe in the transaction `57425db8a7b5e731594fb201c6baf735ae12c94552ff047956c3eee95a758e3b`. There is no UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here is the UTxO at the bystander Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
57425db8a7b5e731594fb201c6baf735ae12c94552ff047956c3eee95a758e3b     2        3000000 lovelace + TxOutDatumNone
d7317cab508e22c859dce2d9fbbfeab599ee87d4db7b5609729123b4243a3b76     0        46642642 lovelace + TxOutDatumNone
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
57425db8a7b5e731594fb201c6baf735ae12c94552ff047956c3eee95a758e3b     0        31532564 lovelace + TxOutDatumNone
57425db8a7b5e731594fb201c6baf735ae12c94552ff047956c3eee95a758e3b     1        7000000 lovelace + TxOutDatumNone
c640b9c558f4d33832ad663bd342732484dd257a8e73d9e750a717d712038504     2        5000000 lovelace + TxOutDatumNone
c640b9c558f4d33832ad663bd342732484dd257a8e73d9e750a717d712038504     3        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "92687a9fe252d1b0cbc990b6edcf5f6969ea70fe944f0059088f9d6cc5bdcc08"
d5a9fce3ec93c6483245a26d8da6d4357269905a4c26c870c21d4a12cfdbe4f4     2        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "e26d52c7c7d6d70f333d39391c68e94b5680e2fe54b55a358c67f7be7f8fa269"
```

## Clean Up

```
cleanup() {
  SRC_SKEY="$1"
  SRC_ADDR="$2"
  DEST_ADDR="$3"
  # Combine all ADA together in the source address
  marlowe-cli util clean \
    --testnet-magic "$MAGIC" \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --required-signer "$SRC_SKEY" \
    --change-address "$SRC_ADDR" \
    --out-file /dev/null \
    --submit 600
  # Get the TxHash#TxIx of these combined funds in a variable we can use as a
  # --tx-in argument
  TX_CLEANUP=$(
  marlowe-cli util select \
    --all "$SRC_ADDR" \
  | sed -e 's/^TxIn "\(.*\)" (TxIx \(.*\))$/\1#\2/' \
  )
  # Send the funds back to the dest address
  marlowe-cli transaction simple \
    --testnet-magic "$MAGIC" \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --tx-in "$TX_CLEANUP" \
    --required-signer "$SRC_SKEY" \
    --change-address "$DEST_ADDR" \
    --out-file /dev/null \
    --submit 600
}
cleanup "$BYSTANDER_PAYMENT_SKEY" "$BYSTANDER_ADDRESS" "$FAUCET_ADDRESS"
```

```console
TxId "e0c42fce3940a1ea5355891e09e4c0a3b440daf2b6a68dff32209b98a1c4016a"
TxId "eeecf7c8d227aa28e14bf801a20a81eb6ec50dba4a8f10fcd35fc20679787eb8"
```

```
cleanup "$PARTY_PAYMENT_SKEY" "$PARTY_ADDRESS" "$FAUCET_ADDRESS"
```

```console
TxId "6e221335c9e8cb0228675295aaa9759405fef882ecae8c72d2d5b26529cb7861"
TxId "032d28f7f02a5bb7f307a53aeecb59a1dd76f4072bbb22451b4e24751a4f71be"
