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
TxId "eeb2329a2dcdf6561c50041b8c0223a82948166c7b9a716a2f0b020d7ddf4c08"
```

The bystander Christopher Marlowe is the minimum-ADA provider and has the address `addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz`. They have the following UTxOs in their wallet:

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
b3c64934a1313068f2985be1dc48350157753a0c8a519935a0e354da4a0afa4a     0        49834279 lovelace + TxOutDatumNone
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

Christopher Marlowe will spend the UTxO `b3c64934a1313068f2985be1dc48350157753a0c8a519935a0e354da4a0afa4a#0`.

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
TxId "5094245c75c8c8c51a6dfa0ac51b7da06bb78d121df0e4331d1b2ebf07746848"
```

The party Francis Beaumont has the address `addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q`. They have the following UTxOs in their wallet:

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
7f7bea0f750f4643d12bbaf2dd0fcc1a6b8222e5a51208a3ee04cdb3e58c4cab     0        49834279 lovelace + TxOutDatumNone
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

Francis Beaumont will spend the UTxO `7f7bea0f750f4643d12bbaf2dd0fcc1a6b8222e5a51208a3ee04cdb3e58c4cab#0`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip --testnet-magic "$MAGIC" | jq '.slot')
NOW="$((TIP*SLOT_LENGTH+SLOT_OFFSET))"
HOUR="$((3600*1000))"
```

The tip is at slot 3420825. The current POSIX time implies that the tip of the blockchain should be slightly before slot 3420829. Tests may fail if this is not the case.

## The Contract

The contract has a minimum time and a timeout.

```
TIMEOUT_TIME="$((NOW+24*HOUR))"
```

The contract will automatically close at Sun, 18 Sep 2022 14:13:45 +0000.

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
marlowe-cli template simple --bystander "$BYSTANDER_ADDRESS"             \
                            --minimum-ada "$MINIMUM_ADA"                 \
                            --party "$PARTY_ADDRESS"                     \
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
Validator size: 12668
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 18653100, exBudgetMemory = ExMemory 81200}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.tx.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wrv0vwr4megau50ujjwsktvajmsu6dzza2rnalufd3husaqs9v6rv`.

The bystander Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                    \
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
Fee: Lovelace 187589
Size: 487 / 16384 = 2%
Execution units:
  Memory: 0 / 14000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the bystander Christopher Marlowe in the transaction `0d4f09dab1ba10603ed29d610df62be9ede8faec903d82061573fd0111f5df89`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0d4f09dab1ba10603ed29d610df62be9ede8faec903d82061573fd0111f5df89     1        3000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "ab1ec9ef55cdc6b6f6b6d93ad00ffa6a49ce79e048863ab5114d0812c3d8acde"
```

Here is the UTxO at the bystander Christopher Marlowe's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$BYSTANDER_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0d4f09dab1ba10603ed29d610df62be9ede8faec903d82061573fd0111f5df89     0        46646690 lovelace + TxOutDatumNone
```

## Transaction 2. Make the Initial Deposit

First we compute the Marlowe input required to make the initial deposit by the party.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe              \
                        --deposit-account "$PARTY_ADDRESS"       \
                        --deposit-party "$PARTY_ADDRESS"         \
                        --deposit-amount "$DEPOSIT_LOVELACE"     \
                        --invalid-before "$NOW"                  \
                        --invalid-hereafter "$((NOW+4*HOUR))"    \
                        --out-file tx-2.marlowe                  \
                        --print-stats
```

```console
Datum size: 217
```

Now the party Francis Beaumont submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 1112237
Size: 13766 / 16384 = 84%
Execution units:
  Memory: 4355198 / 14000000 = 31%
  Steps: 1172607711 / 10000000000 = 11%
```

The contract received the deposit of 12000000 lovelace from the party Francis Beaumont in the transaction `b7dbc8c29f4b001695f95f4bcca9e4bca02c5b87d98d8bc3e836795b30294c12`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b7dbc8c29f4b001695f95f4bcca9e4bca02c5b87d98d8bc3e836795b30294c12     1        15000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "0c946064b9d03d401a98e5a99353b86f4ce4e02879a8c2fe8a7792eb3633e91d"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b7dbc8c29f4b001695f95f4bcca9e4bca02c5b87d98d8bc3e836795b30294c12     0        35222042 lovelace + TxOutDatumNone
b7dbc8c29f4b001695f95f4bcca9e4bca02c5b87d98d8bc3e836795b30294c12     2        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "454342e4a699b6d6f2eebf48dda01743925a29fc7788de2c871f3fde3e224248"
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
Datum size: 186
Payment 1
  Acccount: Address ""addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q""
  Payee: Party (Address ""addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q"")
  Ada: Lovelace {getLovelace = 5000000}
```

Now the party Francis Beaumont can submit a transaction to withdraw funds:

```
TX_3=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 1324912
Size: 13718 / 16384 = 83%
Execution units:
  Memory: 7170282 / 14000000 = 51%
  Steps: 1898775640 / 10000000000 = 18%
```

The contract made a payment of 5000000 lovelace to the party Francis Beaumont in the transaction `30cd207753bd53820071fe892df42116ff707ff6ee855568888e18351d10db42`. Here is the UTxO at the contract address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
30cd207753bd53820071fe892df42116ff707ff6ee855568888e18351d10db42     1        10000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "b3722e1063c35452024e44d58351f5f4998753bb84f7b222bc8dd113833c4d40"
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
30cd207753bd53820071fe892df42116ff707ff6ee855568888e18351d10db42     0        32397130 lovelace + TxOutDatumNone
30cd207753bd53820071fe892df42116ff707ff6ee855568888e18351d10db42     2        5000000 lovelace + TxOutDatumNone
30cd207753bd53820071fe892df42116ff707ff6ee855568888e18351d10db42     3        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "fc89dcbf332c0701d1aef2ccbc332d6db1adbd2246ff37e201b5b2ea1fc363dc"
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
Datum size: 30
Payment 1
  Acccount: Address ""addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz""
  Payee: Party (Address ""addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"")
  Ada: Lovelace {getLovelace = 3000000}
Payment 2
  Acccount: Address ""addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q""
  Payee: Party (Address ""addr_test1vzzpzll6gsl9npf8wfhk2zg8sy2we50jcqc7w8w46gua2pqq7cw2q"")
  Ada: Lovelace {getLovelace = 7000000}
```

Now the party Francis Beaumont can submit a transaction to close the contract and disperse the remaining funds:

```
TX_4=$(
marlowe-cli run execute --testnet-magic "$MAGIC"                  \
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
Fee: Lovelace 1113329
Size: 13240 / 16384 = 80%
Execution units:
  Memory: 4652822 / 14000000 = 33%
  Steps: 1208929042 / 10000000000 = 12%
```

The closing of the contract paid 7000000 lovelace to the the party Francis Beaumont and 3000000 lovelace to the bystander Christopher Marlowe in the transaction `d426aee44fb4a8e8e8c5e521e00c2973a1be0d1f9ddc3f370686be2296609820`. There is no UTxO at the contract address:

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
0d4f09dab1ba10603ed29d610df62be9ede8faec903d82061573fd0111f5df89     0        46646690 lovelace + TxOutDatumNone
d426aee44fb4a8e8e8c5e521e00c2973a1be0d1f9ddc3f370686be2296609820     2        3000000 lovelace + TxOutDatumNone
```

Here is the UTxO at the party Francis Beaumont's address:

```
cardano-cli query utxo --testnet-magic "$MAGIC" --address "$PARTY_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
30cd207753bd53820071fe892df42116ff707ff6ee855568888e18351d10db42     3        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "fc89dcbf332c0701d1aef2ccbc332d6db1adbd2246ff37e201b5b2ea1fc363dc"
b7dbc8c29f4b001695f95f4bcca9e4bca02c5b87d98d8bc3e836795b30294c12     2        1500000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "454342e4a699b6d6f2eebf48dda01743925a29fc7788de2c871f3fde3e224248"
d426aee44fb4a8e8e8c5e521e00c2973a1be0d1f9ddc3f370686be2296609820     0        36283801 lovelace + TxOutDatumNone
d426aee44fb4a8e8e8c5e521e00c2973a1be0d1f9ddc3f370686be2296609820     1        7000000 lovelace + TxOutDatumNone
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
    --testnet-magic "$MAGIC" \
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
  cardano-cli query utxo --testnet-magic "$MAGIC" --address "$SRC_ADDR"
}
cleanup "$BYSTANDER_PAYMENT_SKEY" "$BYSTANDER_ADDRESS" "$FAUCET_ADDRESS"
```

```console
TxId "d8e85326333a3ba2a397f15c7ff431ef529c114b4e04788e6985fe7be3020137"
TxId "5909de95810432a2113aad5a6df6628ea666126923207ae3cfd16da7d1edef7e"
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

cleanup "$PARTY_PAYMENT_SKEY" "$PARTY_ADDRESS" "$FAUCET_ADDRESS"
```

```console
TxId "0d035ce723fe09d3da9a4b04a9bd18b544158f09bbf3fa6967fce3f2831019bf"
TxId "ce111d2f81f566dd93354d64bc6b678841959a24b3fb7ee06346f68b3c38dccf"
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
