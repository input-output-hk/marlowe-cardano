# Example Escrow Contract: "Everything is alright"

In this example execution of [an escrow contract](ReadMe.md), the buyer does not report a problem.

![Flow chart for "everything is alright".](everything-is-alright.svg)

## Prerequisites

The environment variable `CARDANO_NODE_SOCKET_PATH` must be set to the path to the cardano node's socket.

The following tools must be on the PATH:
* [marlowe-cli](../../ReadMe.md)
* [cardano-cli](https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/README.md)
* [jq](https://stedolan.github.io/jq/manual/)
* sed

Signing and verification keys must be provided below for the bystander and party roles: to do this, set the environment variables `SELLER_PREFIX`, `BUYER_PREFIX`, and `MEDIATOR_PREFIX` where they appear below.

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
  SLOT_OFFSET=1638215277000
fi
```

### Role Currency

Set the role currency for the validator.

```
ROLE_CURRENCY=8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d
```

### Select Parties

#### The Seller

The seller sells an item for a price.

```
SELLER_PREFIX="$TREASURY/francis-beaumont"
SELLER_NAME="Francis Beaumont"
SELLER_ROLE=FB
SELLER_TOKEN="$ROLE_CURRENCY.$SELLER_ROLE"
SELLER_PAYMENT_SKEY="$SELLER_PREFIX".skey
SELLER_PAYMENT_VKEY="$SELLER_PREFIX".vkey
SELLER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                          \
                            --payment-verification-key-file "$SELLER_PAYMENT_VKEY" \
)
```

The seller Francis Beaumont has the address `addr_test1vrtntkszteptml4e9ce9l3fsmgavwv4ywunvdnhxv6nw5ksq6737a` and role token named `FB`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$SELLER_PAYMENT_SKEY"  \
                       --change-address "$SELLER_ADDRESS"        \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
7e67af3d3c964e40cd9b9889b83a31cc1ca295128022af83599b1ece762df751     0        2967876497 lovelace + TxOutDatumNone
7e67af3d3c964e40cd9b9889b83a31cc1ca295128022af83599b1ece762df751     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
7e67af3d3c964e40cd9b9889b83a31cc1ca295128022af83599b1ece762df751     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FrancisBeaumont + TxOutDatumNone
```

We select the UTxO with the seller Francis Beaumont's role token.

```
TX_0_SELLER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$SELLER_ADDRESS"                                                              \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_SELLER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                      \
                       --address "$SELLER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                             \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$SELLER_ROLE"'" == 1) | .key' \
)
```

Francis Beaumont will spend the UTxOs `7e67af3d3c964e40cd9b9889b83a31cc1ca295128022af83599b1ece762df751#0` and `7e67af3d3c964e40cd9b9889b83a31cc1ca295128022af83599b1ece762df751#1`.

### The Buyer

```
BUYER_PREFIX="$TREASURY/thomas-middleton"
BUYER_NAME="Thomas Middleton"
BUYER_ROLE=TM
BUYER_TOKEN="$ROLE_CURRENCY.$BUYER_ROLE"
BUYER_PAYMENT_SKEY="$BUYER_PREFIX".skey
BUYER_PAYMENT_VKEY="$BUYER_PREFIX".vkey
BUYER_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                         \
                            --payment-verification-key-file "$BUYER_PAYMENT_VKEY" \
)
```

The buyer Thomas Middleton has the address `addr_test1vzgrqnlp6elmettvuelx5vkn0uxhtu2ewqdhx297ukgjmjgpss5k0` and role token named `TM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                             \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                       --required-signer "$BUYER_PAYMENT_SKEY"   \
                       --change-address "$BUYER_ADDRESS"         \
                       --out-file /dev/null                      \
                       --submit=600                              \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2d62ee884f224459d84e612667b9030871884c8b377be072f02c840004d4de0b     0        2897390534 lovelace + TxOutDatumNone
2d62ee884f224459d84e612667b9030871884c8b377be072f02c840004d4de0b     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
2d62ee884f224459d84e612667b9030871884c8b377be072f02c840004d4de0b     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ThomasMiddleton + TxOutDatumNone
```

We select the UTxO with the buyer Thomas Middleton's role token.

```
TX_0_BUYER_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$BUYER_ADDRESS"                                                               \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_BUYER_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                     \
                       --address "$BUYER_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                            \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$BUYER_ROLE"'" == 1) | .key' \
)
```

Thomas Middleton will spend the UTxOs `2d62ee884f224459d84e612667b9030871884c8b377be072f02c840004d4de0b#0` and `2d62ee884f224459d84e612667b9030871884c8b377be072f02c840004d4de0b#1`.

### The Mediator

```
MEDIATOR_PREFIX="$TREASURY/christopher-marlowe"
MEDIATOR_NAME="Christopher Marlowe"
MEDIATOR_ROLE=CM
MEDIATOR_TOKEN="$ROLE_CURRENCY.$MEDIATOR_ROLE"
MEDIATOR_PAYMENT_SKEY="$MEDIATOR_PREFIX".skey
MEDIATOR_PAYMENT_VKEY="$MEDIATOR_PREFIX".vkey
MEDIATOR_ADDRESS=$(
  cardano-cli address build "${MAGIC[@]}"                                            \
                            --payment-verification-key-file "$MEDIATOR_PAYMENT_VKEY" \
)
```

The mediator Christopher Marlowe has the address `addr_test1vqhqudxtwqcpjqesns79hqgqq2q0xx5q0hnzz5es9492yaqpxltpy` and role token named `CM`. They have the following UTxOs in their wallet:

```
marlowe-cli util clean "${MAGIC[@]}"                              \
                       --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                       --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                       --change-address "$MEDIATOR_ADDRESS"       \
                       --out-file /dev/null                       \
                       --submit=600                               \
> /dev/null
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
e31b1e4026f843a9ef069174fffc8a4ccd3839f970da374dc64b5aaebbac49e5     0        1949186071 lovelace + TxOutDatumNone
e31b1e4026f843a9ef069174fffc8a4ccd3839f970da374dc64b5aaebbac49e5     1        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
e31b1e4026f843a9ef069174fffc8a4ccd3839f970da374dc64b5aaebbac49e5     2        2000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.ChristopherMarlowe + TxOutDatumNone
```

We select the UTxO with the mediator Christopher Marlowe's role token.

```
TX_0_MEDIATOR_ADA=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                            \
                       --address "$MEDIATOR_ADDRESS"                                                            \
                       --out-file /dev/stdout                                                                   \
| jq -r '. | to_entries | sort_by(- .value.value.lovelace) | .[] | select((.value.value | length) == 1) | .key' \
| head -n 1
)
TX_0_MEDIATOR_TOKEN=$(
cardano-cli query utxo "${MAGIC[@]}"                                                                        \
                       --address "$MEDIATOR_ADDRESS"                                                        \
                       --out-file /dev/stdout                                                               \
| jq -r '. | to_entries | .[] | select(.value.value."'"$ROLE_CURRENCY"'"."'"$MEDIATOR_ROLE"'" == 1) | .key' \
)
```

Christopher Marlowe will spend the UTxOs `e31b1e4026f843a9ef069174fffc8a4ccd3839f970da374dc64b5aaebbac49e5#0` and `e31b1e4026f843a9ef069174fffc8a4ccd3839f970da374dc64b5aaebbac49e5#1`.

### Tip of the Blockchain

```
TIP=$(cardano-cli query tip "${MAGIC[@]}" | jq '.slot')
```

The tip is at slot 50684224. The current POSIX time implies that the tip of the blockchain should be slightly before slot 50684226. Tests may fail if this is not the case.

## The Contract

The contract has a minimum slot and several deadlines.

```
PAYMENT_DEADLINE=$((TIP+1*24*3600))
COMPLAINT_DEADLINE=$((TIP+2*24*3600))
DISPUTE_DEADLINE=$((TIP+3*24*3600))
MEDIATION_DEADLINE=$((TIP+4*24*3600))
```

* The current slot is 50684224.
* The buyer Thomas Middleton must pay before slot 50770624.
* They buyer Thomas Middleton has until slot 50857024 to complain.
* The seller Francis Beaumont has until slot 50943424 to dispute a complaint.
* The mediator Christopher Marlowe has until slot 51029824 to decide on a disputed complaint.

The contract also involves the price of the good exchanged and a minimum-ADA value.

```
MINIMUM_ADA=3000000
PRICE=256000000
```

The selling price is 256000000 lovelace.

We create the contract for the previously specified parameters.

```
marlowe-cli template escrow --minimum-ada "$MINIMUM_ADA"               \
                            --price "$PRICE"                           \
                            --seller "Role=$SELLER_ROLE"               \
                            --buyer "Role=$BUYER_ROLE"                 \
                            --mediator "Role=$MEDIATOR_ROLE"           \
                            --payment-deadline "$PAYMENT_DEADLINE"     \
                            --complaint-deadline "$COMPLAINT_DEADLINE" \
                            --dispute-deadline "$DISPUTE_DEADLINE"     \
                            --mediation-deadline "$MEDIATION_DEADLINE" \
                            --out-contract-file tx-1.contract          \
                            --out-state-file    tx-1.state
```

## Transaction 1. Create the Contract by Providing the Minimum ADA.

First we create a `.marlowe` file that contains the initial information needed to run the contract. The bare size and cost of the script provide a lower bound on the resources that running it wiil require.

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
Validator size: 14339
Base-validator cost: ExBudget {exBudgetCPU = ExCPU 40342515, exBudgetMemory = ExMemory 135600}
```

In particular, we can extract the contract's address from the `.marlowe` file.

```
CONTRACT_ADDRESS=$(jq -r '.marloweValidator.address' tx-1.marlowe)
```

The Marlowe contract resides at address `addr_test1wqkj9530xdhg20vjc9jgrtv9atq58ylj4vv96mush546sqg4js3dg`.

Because this is a role-based contract, we compute the address of the script for roles.

```
ROLE_ADDRESS=$(jq -r '.rolesValidator.address' tx-1.marlowe)
```

The role address is `addr_test1wpdsreg339dsku87hskeenlwcaalhdu32dd85rrrjwr002g3c0yy3`.

The mediator Christopher Marlowe submits the transaction along with the minimum ADA 3000000 lovelace required for the contract's initial state. Submitting with the `--print-stats` switch reveals the network fee for the contract, the size of the transaction, and the execution requirements, relative to the protocol limits.

```
TX_1=$(
marlowe-cli run execute "${MAGIC[@]}"                              \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --tx-in "$TX_0_MEDIATOR_ADA"               \
                        --change-address "$MEDIATOR_ADDRESS"       \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe            \
                        --out-file tx-1.raw                        \
                        --print-stats                              \
                        --submit=600                               \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                   \
)
```

```console
Fee: Lovelace 197005
Size: 703 / 16384 = 4%
Execution units:
  Memory: 0 / 16000000 = 0%
  Steps: 0 / 10000000000 = 0%
```

The contract received the minimum ADA of 3000000 lovelace from the mediator Christopher Marlowe in the transaction `c9e5b4c20606990283e13bc93b20293a583f2cf14292e190969e091e759403e2`.  Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c9e5b4c20606990283e13bc93b20293a583f2cf14292e190969e091e759403e2     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "cd9d92227cb7be3fdac806535ce96c83d28a876ece22053dc71d06ed13b7a981"
```

Here is the UTxO at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c9e5b4c20606990283e13bc93b20293a583f2cf14292e190969e091e759403e2     0        1945989066 lovelace + TxOutDatumNone
```

## Transaction 2. Buyer Deposits Funds into Seller's Account.

First we compute the Marlowe input required to make the initial deposit by the buyer.

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$SELLER_ROLE" \
                        --deposit-party "Role=$BUYER_ROLE"    \
                        --deposit-amount "$PRICE"             \
                        --invalid-before "$TIP"               \
                        --invalid-hereafter "$((TIP+4*3600))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

```console
Datum size: 447
```

Now the buyer Thomas Middleton submits the transaction along with their deposit:

```
TX_2=$(
marlowe-cli run execute "${MAGIC[@]}"                                         \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-1.marlowe                        \
                        --tx-in-marlowe "$TX_1"#1                             \
                        --tx-in-collateral "$TX_0_BUYER_ADA"                  \
                        --tx-in "$TX_0_BUYER_ADA"                             \
                        --tx-in "$TX_0_BUYER_TOKEN"                           \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-2.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-2.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                              \
)
```

```console
Fee: Lovelace 1357865
Size: 15737 / 16384 = 96%
Execution units:
  Memory: 5879542 / 16000000 = 36%
  Steps: 2096223427 / 10000000000 = 20%
```

The contract received the deposit of 256000000 lovelace from Thomas Middleton in the transaction `60c59b40eb6b44b9cfe389938540b5bd5b10514a309bd2e8af6aeb3fefb82fd3`. Here is the UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
60c59b40eb6b44b9cfe389938540b5bd5b10514a309bd2e8af6aeb3fefb82fd3     1        259000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "0f1518af756229ecd92b610114ac493165896558fde6092b58a3aa5dd07e084d"
```

Here is the UTxO at Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_2/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
60c59b40eb6b44b9cfe389938540b5bd5b10514a309bd2e8af6aeb3fefb82fd3     0        2639032669 lovelace + TxOutDatumNone
60c59b40eb6b44b9cfe389938540b5bd5b10514a309bd2e8af6aeb3fefb82fd3     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
```

## Transaction 3. The Buyer Reports that Everything is Alright

Funds are released to the seller and mediator, closing the contract.

First we compute the input for the contract to transition forward.

```
marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --choice-name "Everything is alright" \
                        --choice-party "Role=$BUYER_ROLE"     \
                        --choice-number 0                     \
                        --invalid-before "$TIP"               \
                        --invalid-hereafter "$((TIP+4*3600))" \
                        --out-file tx-3.marlowe               \
                        --print-stats
```

```console
Datum size: 53
Payment 1
  Acccount: "CM"
  Payee: Party "CM"
  Ada: 3.000000
Payment 2
  Acccount: "FB"
  Payee: Party "FB"
  Ada: 256.000000
```

Now the buyer Thomas Middleton can submit a transaction to release funds:

```
TX_3=$(
marlowe-cli run execute "${MAGIC[@]}"                                         \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-2.marlowe                        \
                        --tx-in-marlowe "$TX_2"#1                             \
                        --tx-in-collateral "$TX_2"#0                          \
                        --tx-in "$TX_2"#0                                     \
                        --tx-in "$TX_2"#2                                     \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-3.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-3.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
| sed -e 's/^TxId "\(.*\)"$/\1/'
)
```

```console
Fee: Lovelace 1310877
Size: 15351 / 16384 = 93%
Execution units:
  Memory: 5557256 / 16000000 = 34%
  Steps: 1937994004 / 10000000000 = 19%
```

The closing of the contract paid 256000000 lovelace to the role address for the benefit of the seller Francis Beaumont and 3000000 lovelace for the benefit of the mediator Christopher Marlowe in the transaction `4413d53b7e14589bfe9e39eb20658206f2bcb973dc1391b7a2f6c5ec21c8db6c`. There is no UTxO at the contract address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$CONTRACT_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
4413d53b7e14589bfe9e39eb20658206f2bcb973dc1391b7a2f6c5ec21c8db6c     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "1d8ad8b0a642297fa38291e4522015f82d47774e9a9829345a0952c654f427a3"
4413d53b7e14589bfe9e39eb20658206f2bcb973dc1391b7a2f6c5ec21c8db6c     2        256000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f605a13bb90b0f530748173887182b9135318402c3d5c989e40cca12b283664b"
```

Here is the UTxO at the buyer Thomas Middleton's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$BUYER_ADDRESS" | sed -n -e "1p;2p;/$TX_3/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
4413d53b7e14589bfe9e39eb20658206f2bcb973dc1391b7a2f6c5ec21c8db6c     0        2637721792 lovelace + TxOutDatumNone
4413d53b7e14589bfe9e39eb20658206f2bcb973dc1391b7a2f6c5ec21c8db6c     3        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.TM + TxOutDatumNone
```

## Transactions 4 and 5. Seller and Mediator Withdraw Funds.

The seller Francis Beaumont submits a transaction to withdraw the payment from the role address.

```
TX_4=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                           \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"               \
                         --marlowe-file tx-3.marlowe                             \
                         --role-name "$SELLER_ROLE"                              \
                         --tx-in "$TX_0_SELLER_ADA"                              \
                         --tx-in "$TX_0_SELLER_TOKEN"                            \
                         --tx-in-collateral "$TX_0_SELLER_ADA"                   \
                         --required-signer "$SELLER_PAYMENT_SKEY"                \
                         --tx-out "$SELLER_ADDRESS+$MINIMUM_ADA+1 $SELLER_TOKEN" \
                         --change-address "$SELLER_ADDRESS"                      \
                         --out-file tx-4.raw                                     \
                         --print-stats                                           \
                         --submit=600                                            \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                 \
)
```

```console
Fee: Lovelace 451385
Size: 3187 / 16384 = 19%
Execution units:
  Memory: 1607510 / 16000000 = 10%
  Steps: 601307457 / 10000000000 = 6%
```

The mediator Christopher Marlowe submits a transaction to withdraw the minimum ADA from the role address.

```
TX_5=$(
marlowe-cli run withdraw "${MAGIC[@]}"                                               \
                         --socket-path "$CARDANO_NODE_SOCKET_PATH"                   \
                         --marlowe-file tx-3.marlowe                                 \
                         --role-name "$MEDIATOR_ROLE"                                \
                         --tx-in "$TX_1"#0                                           \
                         --tx-in "$TX_0_MEDIATOR_TOKEN"                              \
                         --tx-in-collateral "$TX_1"#0                                \
                         --required-signer "$MEDIATOR_PAYMENT_SKEY"                  \
                         --tx-out "$MEDIATOR_ADDRESS+$MINIMUM_ADA+1 $MEDIATOR_TOKEN" \
                         --change-address "$MEDIATOR_ADDRESS"                        \
                         --out-file tx-5.raw                                         \
                         --print-stats                                               \
                         --submit=600                                                \
| sed -e 's/^TxId "\(.*\)"$/\1/'                                                     \
)
```

```console
Fee: Lovelace 451385
Size: 3187 / 16384 = 19%
Execution units:
  Memory: 1607510 / 16000000 = 10%
  Steps: 601307457 / 10000000000 = 6%
```

There is no UTxO at the role address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$ROLE_ADDRESS" | sed -n -e "1p;2p;/$TX_1/p;/$TX_2/p;/$TX_3/p;/$TX_4/p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Here are the UTxOs at the seller Francis Beaumont's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$SELLER_ADDRESS" | sed -n -e "1p;2p;/$TX_4/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
4af2f976301276d8c34ebd45d189947b669a4bb7a2bbc734e2f39c38adb834f0     0        2966425112 lovelace + TxOutDatumNone
4af2f976301276d8c34ebd45d189947b669a4bb7a2bbc734e2f39c38adb834f0     1        256000000 lovelace + TxOutDatumNone
4af2f976301276d8c34ebd45d189947b669a4bb7a2bbc734e2f39c38adb834f0     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.FB + TxOutDatumNone
```

Here are the UTxOs at the mediator Christopher Marlowe's address:

```
cardano-cli query utxo "${MAGIC[@]}" --address "$MEDIATOR_ADDRESS" | sed -n -e "1p;2p;/$TX_5/p"
```

```console
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ef8c59a1580999fcc0fda7818167676377fdc4cb32de5841936f077a456896d0     0        1944537681 lovelace + TxOutDatumNone
ef8c59a1580999fcc0fda7818167676377fdc4cb32de5841936f077a456896d0     1        3000000 lovelace + TxOutDatumNone
ef8c59a1580999fcc0fda7818167676377fdc4cb32de5841936f077a456896d0     2        3000000 lovelace + 1 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d.CM + TxOutDatumNone
```

## Clean Up
Send the funds back to the buyer, so that the test can be run again.

```
marlowe-cli transaction simple "${MAGIC[@]}"                             \
                               --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                               --tx-in "$TX_4"#1                         \
                               --required-signer "$SELLER_PAYMENT_SKEY"  \
                               --change-address "$BUYER_ADDRESS"         \
                               --out-file tx-6.raw                       \
                               --submit=600                              \
> /dev/null
