# Marlowe CLI Tutorial: Extended Example

This example for using `marlowe-cli` runs a Marlowe three-step contract on `testnet`.

1.  The contract is initially funded with 3 ADA.
2.  The sole party then deposits 10 ADA.
3.  The contract then permits 5 ADA to be payed back to the party.
4.  Finally, the contract closes and the remaining 8 ADA is withdrawn.

Here is the contract in Marlowe format;

<table>
<tr>
<td>
<ol>
<li value="0">The contract is initially funded with 3 ADA.
<li value="1">The sole party then deposits 10 ADA.
<li value="2">The contract then permits 5 ADA to be payed back to the party.
<li value="3">Finally, the contract closes and the remaining 8 ADA is withdrawn.
</ol>
<td>
<td rowspan="2">
<img alt="A Three-Step Marlowe Contract" src="extended-example.png"/>
</td>
</tr>
<tr>
<td>
<pre>
When
    [Case
        (Deposit
            (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07")
            (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07")
            (Token "" "")
            (Constant 10)
        )
        (Pay
            (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07")
            (Account (PK "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07"))
            (Token "" "")
            (Constant 5)
            Close
        )]
    90000000 Close
</pre>
</td>
</tr>
</table>


## 1. Select the network.

Make sure that `marlowe-cli`, `cardano-cli`, and `jq` have been installed on the path for the `bash` shell. Set the environment variable `CARDANO_NODE_SOCKET_PATH` to point to the location of the socket for the `cardano-node` service: see <https://developers.cardano.org/docs/get-started/running-cardano/#querying-the-cardano-blockchain>. In this tutorial, we use the public `testnet`:

    NETWORK=testnet
    MAGIC=(--testnet-magic 1097911063)


## 2. Select the wallet.

Select a wallet for use in this tutorial and specify the files with the signing and payment keys. The address of this wallet is stored in the environment variable `ADDRESS_P`.

    PAYMENT_SKEY=path/to/payment.skey
    PAYMENT_VKEY=path/to/payment.vkey
    ADDRESS_P=$(cardano-cli address build "${MAGIC[@]}" --payment-verification-key-file $PAYMENT_VKEY)
    PUBKEYHASH_P=$(cardano-cli address key-hash --payment-verification-key-file $PAYMENT_VKEY)


## 3. Find the contract address.

Next we compute the contract address.

    ADDRESS_S=$(marlowe-cli export-address "${MAGIC[@]}")
    echo "$ADDRESS_S"


## 4. Create the Plutus script for the validator.

Now we create the Plutus script for the contract.

    $ marlowe-cli export-validator "${MAGIC[@]}"             \
                                   --out-file example.plutus \
                                   --print-stats
    
    Validator size: 13756
    Validator cost: ExBudget {exBudgetCPU = ExCPU 36829301, exBudgetMemory = ExMemory 123800}


## 5. Generate the example contract, state, and inputs files for each step.

Because the contract has three steps, we generate three sets of contracts, states, and inputs. Then we create the datums and validators for each step.

    marlowe-cli example "$PUBKEYHASH_P" --write-files > /dev/null
    
    for i in 0 1 2
    do
      marlowe-cli export-datum --contract-file example-$i.contract \
                               --state-file    example-$i.state    \
                               --out-file      example-$i.datum
    done
    
    for i in 0 1
    do
      marlowe-cli export-redeemer --out-file example-$i.redeemer
    done
    marlowe-cli export-redeemer --input-file example-2.input    \
                                --out-file   example-2.redeemer


## 6. Find some funds, and enter the selected UTxO as "TX_0".

Before running the contract, we need to put funds into it. Examine the UTxOs at the wallet address:

    $ cardano-cli query utxo "${MAGIC[@]}" --address $ADDRESS_P
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    449b359d5b53c9c0b8ccd08c1d81e63c29c88ca5cc34fff196af33c1c6ad66d4     0        901025480 lovelace + TxOutDatumNone


Select one of these UTxOs for use in funding the contract, naming it `TX_0`, and then build and submit the funding transaction:

    TX_0=449b359d5b53c9c0b8ccd08c1d81e63c29c88ca5cc34fff196af33c1c6ad66d4


## 7. Fund the contract by sending the initial funds and setting the initial state.

    $ TX_1=$(
      marlowe-cli transaction-create "${MAGIC[@]}"                             \
                                     --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                     --script-address "$ADDRESS_S"             \
                                     --tx-out-datum-file example-2.datum       \
                                     --tx-out-value 3000000                    \
                                     --tx-in "$TX_0"                           \
                                     --change-address "$ADDRESS_P"             \
                                     --out-file tx.raw                         \
                                     --required-signer $PAYMENT_SKEY           \
                                     --print-stats                             \
                                     --submit=600                              \
      | sed -e 's/^TxId "\(.*\)"$/\1/'
      )
    
    Fee: Lovelace 168845
    Size: 166 / 16384 = 1%
    Execution units:
      Memory: 0 / 12500000 = 0%
      Steps: 0 / 10000000000 = 0%
    
    $ echo TxId "$TX_1"
    
    TxId "2c4fe04f9ef00976d6a4c12c5b0a069e8e66a7072a975b0a4b10fadb7c38a0da"


## 8. Wait until the transaction is appears on the blockchain.

    $ cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    2c4fe04f9ef00976d6a4c12c5b0a069e8e66a7072a975b0a4b10fadb7c38a0da     1        3000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "6d27ab1a0d5778f40d3cc0e5ec0526b336079eeeb695d27795f0b2cec649283d"


## 9. Deposit 10 ADA.

The first step of the contract involves depositing 10 ADA.

    $ TX_2=$(
      marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                      --script-address "$ADDRESS_S"             \
                                      --tx-in-script-file example.plutus        \
                                      --tx-in-redeemer-file example-2.redeemer  \
                                      --tx-in-datum-file example-2.datum        \
                                      --required-signer $PAYMENT_SKEY           \
                                      --tx-in-marlowe "$TX_1"#1                 \
                                      --tx-in "$TX_1"#0                         \
                                      --tx-in-collateral "$TX_1"#0              \
                                      --tx-out-datum-file example-1.datum       \
                                      --tx-out-value 13000000                   \
                                      --tx-out "$ADDRESS_P"+50000000            \
                                      --change-address "$ADDRESS_P"             \
                                      --invalid-before    40000000              \
                                      --invalid-hereafter 80000000              \
                                      --out-file tx.raw                         \
                                      --print-stats                             \
                                      --submit=600                              \
      | sed -e 's/^TxId "\(.*\)"$/\1/'
      )
    
    Fee: Lovelace 1128716
    Size: 14476 / 16384 = 88%
    Execution units:
      Memory: 3897526 / 12500000 = 31%
      Steps: 1335361970 / 10000000000 = 13%
    
    $ echo TxId "$TX_2"
    
    TxId "008cb56948dc22d9cf7b8eda58d2a1c5d6356576a0dd34c43ec4ee1cd60f1dfb"


## 10. Wait until the transaction is appears on the blockchain.

    $ cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    008cb56948dc22d9cf7b8eda58d2a1c5d6356576a0dd34c43ec4ee1cd60f1dfb     1        13000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "f9de2a73ce274c8fd9477ca509fdd5f7b29d953591a1e257c853165f75996ac4"


## 11. Pay 5 ADA back.

Now the contract allows 5 ADA to be paid from the contract.

    $ TX_3=$(
      marlowe-cli transaction-advance "${MAGIC[@]}"                             \
                                      --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                      --script-address "$ADDRESS_S"             \
                                      --tx-in-script-file example.plutus        \
                                      --tx-in-redeemer-file example-1.redeemer  \
                                      --tx-in-datum-file example-1.datum        \
                                      --required-signer $PAYMENT_SKEY           \
                                      --tx-in-marlowe "$TX_2"#1                 \
                                      --tx-in "$TX_2"#0                         \
                                      --tx-in-collateral "$TX_2"#0              \
                                      --tx-out-datum-file example-0.datum       \
                                      --tx-out-value 8000000                    \
                                      --tx-out "$ADDRESS_P"+50000000            \
                                      --change-address "$ADDRESS_P"             \
                                      --invalid-before    40000000              \
                                      --invalid-hereafter 80000000              \
                                      --out-file tx.raw                         \
                                      --print-stats                             \
                                      --submit=600                              \
      | sed -e 's/^TxId "\(.*\)"$/\1/'
      )
    
    Fee: Lovelace 1079301
    Size: 14287 / 16384 = 87%
    Execution units:
      Memory: 3407948 / 12500000 = 27%
      Steps: 1157132402 / 10000000000 = 11%
    
    $ echo TxId "$TX_3"
    
    TxId "e47d1c8a761c0bcdac9910d4da4bf7d4ff4590932861a916543b0e570b538b93"


## 12. Wait until the transaction is appears on the blockchain.

    $ cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    e47d1c8a761c0bcdac9910d4da4bf7d4ff4590932861a916543b0e570b538b93     1        8000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "a52838e20767cabb955d387ffad8c68184c4cdc78e7c60e51ee6b2fd25e07909"


## 13. Withdrawn the remaining 8 ADA.

Finally, the contract allows the remaining 8 ADA to be paid out.

    $ TX_4=$(
      marlowe-cli transaction-close "${MAGIC[@]}"                             \
                                    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                                    --tx-in-script-file example.plutus        \
                                    --tx-in-redeemer-file example-0.redeemer  \
                                    --tx-in-datum-file example-0.datum        \
                                    --tx-in-marlowe "$TX_3"#1                 \
                                    --tx-in "$TX_3"#0                         \
                                    --tx-in-collateral "$TX_3"#0              \
                                    --tx-out "$ADDRESS_P"+8000000             \
                                    --change-address "$ADDRESS_P"             \
                                    --invalid-before    40000000              \
                                    --invalid-hereafter 80000000              \
                                    --out-file tx.raw                         \
                                    --required-signer $PAYMENT_SKEY           \
                                    --print-stats                             \
                                    --submit=600                              \
      | sed -e 's/^TxId "\(.*\)"$/\1/'
      )
    
    Fee: Lovelace 981699
    Size: 14125 / 16384 = 86%
    Execution units:
      Memory: 2294062 / 12500000 = 18%
      Steps: 793702300 / 10000000000 = 7%
    
    $ echo TxId "$TX_4"
    
    TxId "8781cdecc68cd16289a3023e27c16adca7bc92c337216d642a62c4303559ec35"


## 14. See that the transaction succeeded.

After the transaction is recorded on the blockchain, we see that the funds were removed from the script address and are in the wallet.

    $ cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_S"
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    
    $ cardano-cli query utxo "${MAGIC[@]}" --address "$ADDRESS_P"
    
                               TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    008cb56948dc22d9cf7b8eda58d2a1c5d6356576a0dd34c43ec4ee1cd60f1dfb     2        50000000 lovelace + TxOutDatumNone
    8781cdecc68cd16289a3023e27c16adca7bc92c337216d642a62c4303559ec35     0        789549683 lovelace + TxOutDatumNone
    8781cdecc68cd16289a3023e27c16adca7bc92c337216d642a62c4303559ec35     1        8000000 lovelace + TxOutDatumNone
    e47d1c8a761c0bcdac9910d4da4bf7d4ff4590932861a916543b0e570b538b93     2        50000000 lovelace + TxOutDatumNone
