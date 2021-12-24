# Marlowe CLI: Build a Transaction that Pays to and Spends from a Marlowe Contract

The `marlowe transaction advance` command builds a transaction that both pays to and spends from a Marlowe contract. This command can be used for continuing operation of a Marlowe contract.


## Usage

    $ marlowe-cli transaction advance --help
    
    Usage: marlowe-cli transaction advance [--testnet-magic INTEGER]
                                           --socket-path SOCKET_FILE
                                           --script-address ADDRESS
                                           --tx-in-script-file PLUTUS_FILE
                                           --tx-in-redeemer-file REDEEMER_FILE
                                           --tx-in-datum-file DATUM_FILE
                                           [--required-signer SIGNING_FILE]
                                           --tx-in-marlowe TXID#TXIX
                                           --tx-out-datum-file DATUM_FILE
                                           --tx-out-marlowe VALUE
                                           [--tx-in TXID#TXIX]
                                           [--tx-out ADDRESS+VALUE]
                                           --tx-in-collateral TXID#TXIX
                                           --change-address ADDRESS
                                           --invalid-before SLOT
                                           --invalid-hereafter SLOT --out-file FILE
                                           [--submit SECONDS] [--print-stats]
                                           [--script-invalid]
      Build a transaction that both spends from and pays to a Marlowe script.
    
    Available options:
      --testnet-magic INTEGER              Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE            Location of the cardano-node socket file.
      --script-address ADDRESS             Address of the Marlowe contract.
      --tx-in-script-file PLUTUS_FILE      Plutus file for Marlowe contract.
      --tx-in-redeemer-file REDEEMER_FILE  Redeemer JSON file spent from Marlowe contract.
      --tx-in-datum-file DATUM_FILE        Datum JSON file spent from Marlowe contract.
      --required-signer SIGNING_FILE       Files containing required signing keys.
      --tx-in-marlowe TXID#TXIX            UTxO spent from Marlowe contract.
      --tx-out-datum-file DATUM_FILE       Datum JSON file datum paid to Marlowe contract.
      --tx-out-marlowe VALUE               Value paid to Marlowe contract.
      --tx-in TXID#TXIX                    Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE               Transaction output in ADDRESS+VALUE format.
      --tx-in-collateral TXID#TXIX         Collateral for transaction.
      --change-address ADDRESS             Address to receive ADA in excess of fee.
      --invalid-before SLOT                Minimum slot for the redemption.
      --invalid-hereafter SLOT             Maximum slot for the redemption.
      --out-file FILE                      Output file for transaction body.
      --submit SECONDS                     Also submit the transaction, and wait for confirmation.
      --print-stats                        Print statistics.
      --script-invalid                     Assert that the transaction is invalid.
      -h,--help                            Show this help text


## Example

See [extended-tutorial.md](extended-tutorial.md).
