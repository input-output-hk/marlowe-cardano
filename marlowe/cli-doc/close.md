# Marlowe CLI: Build a Transaction that Spends from a Marlowe Contract

The `close` command builds a transaction that spends from a Marlowe contract. This command can be used to close a Marlowe contract.


## Usage

    $ marlowe-cli close --help
    
    Usage: marlowe-cli close [--testnet-magic INTEGER]
                             --socket-path SOCKET_FILE
                             --tx-in-script-file PLUTUS_FILE
                             --tx-in-redeemer-file REDEEMER_FILE
                             --tx-in-datum-file DATUM_FILE
                             [--required-signer SIGNING_FILE]
                             --tx-in-marlowe TXID#TXIX [--tx-in TXID#TXIX]
                             [--tx-out ADDRESS+LOVELACE]
                             --tx-in-collateral TXID#TXIX
                             --change-address ADDRESS --invalid-before SLOT
                             --invalid-hereafter SLOT --out-file FILE
      Build a transaction that spends from a Marlowe script.
    
    Available options:
      --testnet-magic INTEGER              Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE            Location of the cardano-node socket file.
      --tx-in-script-file PLUTUS_FILE      Plutus file for Marlowe contract.
      --tx-in-redeemer-file REDEEMER_FILE  Redeemer JSON file spent from Marlowe contract.
      --tx-in-datum-file DATUM_FILE        Datum JSON file spent from Marlowe contract.
      --required-signer SIGNING_FILE       Files containing required signing keys.
      --tx-in-marlowe TXID#TXIX            UTxO spent from Marlowe contract.
      --tx-in TXID#TXIX                    Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+LOVELACE            Transaction output in ADDRESS+LOVELACE format.
      --tx-in-collateral TXID#TXIX         Collateral for transaction.
      --change-address ADDRESS             Address to receive ADA in excess of fee.
      --invalid-before SLOT                Minimum slot for the redemption.
      --invalid-hereafter SLOT             Maximum slot for the redemption.
      --out-file FILE                      Output file for transaction body.
      -h,--help                            Show this help text
