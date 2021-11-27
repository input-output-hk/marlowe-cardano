# Marlowe CLI: Build a Transaction that Pays to a Marlowe Contract

The `create` command builds a transaction that pays to a Marlowe contract. This command can be used to initially fund a Marlowe contract.


## Usage

    $ marlowe-cli create --help
    
    Usage: marlowe-cli create [--testnet-magic INTEGER]
                              --socket-path SOCKET_FILE
                              --script-address ADDRESS
                              --tx-out-datum-file DATUM_FILE
                              --tx-out-value LOVELACE [--tx-in TXID#TXIX]
                              [--tx-out ADDRESS+LOVELACE]
                              --change-address ADDRESS --out-file FILE
      Build a transaction that pays to a Marlowe script.
    
    Available options:
      --testnet-magic INTEGER         Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE       Location of the cardano-node socket file.
      --script-address ADDRESS        Address of the Marlowe contract.
      --tx-out-datum-file DATUM_FILE  Datum JSON file datum paid to Marlowe contract.
      --tx-out-value LOVELACE         Lovelace value paid to Marlowe contract.
      --tx-in TXID#TXIX               Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+LOVELACE       Transaction output in ADDRESS+LOVELACE format.
      --change-address ADDRESS        Address to receive ADA in excess of fee.
      --out-file FILE                 Output file for transaction body.
      -h,--help                       Show this help text
