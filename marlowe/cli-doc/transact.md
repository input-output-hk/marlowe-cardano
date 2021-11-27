# Marlowe CLI: Build a Non-Marlowe Transaction

The `transact` command builds a non-Marlowe transaction. This command is provided as a convenient alternative to using `cardano-cli transaction build`.


## Usage

    $ marlowe-cli transact --help
    
    Usage: marlowe-cli transact [--testnet-magic INTEGER]
                                --socket-path SOCKET_FILE [--tx-in TXID#TXIX]
                                [--tx-out ADDRESS+LOVELACE]
                                --change-address ADDRESS --out-file FILE
      Build a non-Marlowe transaction.
    
    Available options:
      --testnet-magic INTEGER    Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE  Location of the cardano-node socket file.
      --tx-in TXID#TXIX          Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+LOVELACE  Transaction output in ADDRESS+LOVELACE format.
      --change-address ADDRESS   Address to receive ADA in excess of fee.
      --out-file FILE            Output file for transaction body.
      -h,--help                  Show this help text
