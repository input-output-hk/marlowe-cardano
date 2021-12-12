# Marlowe CLI: Build a Non-Marlowe Transaction

The `transaction-simple` command builds a non-Marlowe transaction. This command is provided as a convenient alternative to using `cardano-cli transaction build`.


## Usage

    $ marlowe-cli transaction-simple --help
    
    Usage: marlowe-cli transaction-simple [--testnet-magic INTEGER]
                                          --socket-path SOCKET_FILE
                                          [--required-signer SIGNING_FILE]
                                          [--tx-in TXID#TXIX]
                                          [--tx-out ADDRESS+VALUE]
                                          --change-address ADDRESS --out-file FILE
                                          [--submit SECONDS] [--print-stats]
      Build a non-Marlowe transaction.
    
    Available options:
      --testnet-magic INTEGER         Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE       Location of the cardano-node socket file.
      --required-signer SIGNING_FILE  Files containing required signing keys.
      --tx-in TXID#TXIX               Transaction input in TxId#TxIx format.
      --tx-out ADDRESS+VALUE          Transaction output in ADDRESS+VALUE format.
      --change-address ADDRESS        Address to receive ADA in excess of fee.
      --out-file FILE                 Output file for transaction body.
      --submit SECONDS                Also submit the transaction, and wait for confirmation.
      --print-stats                   Print statistics.
      -h,--help                       Show this help text


## Example

See [simple-test.sh](simple-test.sh).
