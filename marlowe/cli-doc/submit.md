# Marlowe CLI: Submitting Transactions

The `submit` command submits a transaction body to the blockchain.


## Options

    Usage: marlowe-cli submit [--testnet-magic INTEGER] --socket-path SOCKET_FILE
                              --tx-body-file BODY_FILE
                              [--required-signer SIGNING_FILE]
      Submit a transaction body.
    
    Available options:
      --testnet-magic INTEGER         Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE       Location of the cardano-node socket file.
      --tx-body-file BODY_FILE        File containing the transaction body.
      --required-signer SIGNING_FILE  Files containing required signing keys.
      -h,--help                       Show this help text
