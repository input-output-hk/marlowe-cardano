# Marlowe CLI: Clean UTxOs at an Address

The `marlowe-cli util clean` command creates and optionally runs a transaction that separates UTxOs at an address by token.


## Usage

    $ marlowe-cli util clean --help
    
    Usage: marlowe-cli util clean [--testnet-magic INTEGER]
                                  --socket-path SOCKET_FILE
                                  [--required-signer SIGNING_FILE]
                                  [--lovelace LOVELACE] --change-address ADDRESS
                                  --out-file FILE [--submit SECONDS]
      Reorganize the UTxOs at an address, separating tokens.
    
    Available options:
      --testnet-magic INTEGER         Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE       Location of the cardano-node socket file.
      --required-signer SIGNING_FILE  File containing a required signing key.
      --lovelace LOVELACE             The lovelace to send with each bundle of tokens.
      --change-address ADDRESS        Address to receive ADA in excess of fee.
      --out-file FILE                 Output file for transaction body.
      --submit SECONDS                Also submit the transaction, and wait for confirmation.
      -h,--help                       Show this help text
