# Marlowe CLI: Mint Native Tokens

The `marlowe-cli util mint` command mints or burns native tokens.


## Usage

    $ marlowe-cli util mint --help
    
    Usage: marlowe-cli util mint [--testnet-magic INTEGER] --socket-path SOCKET_FILE
                                 --required-signer SIGNING_FILE 
                                 [--metadata-file JSON_FILE] [--count INTEGER] 
                                 [--expires SLOT_NO] [--lovelace LOVELACE]
                                 --change-address ADDRESS --out-file FILE 
                                 [--submit SECONDS] TOKEN_NAME
      Mint native tokens.
    
    Available options:
      --testnet-magic INTEGER  Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file.
      --required-signer SIGNING_FILE
                               File containing a required signing key.
      --metadata-file JSON_FILE
                               The CIP-25 metadata, with keys for each token name.
      --count INTEGER          The number of each token to mint.
      --expires SLOT_NO        The slot number after which miniting is no longer
                               possible.
      --lovelace LOVELACE      The lovelace to send with each bundle of tokens.
      --change-address ADDRESS Address to receive ADA in excess of fee.
      --out-file FILE          Output file for transaction body.
      --submit SECONDS         Also submit the transaction, and wait for
                               confirmation.
      TOKEN_NAME               The name of the token.
      -h,--help                Show this help text
