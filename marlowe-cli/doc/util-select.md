# Marlowe CLI: Select Ada or Native Tokens

The `marlowe-cli util select` command selects UTxOs containing tokens meeting a criterion.


## Usage

    $ marlowe-cli util select --help
    
    Usage: marlowe-cli util select [--testnet-magic INTEGER]
                                   --socket-path SOCKET_FILE 
                                   [--all | --lovelace-only LOVELACE | 
                                     --asset-only CURRENCY_SYMBOL.TOKEN_NAME]
                                   ADDRESS
      Select UTxO by asset.
    
    Available options:
      --testnet-magic INTEGER  Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file.
      --all                    Report all output.
      --lovelace-only LOVELACE The minimum Lovelace that must be the sole asset in
                               the output value.
      --asset-only CURRENCY_SYMBOL.TOKEN_NAME
                               The current symbol and token name for the sole native
                               asset in the value.
      ADDRESS                  The address.
      -h,--help                Show this help text
