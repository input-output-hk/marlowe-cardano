# Marlowe CLI: Query Transaction Output

The `marlowe-cli query output` command extracts from the chain index the output information according to a specified criterion.


## Usage

    $ marlowe-cli query output --help
    
    Usage: marlowe-cli query output --index-url URL 
                                    [--all | --lovelace-only LOVELACE | 
                                      --asset-only CURRENCY_SYMBOL.TOKEN_NAME] 
                                    [--spent] [--out-file OUTPUT_FILE] ADDRESS
      Query output details.
    
    Available options:
      --index-url URL          URL for the Plutus chain index.
      --all                    Report all output.
      --lovelace-only LOVELACE The minimum Lovelace that must be the sole asset in
                               the output value.
      --asset-only CURRENCY_SYMBOL.TOKEN_NAME
                               The current symbol and token name for the sole native
                               asset in the value.
      --spent                  Whether to also report spent transactions.
      --out-file OUTPUT_FILE   JSON output file for address data.
      ADDRESS                  The address.
      -h,--help                Show this help text
