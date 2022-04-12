# Marlowe CLI: Query Transactions at an Address.

The `marlowe-cli query address` command extracts detailed transaction information from an address.


## Usage

    $ marlowe-cli query address --help
    
    Usage: marlowe-cli query address --index-url URL [--spent] 
                                     [--out-file OUTPUT_FILE] ADDRESS
      Query transactions at an address.
    
    Available options:
      --index-url URL          URL for the Plutus chain index.
      --spent                  Whether to also report spent transactions.
      --out-file OUTPUT_FILE   JSON output file for address data.
      ADDRESS                  The address.
      -h,--help                Show this help text
