# Marlowe CLI: Query Marlowe Contract History

The `marlowe-cli query history` command extracts from the chain index the detailed history of a Marlowe contract that was created with the PAB.


## Usage

    $ marlowe-cli query history --help
    
    Usage: marlowe-cli query history --index-url URL 
                                     [--roles-currency CURRENCY_SYMBOL] 
                                     [--out-file OUTPUT_FILE]
      Query for the Marlowe contract histories.
    
    Available options:
      --index-url URL          URL for the Plutus chain index.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --out-file OUTPUT_FILE   JSON output file for history data.
      -h,--help                Show this help text
