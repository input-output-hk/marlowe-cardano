# Marlowe CLI: Query Marlowe Contract Status

The `marlowe-cli query app` command extracts from the chain index the detailed status of a Marlowe contract that was created with the PAB.


## Usage

    $ marlowe-cli query app --help
    
    Usage: marlowe-cli query app --index-url URL [--roles-currency CURRENCY_SYMBOL] 
                                 [--spent] [--out-file OUTPUT_FILE]
      Query the state of the Marlowe application script.
    
    Available options:
      --index-url URL          URL for the Plutus chain index.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --spent                  Whether to also report spent transactions.
      --out-file OUTPUT_FILE   JSON output file for Marlowe data.
      -h,--help                Show this help text
