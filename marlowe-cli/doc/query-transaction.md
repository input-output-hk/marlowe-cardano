# Marlowe CLI: Query Transactions by Their IDs

The `marlowe-cli query transaction` command extracts detailed transaction information from its ID.


## Usage

    $ marlowe-cli query transaction --help
    
    Usage: marlowe-cli query transaction --index-url URL [--out-file OUTPUT_FILE]
                                         TXID
      Query transaction details.
    
    Available options:
      --index-url URL          URL for the Plutus chain index.
      --out-file OUTPUT_FILE   JSON output file for transaction data.
      TXID                     The transaction ID.
      -h,--help                Show this help text
