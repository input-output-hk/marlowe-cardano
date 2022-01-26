# Marlowe CLI: Initialize a Marlowe Contract

The `marlowe-cli run initialize` command creates the initial configuration for a Marlowe contract.


## Usage

    $ marlowe-cli run initialize --help
    
    Usage: marlowe-cli run initialize [--testnet-magic INTEGER]
                                      [--slot-length INTEGER]
                                      [--slot-offset INTEGER]
                                      [--stake-address ADDRESS]
                                      [--roles-currency CURRENCY_SYMBOL]
                                      --contract-file CONTRACT_FILE
                                      --state-file STATE_FILE
                                      [--out-file OUTPUT_FILE] [--print-stats]
      Initialize the first transaction of a Marlowe contract and write output to a
      JSON file.
    
    Available options:
      --testnet-magic INTEGER           Network magic, or omit for mainnet.
      --slot-length INTEGER             The slot length, in milliseconds.
      --slot-offset INTEGER             The effective POSIX time of slot zero, in milliseconds.
      --stake-address ADDRESS           Stake address, if any.
      --roles-currency CURRENCY_SYMBOL  The currency symbol for roles, if any.
      --contract-file CONTRACT_FILE     JSON input file for the contract.
      --state-file STATE_FILE           JSON input file for the contract state.
      --out-file OUTPUT_FILE            JSON output file for initialize.
      --print-stats                     Print statistics.
      -h,--help                         Show this help text
