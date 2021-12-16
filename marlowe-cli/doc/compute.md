# Marlowe CLI: Create Input JSON File for a Deposit

The `compute` command creates the next step in a Marlowe contract, given input.


## Usage

    $ marlowe-cli compute --help
    
    Usage: marlowe-cli compute --contract-file CONTRACT_FILE --state-file STATE_FILE
                               [--input-file INPUT_FILE] --invalid-before SLOT
                               --invalid-hereafter SLOT [--out-file OUTPUT_FILE]
                               [--print-stats]
      Compute the next step of a Marlowe contract and write the output to a JSON file.
    
    Available options:
      --contract-file CONTRACT_FILE  JSON input file for the contract.
      --state-file STATE_FILE        JSON input file for the contract state.
      --input-file INPUT_FILE        JSON input file for redeemer inputs.
      --invalid-before SLOT          Minimum slot for the redemption.
      --invalid-hereafter SLOT       Maximum slot for the redemption.
      --out-file OUTPUT_FILE         JSON output file for contract.
      --print-stats                  Print statistics.
      -h,--help                      Show this help text
