# Marlowe CLI: Create Input JSON File for a Deposit

The `input-deposit` command creates the JSON input file representing a deposit into a Marlowe contract.


## Usage

    $ marlowe-cli input-deposit --help
    
    Usage: marlowe-cli input-deposit --deposit-account PARTY --deposit-party PARTY 
                                     [--deposit-token TOKEN]
                                     --deposit-amount INTEGER 
                                     [--out-file OUTPUT_FILE]
      Create Marlowe input for a deposit.
    
    Available options:
      --deposit-account PARTY   The account for the deposit.
      --deposit-party PARTY     The party making the deposit.
      --deposit-token TOKEN     The token being deposited, if not Ada.
      --deposit-amount INTEGER  The amount of token being deposited.
      --out-file OUTPUT_FILE    JSON output file for contract input.
      -h,--help                 Show this help text
