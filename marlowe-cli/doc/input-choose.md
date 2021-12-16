# Marlowe CLI: Create Input JSON File for a Choice

The `input-choose` command creates the JSON input file representing a choice into a Marlowe contract.


## Usage

    $ marlowe-cli input-choose --help
    
    Usage: marlowe-cli input-choose --choice-name NAME --choice-party PARTY
                                    --choice-number INTEGER [--out-file OUTPUT_FILE]
      Create Marlowe input for a choice.
    
    Available options:
      --choice-name NAME       The name of the choice made.
      --choice-party PARTY     The party making the choice.
      --choice-number INTEGER  The number chosen.
      --out-file OUTPUT_FILE   JSON output file for contract input.
      -h,--help                Show this help text
