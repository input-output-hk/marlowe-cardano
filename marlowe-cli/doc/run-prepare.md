# Marlowe CLI: Prepare a Marlowe Transaction

The `marlowe-cli run prepare` command creates the next step in a Marlowe contract, given input.


## Usage

    $ marlowe-cli run prepare --help
    
    Usage: marlowe-cli run prepare --marlowe-file MARLOWE_FILE
                                   [--deposit-account PARTY --deposit-party PARTY
                                     [--deposit-token TOKEN]
                                     --deposit-amount INTEGER |
                                     --choice-name NAME --choice-party PARTY
                                     --choice-number INTEGER |
                                     --notify] --invalid-before SLOT
                                   --invalid-hereafter SLOT [--out-file OUTPUT_FILE]
                                   [--print-stats]
      Prepare the next step of a Marlowe contract and write the output to a JSON
      file.
    
    Available options:
      --marlowe-file MARLOWE_FILE  JSON input file for the Marlowe state and contract.
      --deposit-account PARTY      The account for the deposit.
      --deposit-party PARTY        The party making the deposit.
      --deposit-token TOKEN        The token being deposited, if not Ada.
      --deposit-amount INTEGER     The amount of token being deposited.
      --choice-name NAME           The name of the choice made.
      --choice-party PARTY         The party making the choice.
      --choice-number INTEGER      The number chosen.
      --notify                     Notify the contract.
      --invalid-before SLOT        Minimum slot for the redemption.
      --invalid-hereafter SLOT     Maximum slot for the redemption.
      --out-file OUTPUT_FILE       JSON output file for contract.
      --print-stats                Print statistics.
      -h,--help                    Show this help text
