# Marlowe CLI: Datum Information

The `datum` command writes a Marlowe datum to a JSON file in the detailed schema format suitable for use with the `--tx-in-datum-file` option of `cardano-cli transaction build`.


## Options

    $ marlowe-cli datum --help
    
    Usage: marlowe-cli datum --account-hash PUB_KEY_HASH --account-value LOVELACE
                             --min-slot MIN_SLOT --datum-file OUTPUT_FILE 
                             [--print-stats]
      Export a datum to a JSON file.
    
    Available options:
      --account-hash PUB_KEY_HASH   Public key hash for the account.
      --account-value LOVELACE      Lovelace value for the account.
      --min-slot MIN_SLOT           Minimum slot for the contract state.
      --datum-file OUTPUT_FILE      JSON output file for datum.
      --print-stats                 Print statistics.
      -h,--help                     Show this help text

The account hash corresponds to the public key of the actor withdrawing funds from the script and the account value corresponds to the amount withdrawn. The minimum slot is that of the [Marlowe state](../src/Language/Marlowe/SemanticsTypes.hs).

This command prints the hash of the datum on `stdout`. Optionally, it will print on `stderr` the size in bytes of the datum.


## Example

The following command creates the datum [example.datum](example.datum), which can be used as `--tx-in-datum-file example.datum` in `cardano-cli transaction build`. It also prints the hash of the datum.

    $ marlowe-cli datum --account-hash 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07 \
                        --account-value 3000000                                                 \
                        --min-slot 10                                                           \
                        --datum-file example.datum
    
    0c050b99438fcd2c65c54b062338f3692c212cbfb499cfe3ad6a9a07ce15dbc0
    
    
    $ cat example.datum
    
    {
        "constructor": 0,
        "fields": [
            {
                "constructor": 0,
                "fields": [
                    {
                        "map": [
                            {
                                "v": { "int": 3000000 },
                                "k": {
                                    "constructor": 0,
                                    "fields": [
                                        {
                                            "constructor": 0,
                                            "fields": [
                                                { "bytes": "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07" }
                                            ]
                                        },
                                        {
                                            "constructor": 0,
                                            "fields": [
                                                { "bytes": "" },
                                                { "bytes": "" }
                                            ]
                                        }
                                    ]
                                }
                            }
                        ]
                    },
                    { "map": [] },
                    { "map": [] },
                    { "int": 10 }
                ]
            },
            {
                "constructor": 0,
                "fields": []
            }
        ]
    }
