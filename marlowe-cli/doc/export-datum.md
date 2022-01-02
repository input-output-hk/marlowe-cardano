# Marlowe CLI: Datum Information

The `marlowe-cli contract datum` command writes a Marlowe datum to a JSON file in the detailed schema format suitable for use with the `--tx-in-datum-file` option of `cardano-cli transaction build`.


## Options

    $ marlowe-cli contract datum --help
    
    Usage: marlowe-cli contract datum [--slot-length INTEGER]
                                      [--slot-offset INTEGER]
                                      --contract-file CONTRACT_FILE
                                      --state-file STATE_FILE
                                      [--out-file DATUM_FILE] [--print-stats]
      Export a contract datum to a JSON file.
    
    Available options:
      --slot-length INTEGER          The slot length, in milliseconds.
      --slot-offset INTEGER          The effective POSIX time of slot zero, in milliseconds.
      --contract-file CONTRACT_FILE  JSON input file for the contract.
      --state-file STATE_FILE        JSON input file for the contract state.
      --out-file DATUM_FILE          JSON output file for datum.
      --print-stats                  Print statistics.
      -h,--help                      Show this help text

See the `Contract` and `State` data types in [`Language.Marlowe.SemanticTypes`](../src/Language/Marlowe/SemanticsTypes.hs) for valid JSON to represent the contract and its state. The simplest contract is [`Close`](example.contract) and the [simplest state](example.state) is a public key for the actor withdrawing funds from the script and the amount withdrawn, along with a minimum slot number for the withdrawal.

This command prints the hash of the datum on `stdout`. Optionally, it will print on `stderr` the size in bytes of the datum.


## Example

The following command uses the close contract [example.contract](example.contract) and the simple state [example.state](example.state) to create the datum [example.datum](example.datum), which can be used as `--tx-in-datum-file example.datum` in `cardano-cli transaction build`. It also prints the hash of the datum.

    $ marlowe-cli contract datum --contract-file example.contract \
                                 --state-file example.state       \
                                 --out-file example.datum
    
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
                    { "int": 40000000 }
                ]
            },
            {
                "constructor": 0,
                "fields": []
            }
        ]
    }
