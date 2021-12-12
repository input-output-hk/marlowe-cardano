# Marlowe CLI: Redeemer Information

The `export-redeemer` command writes a Marlowe redeemer to a JSON file in the detailed schema format suitable for use with the `--tx-in-redeemer-file` option of `cardano-cli transaction build`.


## Options

    $ marlowe-cli export-redeemer --help
    
    Usage: marlowe-cli export-redeemer [--input-file INPUT_FILE]
                                       [--out-file OUTPUT_FILE] [--print-stats]
      Export a redeemer to a JSON file.
    
    Available options:
      --input-file INPUT_FILE  JSON input file for redeemer inputs.
      --out-file OUTPUT_FILE   JSON output file for redeemer.
      --print-stats            Print statistics.
      -h,--help                Show this help text

Optionally, this command will print to `stderr` the size in bytes of the redeemer.


## Example

The following command uses the inputs [example-2.input](example-2.input) to create the redeemer [example-2.redeemer](example-2.redeemer), which can be used as `--tx-in-redeemer-file example.redeemer` in `cardano-cli transaction`.

    $ marlowe-cli export-redeemer --input-file example-2.input  \
                                  --out-file example-2.redeemer \
                                  --print-stats
    
    Redeemer size: 85
    
    
    $ cat example-2.redeemer
    
    {
        "list": [
            {
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
                            { "bytes": "0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07" }
                        ]
                    },
                    {
                        "constructor": 0,
                        "fields": [
                            { "bytes": "" },
                            { "bytes": "" }
                        ]
                    },
                    {
                        "int": 10000000
                    }
                ]
            }
        ]
    }
