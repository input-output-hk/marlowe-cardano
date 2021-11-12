# Marlowe CLI: Redeemer Information

The `redeemer` command writes a Marlowe redeemer to a JSON file in the detailed schema format suitable for use with the `--tx-in-redeemer-file` option of `cardano-cli transaction build`.


## Options

    $ marlowe-cli redeemer --help
    
    Usage: marlowe-cli redeemer --min-slot SLOT_NUMBER --max-slot SLOT_NUMBER
                                --redeemer-file OUTPUT_FILE [--print-stats]
      Export a redeemer to a JSON file.
    
    Available options:
      --min-slot SLOT_NUMBER        Minimum slot for the redemption.
      --max-slot SLOT_NUMBER        Maximum slot for the redemption.
      --redeemer-file OUTPUT_FILE   JSON output file for redeemer.
      --print-stats                 Print statistics.
      -h,--help                     Show this help text

The minimum and maximum slot numbers generally should match the `--invalid-before` and `--invalid-hereafter` options of `cardano-cli transaction build`.

Optionally, this command will print to `stderr` the size in bytes of the redeemer.


## Example

The following command creates the redeemer [example.redeemer](example.redeemer), which can be used as `--tx-in-redeemer-file example.redeemer` in `cardano-cli transaction`.

    $ marlowe-cli redeemer --min-slot 1000     \
                           --max-slot 43500000 \
                           --redeemer-file example.redeemer
    
    
    $ cat example.redeemer
    
    {
        "constructor": 0,
        "fields": [
            {
                "constructor": 0,
                "fields": [
                    { "int": 1000 },
                    { "int": 43500000 }
                ]
            },
            { "list": [] }
        ]
    }
