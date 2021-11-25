# Marlowe CLI: Redeemer Information

The `redeemer` command writes a Marlowe redeemer to a JSON file in the detailed schema format suitable for use with the `--tx-in-redeemer-file` option of `cardano-cli transaction build`.


## Options

    $ marlowe-cli redeemer --help
    
    Usage: marlowe-cli redeemer [--inputs-file INPUTS_FILE] --out-file OUTPUT_FILE
                                [--print-stats]
      Export a redeemer to a JSON file.
    
    Available options:
      --inputs-file INPUTS_FILE   JSON input file for redeemer inputs, if any.
      --out-file OUTPUT_FILE      JSON output file for redeemer.
      --print-stats               Print statistics.
      -h,--help                   Show this help text

The minimum and maximum slot numbers generally should match the `--invalid-before` and `--invalid-hereafter` options of `cardano-cli transaction build`.

Optionally, this command will print to `stderr` the size in bytes of the redeemer.


## Example

The following command uses the inputs [example.inputs](example.inputs) to create the redeemer [example.redeemer](example.redeemer), which can be used as `--tx-in-redeemer-file example.redeemer` in `cardano-cli transaction`.

    $ marlowe-cli redeemer --inputs-file example.inputs \
                           --out-file example.redeemer
    
    
    $ cat example.redeemer
    
    {
        "list": []
    }
