# Marlowe CLI: Validator Information

The `validator` command writes the default Marlowe validator to a script text envelope suitable for use with the `--tx-in-script-file` option of `cardano-cli transaction build`. It also prints the script address.


## Options

    $ marlowe-cli validator --help
    
    Usage: marlowe-cli validator [--testnet-magic INTEGER] [--stake-address ADDRESS]
                                 --validator-file OUTPUT_FILE [--print-hash]
                                 [--print-stats]
      Export a validator to a JSON file.
    
    Available options:
      --testnet-magic INTEGER        Network magic, or omit for mainnet.
      --stake-address ADDRESS        Stake address, if any.
      --validator-file OUTPUT_FILE   JSON output file for validator.
      --print-hash                   Print validator hash.
      --print-stats                  Print statistics.
      -h,--help                      Show this help text

The stake address can be omitted if no staking will be done at the script address.


## Example

The following commands create the validator [example.plutus](example.plutus), which can be used as `--tx-in-script-file example.plutus` in `cardano-cli transaction build`. It also prints the script address.

    $ marlowe-cli address --testnet-magic 1097911063 \
                          --validator-file example.plutus
    
    addr_test1wqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6q2ng95l
    
    
    $ marlowe-cli address --testnet-magic 1097911063                                                  \
                          --stake-address stake1uyx07tvec6fuff78s7v456fx94ukmpvh4x6tynjhmqwta8c09uy75 \
                          --validator-file example.plutus
    
    addr_test1zqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6qvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60sa5y78q
    
    
    $ cat example.plutus
    
    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "593e0f593e.......100300220011"
    }
