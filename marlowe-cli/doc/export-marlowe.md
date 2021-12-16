# Marlowe CLI: Export Validator, Datum, and Redeemer Information

The `export-marlowe` command writes a JSON file with comprehensive information about the Marlowe validator, datum, and redeemer for a contract. In contrast, the [`export-address`](export-address.md), [`export-validator`](export-validator.md), [`export-datum`](export-datum.md), and [`export-redeemer`](export-redeemer.md) commands only export information on one aspect of the contract.


## Usage

    $ marlowe-cli export-marlowe --help
    
    Usage: marlowe-cli export-marlowe [--testnet-magic INTEGER]
                                      [--stake-address ADDRESS]
                                      [--roles-currency CURRENCY_SYMBOL]
                                      --contract-file CONTRACT_FILE
                                      --state-file STATE_FILE
                                      [--input-file INPUT_FILE]
                                      [--out-file OUTPUT_FILE] [--print-stats]
      Export a Marlowe contract to a JSON file.
    
    Available options:
      --testnet-magic INTEGER           Network magic, or omit for mainnet.
      --stake-address ADDRESS           Stake address, if any.
      --roles-currency CURRENCY_SYMBOL  The currency symbol for roles, if any.
      --contract-file CONTRACT_FILE     JSON input file for the contract.
      --state-file STATE_FILE           JSON input file for the contract state.
      --input-file INPUT_FILE           JSON input file for redeemer inputs.
      --out-file OUTPUT_FILE            JSON output file for contract.
      --print-stats                     Print statistics.
      -h,--help                         Show this help text

The stake address can be omitted if no staking will be done at the script address. If the currency symbol is omitted, then ADA is used as the currency for the Marlowe roles.

See the `Contract` and `State` data types in [`Language.Marlowe.SemanticTypes`](../src/Language/Marlowe/SemanticsTypes.hs) for valid JSON to represent the contract and its state. The simplest contract is [`Close`](example.contract) and the [simplest state](example.state) is a public key for the actor withdrawing funds from the script and the amount withdrawn, along with a minimum slot number for the withdrawal.

For the redeemer, the minimum and maximum slot numbers generally should match the `--invalid-before` and `--invalid-hereafter` options of `cardano-cli transaction build`.

Optionally, this command will print on `stderr` the size and cost of the contract.


# Example

The following command uses the close contract [example.contract](example.contract), the simple state [example.state](example.state), and the inputs [example.inputs](example.inputs) to create the datum to create the JSON file [example.marlowe](example.marlowe) that contains address, validator, datum, and redeemer information for a Marlowe contract.

    $ marlowe-cli export-marlowe --testnet-magic 1097911063       \
                                 --contract-file example.contract \
                                 --state-file example.state       \
                                 --out-file example.marlowe       \
                                 --print-stats
    
    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 36829301, exBudgetMemory = ExMemory 123800}
    Validator size: 13756
    Datum size: 64
    Redeemer size: 1
    Total size: 13821
    
    
    $ cat example.marlowe
    
    {
        "validator": {
            "address": "addr_test1wqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6q2ng95l",
            "cost": {
                "exBudgetCPU": 50941703,
                "exBudgetMemory": 171200
            },
            "hash": "00e162323c8604cdf0c4ce46ee9bcc3fda18688ec01c6c27bc83f5e8",
            "cborHex": "593e0c.........0100300220011",
            "size": 15887
        },
        "datum": {
            "json": {
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
            },
            "hash": "0c050b99438fcd2c65c54b062338f3692c212cbfb499cfe3ad6a9a07ce15dbc0",
            "cborHex": "d8799fd8799fa1d8799fd8799f581c0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07ffd8799f4040ffff1a002dc6c0a0a00affd87980ff",
            "size": 64
        },
        "redeemer": {
            "json": {
                "list": []
            },
            "cboxHex": "d8799fd8799f1903e81a0297c1e0ff80ff",
            "size": 17
        }
    }
