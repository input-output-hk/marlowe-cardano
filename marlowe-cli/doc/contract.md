---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe CLI `contract` Subcommands

The `marlowe-cli contract` commands provide low-level access to the
Plutus mechanics of Marlowe contracts: script address, validator, datum,
and redeemer.

## Contents

-   [Address](#address)
-   [Datum](#datum)
-   [Redeemer](#redeemer)
-   [Validator](#validator)
-   [Marlowe](#marlowe)

## Available Commands

``` bash
marlowe-cli contract --help
```

    Usage: marlowe-cli contract COMMAND

      Export contract address, validator, datum, or redeemer.

    Available options:
      -h,--help                Show this help text

    Low-level commands for exporting Marlowe contract information:
      address                  Print a contract validator address.
      datum                    Export a contract datum to a JSON file.
      marlowe                  Export a Marlowe contract to a JSON file.
      redeemer                 Export a contract redeemer to a JSON file.
      validator                Export a contract validator to a JSON file.

## Address

``` bash
marlowe-cli contract address --help
```

    Usage: marlowe-cli contract address 
             --testnet-magic INTEGER [--stake-address ADDRESS] 
             [--roles-currency CURRENCY_SYMBOL]

      Print a contract validator address.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      -h,--help                Show this help text

### Example

Compute the Marlowe application\'s script address for a particular roles
currency.

``` bash
marlowe-cli contract address --testnet-magic 1566 \
                             --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f
```

    addr_test1wz05yhgck28y4avk9zvy94ekcv75arjrz0g6z2l9f0lrgusf93am7

## Datum

``` bash
marlowe-cli contract datum --help
```

    Usage: marlowe-cli contract datum --contract-file CONTRACT_FILE
                                      --state-file STATE_FILE 
                                      [--out-file DATUM_FILE] [--print-stats]

      Export a contract datum to a JSON file.

    Available options:
      --contract-file CONTRACT_FILE
                               JSON input file for the contract.
      --state-file STATE_FILE  JSON input file for the contract state.
      --out-file DATUM_FILE    JSON output file for datum.
      --print-stats            Print statistics.
      -h,--help                Show this help text

### Example

This invocation of `marlowe-cli contract datum` takes the contract file
[contract-1.contract](contract-1.contract) and state file
[contract-1.state](contract-1.state) as inputs and it outputs the
corresponding Plutus datum file [contract-1.datum](contract-1.datum).

``` bash
json2yaml contract-1.contract
```

    close
    ...

``` bash
json2yaml contract-1.state
```

    accounts:
    - - - pk_hash: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
        - currency_symbol: ''
          token_name: ''
      - 8000000
    boundValues: []
    choices: []
    minTime: 40000000

``` bash
marlowe-cli contract datum --contract-file contract-1.contract \
                           --state-file contract-1.state \
                           --out-file contract-1.datum \
                           --print-stats
```

    a52838e20767cabb955d387ffad8c68184c4cdc78e7c60e51ee6b2fd25e07909

    Datum size: 68

``` bash
json2yaml contract-1.datum
```

    constructor: 0
    fields:
    - constructor: 0
      fields:
      - map:
        - k:
            constructor: 0
            fields:
            - constructor: 0
              fields:
              - bytes: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
            - constructor: 0
              fields:
              - bytes: ''
              - bytes: ''
          v:
            int: 8000000
      - map: []
      - map: []
      - int: 40000000
    - constructor: 0
      fields: []

## Redeemer

``` bash
marlowe-cli contract redeemer --help
```

    Usage: marlowe-cli contract redeemer 
             [--input-file INPUT_FILE] [--out-file OUTPUT_FILE] [--print-stats]

      Export a contract redeemer to a JSON file.

    Available options:
      --input-file INPUT_FILE  JSON input file for redeemer inputs.
      --out-file OUTPUT_FILE   JSON output file for redeemer.
      --print-stats            Print statistics.
      -h,--help                Show this help text

### Example

This invocation of `marlowe-cli contract redeemer` takes the Marlowe
input file [contract-2.input](contract-2.input) as input and it outputs
the corresponding Plutus redeemer
[contract-2.redeemer](contract-2.redeemer).

``` bash
json2yaml contract-2.input
```

    input_from_party:
      pk_hash: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
    into_account:
      pk_hash: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
    of_token:
      currency_symbol: ''
      token_name: ''
    that_deposits: 10000000

``` bash
marlowe-cli contract redeemer --input-file contract-2.input \
                              --out-file contract-2.redeemer \
                              --print-stats
```


    Redeemer size: 89

``` bash
json2yaml contract-2.redeemer
```

    list:
    - constructor: 0
      fields:
      - constructor: 0
        fields:
        - constructor: 0
          fields:
          - bytes: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
        - constructor: 0
          fields:
          - bytes: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
        - constructor: 0
          fields:
          - bytes: ''
          - bytes: ''
        - int: 10000000

## Validator

``` bash
marlowe-cli contract validator --help
```

    Usage: marlowe-cli contract validator 
             --testnet-magic INTEGER [--stake-address ADDRESS] 
             [--roles-currency CURRENCY_SYMBOL] [--out-file OUTPUT_FILE] 
             [--print-hash] [--print-stats]

      Export a contract validator to a JSON file.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --out-file OUTPUT_FILE   JSON output file for validator.
      --print-hash             Print validator hash.
      --print-stats            Print statistics.
      -h,--help                Show this help text

## Example

This invocation of `marlowe-cli contract validator` computes the Marlowe
application Plutus script [contract-3.plutus](contract-3.plutus) for a
particular roles currency.

``` bash
marlowe-cli contract validator --testnet-magic 1566 \
                               --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f \
                               --out-file contract-3.plutus \
                               --print-stats
```

    addr_test1wz05yhgck28y4avk9zvy94ekcv75arjrz0g6z2l9f0lrgusf93am7

    Validator size: 12415
    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}

``` bash
head -c 1000 contract-3.plutus
```

    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "59307f59307c010000332323233223232323232323232323232323232323322323232323232323232323232323322323232323232323232323322332232323232323233223232323232323232323232323322332232323232323232323232323232323232323232323232323232323232323232323232323232332232323232323232323232323232323232323232232222323253353332223500a2235005232322350072323232323223353235001223500223223355335333573466e2000400c23804234044c0d0c8488c00400ccd5421c0400c00454cd4ccd5cd19b88001501008e0108d0113034332212233002004003501033550870100300113322122330010040033355087015002001350112222333308a01004003002500623033122222300200622533335333333305408f0100200101000650a10150a101130341222220051303412222200313034122222004222221533500513333038004003002001153333335015221303b03c13501822225335333355307712001505f2209a01004099011303d03e1333303c0080070060052221303c03d2221303c03d222221303e03f2221303c03d15335333573466e2400540382300422c04540384004cc8848cc00400c008d4d401c888888888

## Marlowe

``` bash
marlowe-cli contract marlowe --help
```

    Usage: marlowe-cli contract marlowe 
             --testnet-magic INTEGER [--stake-address ADDRESS] 
             [--roles-currency CURRENCY_SYMBOL] --contract-file CONTRACT_FILE
             --state-file STATE_FILE [--input-file INPUT_FILE] 
             [--out-file OUTPUT_FILE] [--print-stats]

      Export a Marlowe contract to a JSON file.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --contract-file CONTRACT_FILE
                               JSON input file for the contract.
      --state-file STATE_FILE  JSON input file for the contract state.
      --input-file INPUT_FILE  JSON input file for redeemer inputs.
      --out-file OUTPUT_FILE   JSON output file for contract.
      --print-stats            Print statistics.
      -h,--help                Show this help text

### Example

This invocation of `marlowe-cli contract marlowe` takes the contract
file [contract-2.contract](contract-2.contract), the state file
[contract-2.state](contract-2.state), and the Marlowe input file
[contract-2.input](contract-2.input) as inputs and outputs a file
bundling Plutus validator, datum, and redeemer information for the
corresponding Marlowe transaction.

``` bash
marlowe-cli contract marlowe --testnet-magic 1566 \
                             --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f \
                             --contract-file contract-2.contract \
                             --state-file contract-2.state \
                             --input-file contract-2.input \
                             --out-file contract-2.marlowe \
                             --print-stats
```


    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 24652144, exBudgetMemory = ExMemory 82900}
    Validator size: 12415
    Datum size: 264
    Redeemer size: 89
    Total size: 12768

``` bash
jq 'to_entries[] | .key' contract-2.marlowe
```

    "redeemer"
    "validator"
    "datum"
