---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Marlowe CLI `contract` Subcommands

The `marlowe-cli contract` commands provide low-level access to the
Plutus mechanics of Marlowe contracts: script address, validator, datum,
and redeemer.

</div>

<div class="cell markdown">

## Contents

-   [Address](#address)
-   [Datum](#datum)
-   [Redeemer](#redeemer)
-   [Validator](#validator)
-   [Marlowe](#marlowe)

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli contract --help
```

<div class="output stream stdout">

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

</div>

</div>

<div class="cell markdown">

## Address

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli contract address --help
```

<div class="output stream stdout">

    Usage: marlowe-cli contract address 
             --testnet-magic INTEGER [--stake-address ADDRESS]

      Print a contract validator address.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Compute the Marlowe application's script address for a particular roles
currency.

</div>

<div class="cell code" execution_count="3">

``` bash
marlowe-cli contract address --testnet-magic 1566
```

<div class="output stream stdout">

    addr_test1wrv0vwr4megau50ujjwsktvajmsu6dzza2rnalufd3husaqs9v6rv

</div>

</div>

<div class="cell markdown">

## Datum

</div>

<div class="cell code" execution_count="4">

``` bash
marlowe-cli contract datum --help
```

<div class="output stream stdout">

    Usage: marlowe-cli contract datum [--roles-currency CURRENCY_SYMBOL]
                                      --contract-file CONTRACT_FILE
                                      --state-file STATE_FILE 
                                      [--out-file DATUM_FILE] [--print-stats]

      Export a contract datum to a JSON file.

    Available options:
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --contract-file CONTRACT_FILE
                               JSON input file for the contract.
      --state-file STATE_FILE  JSON input file for the contract state.
      --out-file DATUM_FILE    JSON output file for datum.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli contract datum` takes the contract file
[contract-1.contract](contract-1.contract) and state file
[contract-1.state](contract-1.state) as inputs and it outputs the
corresponding Plutus datum file [contract-1.datum](contract-1.datum).

</div>

<div class="cell code" execution_count="5">

``` bash
json2yaml contract-1.contract
```

<div class="output stream stdout">

    close
    ...

</div>

</div>

<div class="cell code" execution_count="6">

``` bash
json2yaml contract-1.state
```

<div class="output stream stdout">

    accounts:
    - - - address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
        - currency_symbol: ''
          token_name: ''
      - 8000000
    boundValues: []
    choices: []
    minTime: 40000000

</div>

</div>

<div class="cell code" execution_count="7">

``` bash
marlowe-cli contract datum --contract-file contract-1.contract \
                           --state-file contract-1.state \
                           --out-file contract-1.datum \
                           --print-stats
```

<div class="output stream stdout">

    b553ce60e89f0f29842d1e0d2ecebd0dbb866f180c30ed25e46c990ba8c8b14a

    Datum size: 89

</div>

</div>

<div class="cell code" execution_count="8">

``` bash
json2yaml contract-1.datum
```

<div class="output stream stdout">

    constructor: 0
    fields:
    - constructor: 0
      fields:
      - bytes: ''
    - constructor: 0
      fields:
      - map:
        - k:
            constructor: 0
            fields:
            - constructor: 0
              fields:
              - constructor: 0
                fields: []
              - constructor: 0
                fields:
                - constructor: 0
                  fields:
                  - bytes: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
                - constructor: 1
                  fields: []
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

</div>

</div>

<div class="cell markdown">

## Redeemer

</div>

<div class="cell code" execution_count="9">

``` bash
marlowe-cli contract redeemer --help
```

<div class="output stream stdout">

    Usage: marlowe-cli contract redeemer 
             [--input-file INPUT_FILE] [--out-file OUTPUT_FILE] [--print-stats]

      Export a contract redeemer to a JSON file.

    Available options:
      --input-file INPUT_FILE  JSON input file for redeemer inputs.
      --out-file OUTPUT_FILE   JSON output file for redeemer.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli contract redeemer` takes the Marlowe
input file [contract-2.input](contract-2.input) as input and it outputs
the corresponding Plutus redeemer
[contract-2.redeemer](contract-2.redeemer).

</div>

<div class="cell code" execution_count="10">

``` bash
json2yaml contract-2.input
```

<div class="output stream stdout">

    input_from_party:
      address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
    into_account:
      address: addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
    of_token:
      currency_symbol: ''
      token_name: ''
    that_deposits: 10000000

</div>

</div>

<div class="cell code" execution_count="11">

``` bash
marlowe-cli contract redeemer --input-file contract-2.input \
                              --out-file contract-2.redeemer \
                              --print-stats
```

<div class="output stream stdout">


    Redeemer size: 119

</div>

</div>

<div class="cell code" execution_count="12">

``` bash
json2yaml contract-2.redeemer
```

<div class="output stream stdout">

    list:
    - constructor: 0
      fields:
      - constructor: 0
        fields:
        - constructor: 0
          fields:
          - constructor: 0
            fields: []
          - constructor: 0
            fields:
            - constructor: 0
              fields:
              - bytes: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
            - constructor: 1
              fields: []
        - constructor: 0
          fields:
          - constructor: 0
            fields: []
          - constructor: 0
            fields:
            - constructor: 0
              fields:
              - bytes: 0a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07
            - constructor: 1
              fields: []
        - constructor: 0
          fields:
          - bytes: ''
          - bytes: ''
        - int: 10000000

</div>

</div>

<div class="cell markdown">

## Validator

</div>

<div class="cell code" execution_count="13">

``` bash
marlowe-cli contract validator --help
```

<div class="output stream stdout">

    Usage: marlowe-cli contract validator 
             --testnet-magic INTEGER [--stake-address ADDRESS] 
             [--protocol-version PROTOCOL_VERSION] [--out-file OUTPUT_FILE] 
             [--print-hash] [--print-stats]

      Export a contract validator to a JSON file.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      --protocol-version PROTOCOL_VERSION
                               Protocol version: [alonzo|vasil]
      --out-file OUTPUT_FILE   JSON output file for validator.
      --print-hash             Print validator hash.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

## Example

This invocation of `marlowe-cli contract validator` computes the Marlowe
application Plutus script [contract-3.plutus](contract-3.plutus) for a
particular roles currency.

</div>

<div class="cell code" execution_count="14">

``` bash
marlowe-cli contract validator --testnet-magic 1566 \
                               --out-file contract-3.plutus \
                               --print-stats
```

<div class="output stream stdout">

    addr_test1wrv0vwr4megau50ujjwsktvajmsu6dzza2rnalufd3husaqs9v6rv

    Validator size: 12668
    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 18653100, exBudgetMemory = ExMemory 81200}

</div>

</div>

<div class="cell code" execution_count="15">

``` bash
head -c 1000 contract-3.plutus
```

<div class="output stream stdout">

    {
        "type": "PlutusScriptV2",
        "description": "",
        "cborHex": "59317c593179010000332323233223232323322323322323232323232323232323232323232323232323232323232323232323232323232332233223232323232332232323232323232323232323232332233223232323232323232323232323232323232323232323232323232323232323232323232323232323233223232323232323232323232323232323223222232325335333222350032232322350062323232323223353235001223500223223355335333573466e2000400c25404250044c0c0c8488c00400ccd542040400c00454cd4ccd5cd19b88001501009501094011303033221223300200400350103355081010030011332212233001004003335508101500200135011222233330840100400300250062302f122222300200622533335333333305309601002001010006509f01509f01130301222220051303012222200313030122222004222221533500513333034004003002001153333335015221303703813501822225335333355307112001505e220a1010040a0011303903a133330380080070060052221303803922213038039222221303a03b2221303803915335333573466e24005403824c0424804540384004cc8848cc00400c0094cd4c8d400488d4008894ccd

</div>

</div>

<div class="cell markdown">

## Marlowe

</div>

<div class="cell code" execution_count="16">

``` bash
marlowe-cli contract marlowe --help
```

<div class="output stream stdout">

    Usage: marlowe-cli contract marlowe 
             --testnet-magic INTEGER [--stake-address ADDRESS] 
             [--roles-currency CURRENCY_SYMBOL] 
             [--protocol-version PROTOCOL_VERSION] --contract-file CONTRACT_FILE
             --state-file STATE_FILE [--input-file INPUT_FILE] 
             [--out-file OUTPUT_FILE] [--print-stats]

      Export a Marlowe contract to a JSON file.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles, if any.
      --protocol-version PROTOCOL_VERSION
                               Protocol version: [alonzo|vasil]
      --contract-file CONTRACT_FILE
                               JSON input file for the contract.
      --state-file STATE_FILE  JSON input file for the contract state.
      --input-file INPUT_FILE  JSON input file for redeemer inputs.
      --out-file OUTPUT_FILE   JSON output file for contract.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli contract marlowe` takes the contract
file [contract-2.contract](contract-2.contract), the state file
[contract-2.state](contract-2.state), and the Marlowe input file
[contract-2.input](contract-2.input) as inputs and outputs a file
bundling Plutus validator, datum, and redeemer information for the
corresponding Marlowe transaction.

</div>

<div class="cell code" execution_count="17">

``` bash
marlowe-cli contract marlowe --testnet-magic 2 \
                             --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f \
                             --contract-file contract-2.contract \
                             --state-file contract-2.state \
                             --input-file contract-2.input \
                             --out-file contract-2.marlowe \
                             --print-stats
```

<div class="output stream stdout">


    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 18653100, exBudgetMemory = ExMemory 81200}
    Validator size: 12668
    Datum size: 370
    Redeemer size: 119
    Total size: 13157

</div>

</div>

<div class="cell code" execution_count="18">

``` bash
jq 'to_entries[] | .key' contract-2.marlowe
```

<div class="output stream stdout">

    "datum"
    "redeemer"
    "validator"

</div>

</div>
