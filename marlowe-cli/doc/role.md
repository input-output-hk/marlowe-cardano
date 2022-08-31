---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe CLI `role` Subcommands

The `marlowe-cli role` commands provide low-level access to the Plutus
mechanics of Marlowe payouts: script address, validator, datum, and
redeemer.

## Contents

-   [Address](#address)
-   [Datum](#datum)
-   [Redeemer](#redeemer)
-   [Validator](#validator)

## Available Commands

``` bash
marlowe-cli role --help
```

    Usage: marlowe-cli role COMMAND

      Export role address, validator, datum, or redeemer.

    Available options:
      -h,--help                Show this help text

    Low-level commands for exporting Marlowe role information:
      address                  Print a role validator address.
      datum                    Export a role datum to a JSON file.
      redeemer                 Export a role redeemer to a JSON file.
      validator                Export a role validator to a JSON file.

## Address

``` bash
marlowe-cli role address --help
```

    Usage: marlowe-cli role address --testnet-magic INTEGER 
                                    [--stake-address ADDRESS]
                                    --roles-currency CURRENCY_SYMBOL

      Print a role validator address.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles.
      -h,--help                Show this help text

### Example

Compute the Marlowe payout script address for a particular roles
currency.

``` bash
marlowe-cli role address --testnet-magic 1566 \
                         --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f
```

    addr_test1wrjp95v0mksu8g9klzg06nmuw3wm9amtw7nf4q2gkdcmckq0fcv9s

## Datum

``` bash
marlowe-cli role datum --help
```

    Usage: marlowe-cli role datum --role-name TOKEN_NAME [--out-file DATUM_FILE] 
                                  [--print-stats]

      Export a role datum to a JSON file.

    Available options:
      --role-name TOKEN_NAME   The role name for the datum.
      --out-file DATUM_FILE    JSON output file for datum.
      --print-stats            Print statistics.
      -h,--help                Show this help text

### Example

This invocation of `marlowe-cli role datum` takes the role name as input
and it outputs the corresponding Plutus datum file
[role-1.datum](role-1.datum).

``` bash
marlowe-cli role datum --role-name 'Christopher Marlowe' \
                       --out-file role-1.datum \
                       --print-stats
```

    b57430ad5004b26cc0fc9ab7345450f864c24c6aeba8083869e62af7b27c38f9

    Datum size: 20

``` bash
json2yaml role-1.datum
```

    bytes: 4368726973746f70686572204d61726c6f7765

## Redeemer

``` bash
marlowe-cli role redeemer --help
```

    Usage: marlowe-cli role redeemer [--out-file OUTPUT_FILE] [--print-stats]

      Export a role redeemer to a JSON file.

    Available options:
      --out-file OUTPUT_FILE   JSON output file for redeemer.
      --print-stats            Print statistics.
      -h,--help                Show this help text

### Example

This invocation of `marlowe-cli role redeemer` outputs the corresponding
Plutus role-payout redeemer [role-1.redeemer](role-1.redeemer).

``` bash
marlowe-cli role redeemer --out-file role-1.redeemer \
                          --print-stats
```


    Redeemer size: 3

``` bash
json2yaml role-1.redeemer
```

    constructor: 0
    fields: []

## Validator

``` bash
marlowe-cli role validator --help
```

    Usage: marlowe-cli role validator --testnet-magic INTEGER 
                                      [--stake-address ADDRESS]
                                      --roles-currency CURRENCY_SYMBOL 
                                      [--out-file OUTPUT_FILE] [--print-hash] 
                                      [--print-stats]

      Export a role validator to a JSON file.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles.
      --out-file OUTPUT_FILE   JSON output file for validator.
      --print-hash             Print validator hash.
      --print-stats            Print statistics.
      -h,--help                Show this help text

### Example

This invocation of `marlowe-cli role validator` computes the Marlowe
payout Plutus script [role-1.plutus](role-1.plutus) for a particular
roles currency.

``` bash
marlowe-cli role validator --testnet-magic 1566 \
                           --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f \
                           --out-file role-1.plutus \
                           --print-stats
```

    addr_test1wrjp95v0mksu8g9klzg06nmuw3wm9amtw7nf4q2gkdcmckq0fcv9s

    Validator size: 2469
    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 4317185, exBudgetMemory = ExMemory 14600}

``` bash
head -c 1000 role-1.plutus
```

    {
        "type": "PlutusScriptV1",
        "description": "",
        "cborHex": "5909a55909a20100003323233223232332232323322323232323232323232323232323232323232322222323253353232323232333573466e24c8c010004c8d40048888888888ccd54c0b448004cd40c088cd54c0b8480048d400488cd540b4008cd54c0c4480048d400488cd540c0008ccd40048cc0f52000001223303e00200123303d00148000004cd54c0b8480048d400488cd540b4008ccd40048cd54c0c8480048d400488cd540c4008d540d000400488ccd5540b80e00080048cd54c0c8480048d400488cd540c4008d540cc004004ccd5540a40cc00800540bc8d400488d4004888008028d40048800920000270283333573466e1cd55cea8032400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40608c8c8cccd5cd19b8735573aa004900011991091980080180118109aba15002301d357426ae8940088c98d4cd5ce01581501481409aab9e5001137540026ae854028cd4060064d5d0a804999aa80dbae501a35742a010666aa036eb94068d5d0a80399a80c0109aba15006335018335502402275a6ae854014c8c8c8cccd5cd19b8735573aa004900011991
