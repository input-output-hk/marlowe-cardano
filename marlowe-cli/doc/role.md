---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Marlowe CLI `role` Subcommands

The `marlowe-cli role` commands provide low-level access to the Plutus
mechanics of Marlowe payouts: script address, validator, datum, and
redeemer.

</div>

<div class="cell markdown">

## Contents

-   [Address](#address)
-   [Datum](#datum)
-   [Redeemer](#redeemer)
-   [Validator](#validator)

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli role --help
```

<div class="output stream stdout">

    Usage: marlowe-cli role COMMAND

      Export role address, validator, datum, or redeemer.

    Available options:
      -h,--help                Show this help text

    Low-level commands for exporting Marlowe role information:
      address                  Print a role validator address.
      datum                    Export a role datum to a JSON file.
      redeemer                 Export a role redeemer to a JSON file.
      validator                Export a role validator to a JSON file.

</div>

</div>

<div class="cell markdown">

## Address

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli role address --help
```

<div class="output stream stdout">

    Usage: marlowe-cli role address --testnet-magic INTEGER 
                                    [--stake-address ADDRESS]

      Print a role validator address.

    Available options:
      --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
                               environment variable's value.
      --stake-address ADDRESS  Stake address, if any.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Compute the Marlowe payout script address for a particular roles
currency.

</div>

<div class="cell code" execution_count="3">

``` bash
marlowe-cli role address --testnet-magic 1566
```

<div class="output stream stdout">

    addr_test1wpkmnxz4aylglk57j9mf90r5dj0kmde7n6frfgatam4fw8qyrah58

</div>

</div>

<div class="cell markdown">

## Datum

</div>

<div class="cell code" execution_count="4">

``` bash
marlowe-cli role datum --help
```

<div class="output stream stdout">

    Usage: marlowe-cli role datum --roles-currency CURRENCY_SYMBOL
                                  --role-name TOKEN_NAME [--out-file DATUM_FILE] 
                                  [--print-stats]

      Export a role datum to a JSON file.

    Available options:
      --roles-currency CURRENCY_SYMBOL
                               The currency symbol for roles.
      --role-name TOKEN_NAME   The role name for the datum.
      --out-file DATUM_FILE    JSON output file for datum.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli role datum` takes the role name as input
and it outputs the corresponding Plutus datum file
[role-1.datum](role-1.datum).

</div>

<div class="cell code" execution_count="5">

``` bash
marlowe-cli role datum --roles-currency d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f \
                       --role-name 'Christopher Marlowe' \
                       --out-file role-1.datum \
                       --print-stats
```

<div class="output stream stdout">

    aa3d5af65e9238031f51a84e96280d59d4b1dbcb4fc7c4b934c2a7348715a8d8

    Datum size: 56

</div>

</div>

<div class="cell code" execution_count="6">

``` bash
json2yaml role-1.datum
```

<div class="output stream stdout">

    constructor: 0
    fields:
    - bytes: d0e2ebf0a20c10d870d447854d178b2b0928ae1ce8661a01acfc662f
    - bytes: 4368726973746f70686572204d61726c6f7765

</div>

</div>

<div class="cell markdown">

## Redeemer

</div>

<div class="cell code" execution_count="7">

``` bash
marlowe-cli role redeemer --help
```

<div class="output stream stdout">

    Usage: marlowe-cli role redeemer [--out-file OUTPUT_FILE] [--print-stats]

      Export a role redeemer to a JSON file.

    Available options:
      --out-file OUTPUT_FILE   JSON output file for redeemer.
      --print-stats            Print statistics.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli role redeemer` outputs the corresponding
Plutus role-payout redeemer [role-1.redeemer](role-1.redeemer).

</div>

<div class="cell code" execution_count="8">

``` bash
marlowe-cli role redeemer --out-file role-1.redeemer \
                          --print-stats
```

<div class="output stream stdout">


    Redeemer size: 5

</div>

</div>

<div class="cell code" execution_count="9">

``` bash
json2yaml role-1.redeemer
```

<div class="output stream stdout">

    constructor: 0
    fields: []

</div>

</div>

<div class="cell markdown">

## Validator

</div>

<div class="cell code" execution_count="10">

``` bash
marlowe-cli role validator --help
```

<div class="output stream stdout">

    Usage: marlowe-cli role validator --testnet-magic INTEGER 
                                      [--stake-address ADDRESS] 
                                      [--protocol-version PROTOCOL_VERSION] 
                                      [--out-file OUTPUT_FILE] [--print-hash] 
                                      [--print-stats]

      Export a role validator to a JSON file.

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

### Example

This invocation of `marlowe-cli role validator` computes the Marlowe
payout Plutus script [role-1.plutus](role-1.plutus) for a particular
roles currency.

</div>

<div class="cell code" execution_count="11">

``` bash
marlowe-cli role validator --testnet-magic 1566 \
                           --out-file role-1.plutus \
                           --print-stats
```

<div class="output stream stdout">

    addr_test1wpkmnxz4aylglk57j9mf90r5dj0kmde7n6frfgatam4fw8qyrah58

    Validator size: 2627
    Bare-validator cost: ExBudget {exBudgetCPU = ExCPU 3680100, exBudgetMemory = ExMemory 16100}

</div>

</div>

<div class="cell code" execution_count="12">

``` bash
head -c 1000 role-1.plutus
```

<div class="output stream stdout">

    {
        "type": "PlutusScriptV2",
        "description": "",
        "cborHex": "590a43590a40010000332323232323232323232323232323322323232323222223232533533300632323333573466e1cd55cea80124000466aa0226eb8d5d0a8011bae357426ae8940088c98c8070cd5ce00e80e00d09aab9e50011375400a6666ae68cdc39aab9d37540089000100d11931900d19ab9c01b01a0183333573466e1cd55cea80124000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd405805cd5d0a80619a80b00b9aba1500b33501601835742a014666aa034eb94064d5d0a804999aa80d3ae501935742a01066a02c0446ae85401cccd5406808dd69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40b5d69aba15002302e357426ae8940088c98c80c0cd5ce01881801709aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a816bad35742a004605c6ae84d5d1280111931901819ab9c03103002e135573ca00226ea8004d5d09aba2500223263202c33573805a05805426aae7940044d

</div>

</div>
