---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Marlowe CLI `input` Subcommands

The `marlowe-cli input` commands generate files for Marlowe inputs.

</div>

<div class="cell markdown">

## Contents

-   [Choose](#choose)
-   [Deposit](#deposit)
-   [Notify](#notify)

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli input --help
```

<div class="output stream stdout">

    Usage: marlowe-cli input COMMAND

      Create inputs to a contract.

    Available options:
      -h,--help                Show this help text

    Low-level commands for creating inputs to a contract:
      choose                   Create Marlowe input for a choice.
      deposit                  Create Marlowe input for a deposit.
      notify                   Create Marlowe input for a notification.

</div>

</div>

<div class="cell markdown">

## Choose

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli input choose --help
```

<div class="output stream stdout">

    Usage: marlowe-cli input choose --choice-name NAME --choice-party PARTY
                                    --choice-number INTEGER [--out-file OUTPUT_FILE]

      Create Marlowe input for a choice.

    Available options:
      --choice-name NAME       The name of the choice made.
      --choice-party PARTY     The party making the choice.
      --choice-number INTEGER  The number chosen.
      --out-file OUTPUT_FILE   JSON output file for contract input.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli input choose` has Christopher Marlowe
choose the "ADA/GBP Price" with value 1,000,000. It creates the Marlowe
input file [input-1.input](input-1.input).

</div>

<div class="cell code" execution_count="3">

``` bash
marlowe-cli input choose --choice-party 'Christopher Marlowe' \
                         --choice-name 'ADA/GBP Price' \
                         --choice-number 1000000 \
                         --out-file input-1.input
```

</div>

<div class="cell code" execution_count="4">

``` bash
json2yaml input-1.input
```

<div class="output stream stdout">

    for_choice_id:
      choice_name: ADA/GBP Price
      choice_owner:
        role_token: Christopher Marlowe
    input_that_chooses_num: 1000000

</div>

</div>

<div class="cell markdown">

## Deposit

</div>

<div class="cell code" execution_count="5">

``` bash
marlowe-cli input deposit --help
```

<div class="output stream stdout">

    Usage: marlowe-cli input deposit --deposit-account PARTY --deposit-party PARTY 
                                     [--deposit-token TOKEN]
                                     --deposit-amount INTEGER 
                                     [--out-file OUTPUT_FILE]

      Create Marlowe input for a deposit.

    Available options:
      --deposit-account PARTY  The account for the deposit.
      --deposit-party PARTY    The party making the deposit.
      --deposit-token TOKEN    The token being deposited, if not Ada.
      --deposit-amount INTEGER The amount of token being deposited.
      --out-file OUTPUT_FILE   JSON output file for contract input.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli input deposit` has Christopher Marlowe
deposit 1000 ADA into the account of Francis Bacon. It creates the
Marlowe input file [input-2.input](input-2.input).

</div>

<div class="cell code" execution_count="6">

``` bash
marlowe-cli input deposit --deposit-party 'Christopher Marlowe' \
                          --deposit-amount 1000000000 \
                          --deposit-account 'Francis Bacon' \
                          --out-file input-2.input
```

</div>

<div class="cell code" execution_count="7">

``` bash
json2yaml input-2.input
```

<div class="output stream stdout">

    input_from_party:
      role_token: Christopher Marlowe
    into_account:
      role_token: Francis Bacon
    of_token:
      currency_symbol: ''
      token_name: ''
    that_deposits: 1000000000

</div>

</div>

<div class="cell markdown">

## Notify

</div>

<div class="cell code" execution_count="8">

``` bash
marlowe-cli input notify --help
```

<div class="output stream stdout">

    Usage: marlowe-cli input notify [--out-file OUTPUT_FILE]

      Create Marlowe input for a notification.

    Available options:
      --out-file OUTPUT_FILE   JSON output file for contract input.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

This invocation of `marlowe-cli input notify` simply notifices a
contract to progress. It creates the Marlowe input file
[input-3.input](input-3.input).

</div>

<div class="cell code" execution_count="9">

``` bash
marlowe-cli input notify --out-file input-3.input
```

</div>

<div class="cell code" execution_count="10">

``` bash
json2yaml input-3.input
```

<div class="output stream stdout">

    input_notify
    ...

</div>

</div>
