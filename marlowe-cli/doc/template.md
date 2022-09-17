---
date: 17 September 2022
version: marlowe-cli 0.0.8.0
---

<div class="cell markdown">

# Marlowe CLI `template` Subcommands

The `marlowe-cli template` command generates contract and state files
for example Marlowe contracts.

</div>

<div class="cell markdown">

## Contents

-   [Simple](#simple)
-   [Swap](#swap)
-   [Zero-coupon bond](#zero-coupon-bond)
-   [Escrow](#escrow)
-   [Covered call](#covered-call)

</div>

<div class="cell markdown">

## Available Commands

</div>

<div class="cell code" execution_count="1">

``` bash
marlowe-cli template --help
```

<div class="output stream stdout">

    Usage: marlowe-cli template COMMAND --out-contract-file CONTRACT_FILE
                                --out-state-file STATE_FILE

      Create a contract from a template.

    Available options:
      --out-contract-file CONTRACT_FILE
                               JSON output file for the contract.
      --out-state-file STATE_FILE
                               JSON output file for the contract's state.
      -h,--help                Show this help text

    Commands for creating Marlowe contracts from templates:
      escrow                   Create an escrow contract.
      simple                   Create a simple example contract.
      swap                     Create a swap contract.
      zcb                      Create a zero-coupon bond.
      coveredCall              Create a covered call Option.
      actus                    Create an Actus contract.

</div>

</div>

<div class="cell markdown">

## Simple

</div>

<div class="cell code" execution_count="2">

``` bash
marlowe-cli template simple --help
```

<div class="output stream stdout">

    Usage: marlowe-cli template simple --bystander PARTY --minimum-ada INTEGER
                                       --party PARTY --deposit-lovelace INTEGER
                                       --withdrawal-lovelace INTEGER
                                       --timeout TIMEOUT

      Create a simple example contract.

    Available options:
      --bystander PARTY        The party providing the min-ADA.
      --minimum-ada INTEGER    Lovelace in the initial state.
      --party PARTY            The party.
      --deposit-lovelace INTEGER
                               Lovelace in the deposit.
      --withdrawal-lovelace INTEGER
                               Lovelace in the withdrawal.
      --timeout TIMEOUT        The timeout. POSIX milliseconds or duration:
                               `INTEGER[s|m|d|w|h]`.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Create a simple contract as [simple-1.contract](simple-1.contract) with
initial state [simple-1.state](simple-1.state)

</div>

<div class="cell code" execution_count="3">

``` bash
marlowe-cli template simple --minimum-ada 3000000 \
                            --deposit-lovelace 12000000 \
                            --withdrawal-lovelace 5000000 \
                            --timeout 1655663505000 \
                            --party addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
                            --bystander addr_test1qp2l7afky3eqfkrht5f3qgy7x2yek5dejcnpnuqlwywz9twr7cz4mu6gh005gdck67p7y9d8s8zsfgjkcdy75mrjh6jqp8jwfw \
                            --out-contract-file simple-1.contract \
                            --out-state-file simple-1.state
```

</div>

<div class="cell markdown">

## Swap

</div>

<div class="cell code" execution_count="4">

``` bash
marlowe-cli template swap --help
```

<div class="output stream stdout">

    Usage: marlowe-cli template swap --minimum-ada INTEGER --a-party PARTY
                                     --a-token TOKEN --a-amount INTEGER
                                     --a-timeout TIMEOUT --b-party PARTY
                                     --b-token TOKEN --b-amount INTEGER
                                     --b-timeout TIMEOUT

      Create a swap contract.

    Available options:
      --minimum-ada INTEGER    Lovelace that the first party contributes to the
                               initial state.
      --a-party PARTY          The first party.
      --a-token TOKEN          The first party's token.
      --a-amount INTEGER       The amount of the first party's token.
      --a-timeout TIMEOUT      The timeout for the first party's deposit. POSIX
                               milliseconds or duration: `INTEGER[s|m|d|w|h]`.
      --b-party PARTY          The second party.
      --b-token TOKEN          The second party's token.
      --b-amount INTEGER       The amount of the second party's token.
      --b-timeout TIMEOUT      The timeout for the second party's deposit. POSIX
                               milliseconds or duration: `INTEGER[s|m|d|w|h]`.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Create a swap contract [swap-1.contract](swap-1.contract) with initial
state [swap-1.state](swap-1.state).

</div>

<div class="cell code" execution_count="5">

``` bash
marlowe-cli template swap --minimum-ada 3000000 \
                          --a-party addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
                          --a-token d537d311e915c4d815607ead0bcd5880807895871ce9be7c1eaf72a6.Globe \
                          --a-amount 300 \
                          --a-timeout 1655621320000 \
                          --b-party addr_test1qp2l7afky3eqfkrht5f3qgy7x2yek5dejcnpnuqlwywz9twr7cz4mu6gh005gdck67p7y9d8s8zsfgjkcdy75mrjh6jqp8jwfw \
                          --b-token  ffd80473e79426fef5de6c124369bb96a67a8b5814211009354e311e.Swan\
                          --b-amount 500 \
                          --b-timeout 1655664520000 \
                          --out-contract-file swap-1.contract \
                          --out-state-file swap-1.state
```

</div>

<div class="cell markdown">

## Zero-Coupon Bond

</div>

<div class="cell code" execution_count="6">

``` bash
marlowe-cli template zcb --help
```

<div class="output stream stdout">

    Usage: marlowe-cli template zcb --minimum-ada INTEGER --lender PARTY
                                    --borrower PARTY --principal INTEGER
                                    --interest INTEGER --lending-deadline TIMEOUT
                                    --repayment-deadline TIMEOUT

      Create a zero-coupon bond.

    Available options:
      --minimum-ada INTEGER    Lovelace that the lender contributes to the initial
                               state.
      --lender PARTY           The lender.
      --borrower PARTY         The borrower.
      --principal INTEGER      The principal, in lovelace.
      --interest INTEGER       The interest, in lovelace.
      --lending-deadline TIMEOUT
                               The lending deadline. POSIX milliseconds or duration:
                               `INTEGER[s|m|d|w|h]`.
      --repayment-deadline TIMEOUT
                               The repayment deadline. POSIX milliseconds or
                               duration: `INTEGER[s|m|d|w|h]`.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Create a zero-coupon bond contract [zcb-1.contract](zcb-1.contract) with
initial state [zcb-1.state](zcb-1.state).

</div>

<div class="cell code" execution_count="7">

``` bash
marlowe-cli template zcb --minimum-ada 3000000 \
                         --lender JF \
                         --borrower TM \
                         --principal 100000000 \
                         --interest 5000000 \
                         --lending-deadline 1655621700000 \
                         --repayment-deadline 1655664900000 \
                         --out-contract-file zcb-1.contract \
                         --out-state-file zcb-1.state
```

</div>

<div class="cell markdown">

# Escrow

</div>

<div class="cell code" execution_count="8">

``` bash
marlowe-cli template escrow --help
```

<div class="output stream stdout">

    Usage: marlowe-cli template escrow --minimum-ada INTEGER --price INTEGER
                                       --seller PARTY --buyer PARTY --mediator PARTY
                                       --payment-deadline TIMEOUT
                                       --complaint-deadline TIMEOUT
                                       --dispute-deadline TIMEOUT
                                       --mediation-deadline TIMEOUT

      Create an escrow contract.

    Available options:
      --minimum-ada INTEGER    Lovelace in the initial state.
      --price INTEGER          The price of the sale, in lovelace.
      --seller PARTY           The seller.
      --buyer PARTY            The buyer.
      --mediator PARTY         The mediator.
      --payment-deadline TIMEOUT
                               The deadline for the buyer to pay. POSIX milliseconds
                               or duration: `INTEGER[s|m|d|w|h]`.
      --complaint-deadline TIMEOUT
                               The deadline for the buyer to complain. POSIX
                               milliseconds or duration: `INTEGER[s|m|d|w|h]`.
      --dispute-deadline TIMEOUT
                               The deadline for the seller to dispute a complaint.
                               POSIX milliseconds or duration: `INTEGER[s|m|d|w|h]`.
      --mediation-deadline TIMEOUT
                               The deadline for the mediator to decide. POSIX
                               milliseconds or duration: `INTEGER[s|m|d|w|h]`.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Create an escrow contract [escrow-1.contract](escrow-1.contract) with
initial state [escrow-1.state](escrow-1.state).

</div>

<div class="cell code" execution_count="9">

``` bash
marlowe-cli template escrow --minimum-ada 3000000 \
                            --price 256000000 \
                            --seller FB \
                            --buyer TM \
                            --mediator CM \
                            --payment-deadline 1655664900000 \
                            --complaint-deadline 1655751300000 \
                            --dispute-deadline 1655837700000 \
                            --mediation-deadline 1655924100000 \
                            --out-contract-file escrow-1.contract \
                            --out-state-file escrow-1.state
```

</div>

<div class="cell markdown">

## Covered Call

</div>

<div class="cell code" execution_count="10">

``` bash
marlowe-cli template coveredCall --help
```

<div class="output stream stdout">

    Usage: marlowe-cli template coveredCall 
             --minimum-ada INTEGER --issuer PARTY --counter-party PARTY
             --currency TOKEN --underlying TOKEN --strike INTEGER --amount INTEGER
             --issue-date TIMEOUT --maturity-date TIMEOUT --settlement-date TIMEOUT

      Create a covered call Option.

    Available options:
      --minimum-ada INTEGER    Lovelace that the lender contributes to the initial
                               state.
      --issuer PARTY           The issuer.
      --counter-party PARTY    The counter-party.
      --currency TOKEN         The curreny.
      --underlying TOKEN       The underlying asset.
      --strike INTEGER         The strike, in currency.
      --amount INTEGER         The amount of underlying
      --issue-date TIMEOUT     The issue date. POSIX milliseconds or duration:
                               `INTEGER[s|m|d|w|h]`.
      --maturity-date TIMEOUT  The maturity date. POSIX milliseconds or duration:
                               `INTEGER[s|m|d|w|h]`.
      --settlement-date TIMEOUT
                               The settlement date. POSIX milliseconds or duration:
                               `INTEGER[s|m|d|w|h]`.
      -h,--help                Show this help text

</div>

</div>

<div class="cell markdown">

### Example

Create a covered call contract [covered-1.contract](covered-1.contract)
with initial state [covered-1.state](covered-1.state).

</div>

<div class="cell code" execution_count="11">

``` bash
marlowe-cli template coveredCall --minimum-ada 3000000 \
                                 --issuer addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
                                 --counter-party addr_test1qp2l7afky3eqfkrht5f3qgy7x2yek5dejcnpnuqlwywz9twr7cz4mu6gh005gdck67p7y9d8s8zsfgjkcdy75mrjh6jqp8jwfw \
                                 --currency 02332a91ff02a6801a5cc34ad6c4f15a77ac70348f8e305e6ae97659.Swan \
                                 --underlying 48a6526d690a0e5258d6b883507f540cf6e2348596aa03d754753423.Globe \
                                 --strike 500 \
                                 --amount 300 \
                                 --issue-date 1655596500000 \
                                 --maturity-date 1655578500000 \
                                 --settlement-date 1655621700000 \
                                 --out-contract-file covered-1.contract \
                                 --out-state-file covered-1.state
```

</div>
