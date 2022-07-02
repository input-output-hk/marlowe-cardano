---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe CLI `template` Subcommands

The `marlowe-cli template` command generates contract and state files
for example Marlowe contracts.

## Contents

-   [Simple](#simple)
-   [Swap](#swap)
-   [Zero-coupon bond](#zero-coupon-bond)
-   [Escrow](#escrow)
-   [Covered call](#covered-call)

## Available Commands

``` bash
marlowe-cli template --help
```

    Usage: marlowe-cli template COMMAND

      Create a contract from a template.

    Available options:
      -h,--help                Show this help text

    Commands for creating Marlowe contracts from templates:
      escrow                   Create an escrow contract.
      simple                   Create a simple example contract.
      swap                     Create a swap contract.
      zcb                      Create a zero-coupon bond.
      coveredCall              Create a covered call Option.

## Simple

``` bash
marlowe-cli template simple --help
```

    Usage: marlowe-cli template simple --bystander PARTY --minimum-ada INTEGER
                                       --party PARTY --deposit-lovelace INTEGER
                                       --withdrawal-lovelace INTEGER
                                       --timeout POSIX_TIME
                                       --out-contract-file CONTRACT_FILE
                                       --out-state-file STATE_FILE

      Create a simple example contract.

    Available options:
      --bystander PARTY        The party providing the min-ADA.
      --minimum-ada INTEGER    Lovelace in the initial state.
      --party PARTY            The party.
      --deposit-lovelace INTEGER
                               Lovelace in the deposit.
      --withdrawal-lovelace INTEGER
                               Lovelace in the withdrawal.
      --timeout POSIX_TIME     The timeout, in POSIX milliseconds.
      --out-contract-file CONTRACT_FILE
                               JSON output file for the contract.
      --out-state-file STATE_FILE
                               JSON output file for the contract's state.
      -h,--help                Show this help text

### Example

Create a simple contract as [simple-1.contract](simple-1.contract) with
initial state [simple-1.state](simple-1.state)

``` bash
marlowe-cli template simple --minimum-ada 3000000 \
                            --deposit-lovelace 12000000 \
                            --withdrawal-lovelace 5000000 \
                            --timeout 1655663505000 \
                            --party PK=84117ffa443e598527726f6509078114ecd1f2c031e71dd5d239d504 \
                            --bystander PK=e107572dc2e0f7dadaf87c1a4f55d6b5b5456108c1de8f6d75ba0d15 \
                            --out-contract-file simple-1.contract \
                            --out-state-file simple-1.state
```

## Swap

``` bash
marlowe-cli template swap --help
```

    Usage: marlowe-cli template swap --minimum-ada INTEGER --a-party PARTY
                                     --a-token TOKEN --a-amount INTEGER
                                     --a-timeout POSIX_TIME --b-party PARTY
                                     --b-token TOKEN --b-amount INTEGER
                                     --b-timeout POSIX_TIME
                                     --out-contract-file CONTRACT_FILE
                                     --out-state-file STATE_FILE

      Create a swap contract.

    Available options:
      --minimum-ada INTEGER    Lovelace that the first party contributes to the
                               initial state.
      --a-party PARTY          The first party.
      --a-token TOKEN          The first party's token.
      --a-amount INTEGER       The amount of the first party's token.
      --a-timeout POSIX_TIME   The timeout for the first party's deposit, in POSIX
                               milliseconds.
      --b-party PARTY          The second party.
      --b-token TOKEN          The second party's token.
      --b-amount INTEGER       The amount of the second party's token.
      --b-timeout POSIX_TIME   The timeout for the second party's deposit, in POSIX
                               milliseconds.
      --out-contract-file CONTRACT_FILE
                               JSON output file for the contract.
      --out-state-file STATE_FILE
                               JSON output file for the contract's state.
      -h,--help                Show this help text

### Example

Create a swap contract [swap-1.contract](swap-1.contract) with initial
state [swap-1.state](swap-1.state).

``` bash
marlowe-cli template swap --minimum-ada 3000000 \
                          --a-party PK=1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf \
                          --a-token d537d311e915c4d815607ead0bcd5880807895871ce9be7c1eaf72a6.Globe \
                          --a-amount 300 \
                          --a-timeout 1655621320000 \
                          --b-party PK=fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff \
                          --b-token  ffd80473e79426fef5de6c124369bb96a67a8b5814211009354e311e.Swan\
                          --b-amount 500 \
                          --b-timeout 1655664520000 \
                          --out-contract-file swap-1.contract \
                          --out-state-file swap-1.state
```

## Zero-Coupon Bond

``` bash
marlowe-cli template zcb --help
```

    Usage: marlowe-cli template zcb --minimum-ada INTEGER --lender PARTY
                                    --borrower PARTY --principal INTEGER
                                    --interest INTEGER --lending-deadline POSIX_TIME
                                    --repayment-deadline POSIX_TIME
                                    --out-contract-file CONTRACT_FILE
                                    --out-state-file STATE_FILE

      Create a zero-coupon bond.

    Available options:
      --minimum-ada INTEGER    Lovelace that the lender contributes to the initial
                               state.
      --lender PARTY           The lender.
      --borrower PARTY         The borrower.
      --principal INTEGER      The principal, in lovelace.
      --interest INTEGER       The interest, in lovelace.
      --lending-deadline POSIX_TIME
                               The lending deadline, in POSIX milliseconds.
      --repayment-deadline POSIX_TIME
                               The repayment deadline, in POSIX milliseconds.
      --out-contract-file CONTRACT_FILE
                               JSON output file for the contract.
      --out-state-file STATE_FILE
                               JSON output file for the contract's state.
      -h,--help                Show this help text

### Example

Create a zero-coupon bond contract [zcb-1.contract](zcb-1.contract) with
initial state [zcb-1.state](zcb-1.state).

``` bash
marlowe-cli template zcb --minimum-ada 3000000 \
                         --lender Role=JF \
                         --borrower Role=TM \
                         --principal 100000000 \
                         --interest 5000000 \
                         --lending-deadline 1655621700000 \
                         --repayment-deadline 1655664900000 \
                         --out-contract-file zcb-1.contract \
                         --out-state-file zcb-1.state
```

# Escrow

``` bash
marlowe-cli template escrow --help
```

    Usage: marlowe-cli template escrow --minimum-ada INTEGER --price INTEGER
                                       --seller PARTY --buyer PARTY --mediator PARTY
                                       --payment-deadline POSIX_TIME
                                       --complaint-deadline POSIX_TIME
                                       --dispute-deadline POSIX_TIME
                                       --mediation-deadline POSIX_TIME
                                       --out-contract-file CONTRACT_FILE
                                       --out-state-file STATE_FILE

      Create an escrow contract.

    Available options:
      --minimum-ada INTEGER    Lovelace in the initial state.
      --price INTEGER          The price of the sale, in lovelace.
      --seller PARTY           The seller.
      --buyer PARTY            The buyer.
      --mediator PARTY         The mediator.
      --payment-deadline POSIX_TIME
                               The deadline for the buyer to pay, in POSIX
                               milliseconds.
      --complaint-deadline POSIX_TIME
                               The deadline for the buyer to complain, in POSIX
                               milliseconds.
      --dispute-deadline POSIX_TIME
                               The deadline for the seller to dispute a complaint,
                               in POSIX milliseconds.
      --mediation-deadline POSIX_TIME
                               The deadline for the mediator to decide, in POSIX
                               milliseconds.
      --out-contract-file CONTRACT_FILE
                               JSON output file for the contract.
      --out-state-file STATE_FILE
                               JSON output file for the contract's state.
      -h,--help                Show this help text

### Example

Create an escrow contract [escrow-1.contract](escrow-1.contract) with
initial state [escrow-1.state](escrow-1.state).

``` bash
marlowe-cli template escrow --minimum-ada 3000000 \
                            --price 256000000 \
                            --seller Role=FB \
                            --buyer Role=TM \
                            --mediator Role=CM \
                            --payment-deadline 1655664900000 \
                            --complaint-deadline 1655751300000 \
                            --dispute-deadline 1655837700000 \
                            --mediation-deadline 1655924100000 \
                            --out-contract-file escrow-1.contract \
                            --out-state-file escrow-1.state
```

## Covered Call

``` bash
marlowe-cli template coveredCall --help
```

    Usage: marlowe-cli template coveredCall 
             --minimum-ada INTEGER --issuer PARTY --counter-party PARTY
             --currency TOKEN --underlying TOKEN --strike INTEGER --amount INTEGER
             --issue-date POSIX_TIME --maturity-date POSIX_TIME
             --settlement-date POSIX_TIME --out-contract-file CONTRACT_FILE
             --out-state-file STATE_FILE

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
      --issue-date POSIX_TIME  The issue date, in POSIX milliseconds.
      --maturity-date POSIX_TIME
                               The maturity date, in POSIX milliseconds.
      --settlement-date POSIX_TIME
                               The settlement date, in POSIX milliseconds.
      --out-contract-file CONTRACT_FILE
                               JSON output file for the contract.
      --out-state-file STATE_FILE
                               JSON output file for the contract's state.
      -h,--help                Show this help text

### Example

Create a covered call contract [covered-1.contract](covered-1.contract)
with initial state [covered-1.state](covered-1.state).

``` bash
marlowe-cli template coveredCall --minimum-ada 3000000 \
                                 --issuer PK=1cb51be3ab4e4b540e86bd4c9be02682db8150f69c3cded2422cc1bf \
                                 --counter-party PK=fd37884bbd044c72e5f29de1b777a9c1c1d531773535cd5b55e2f6ff \
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
