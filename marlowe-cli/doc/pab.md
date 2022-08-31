---
date: 18 June 2022
version: marlowe-cli 0.0.5.0
---

# Marlowe CLI Backend Subcommands

These commands interact with Marlowe\'s Plutus Application Backend (PAB)
instance. See [the PAB example](pab-tutorial.md) for a tutorial on using
these commands.

## Contents

-   [App](#app)
-   [Create](#create)
-   [Apply-inputs](#apply-inputs)
-   [Redeem](#redeem)
-   [Follower](#follower)
-   [Follow](#follow)
-   [Companion](#companion)
-   [Stop](#stop)

## Available Commands

``` bash
marlowe-cli pab --help
```

    Usage: marlowe-cli pab COMMAND

      Run a contract via the PAB.

    Available options:
      -h,--help                Show this help text

    Commands for running contracts on the PAB:
      app                      Start the Marlowe application contract.
      create                   Create a Marlowe contract.
      apply-inputs             Apply inputs to a Marlowe contract.
      redeem                   Redeem funds from a Marlowe contract.
      follower                 Start the Marlowe follower contract.
      follow                   Follow a Marlowe contract.
      companion                Start the Marlowe companion contract.
      stop                     Stop a Marlowe contract.

## App

``` bash
marlowe-cli pab app --help
```

    Usage: marlowe-cli pab app --pab-url URL --wallet WALLET_ID [--loop] 
                               [--out-params-file PARAMS_FILE] 
                               [--out-instance-file INSTANCE_FILE]

      Start the Marlowe application contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --wallet WALLET_ID       Wallet ID for the contract.
      --loop                   Whether to listen to PAB messages until the contract
                               stops.
      --out-params-file PARAMS_FILE
                               Output file for the Marlowe parameters.
      --out-instance-file INSTANCE_FILE
                               Output file for the instance ID.
      -h,--help                Show this help text

## Create

``` bash
marlowe-cli pab create --help
```

    Usage: marlowe-cli pab create --pab-url URL --instance-file INSTANCE_FILE
                                  --contract-file CONTRACT_FILE 
                                  [--owner ROLE=ADDRESS]

      Create a Marlowe contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --instance-file INSTANCE_FILE
                               Input file for the instance ID.
      --contract-file CONTRACT_FILE
                               JSON input file for the contract.
      --owner ROLE=ADDRESS     The role name and its address.
      -h,--help                Show this help text

## Apply-Inputs

``` bash
marlowe-cli pab apply-inputs --help
```

    Usage: marlowe-cli pab apply-inputs 
             --pab-url URL --instance-file INSTANCE_FILE --params-file PARAMS_FILE 
             [--deposit-account PARTY --deposit-party PARTY [--deposit-token TOKEN]
               --deposit-amount INTEGER |
               --choice-name NAME --choice-party PARTY --choice-number INTEGER | 
               --notify] --invalid-before POSIX_TIME --invalid-hereafter POSIX_TIME

      Apply inputs to a Marlowe contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --instance-file INSTANCE_FILE
                               Input file for the instance ID.
      --params-file PARAMS_FILE
                               JSON input file for the Marlowe parameters.
      --deposit-account PARTY  The account for the deposit.
      --deposit-party PARTY    The party making the deposit.
      --deposit-token TOKEN    The token being deposited, if not Ada.
      --deposit-amount INTEGER The amount of token being deposited.
      --choice-name NAME       The name of the choice made.
      --choice-party PARTY     The party making the choice.
      --choice-number INTEGER  The number chosen.
      --notify                 Notify the contract.
      --invalid-before POSIX_TIME
                               Minimum time for the input, in POSIX milliseconds.
      --invalid-hereafter POSIX_TIME
                               Maximum time for the input, in POSIX milliseconds.
      -h,--help                Show this help text

## Redeem

``` bash
marlowe-cli pab redeem --help
```

    Usage: marlowe-cli pab redeem --pab-url URL --instance-file INSTANCE_FILE
                                  --params-file PARAMS_FILE --owner ROLE=ADDRESS

      Redeem funds from a Marlowe contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --instance-file INSTANCE_FILE
                               Input file for the instance ID.
      --params-file PARAMS_FILE
                               JSON input file for the Marlowe parameters.
      --owner ROLE=ADDRESS     The role name and its address.
      -h,--help                Show this help text

## Follower

``` bash
marlowe-cli pab follower --help
```

    Usage: marlowe-cli pab follower --pab-url URL --wallet WALLET_ID [--loop] 
                                    [--out-file INSTANCE_FILE]

      Start the Marlowe follower contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --wallet WALLET_ID       Wallet ID for the contract.
      --loop                   Whether to listen to PAB messages until the contract
                               stops.
      --out-file INSTANCE_FILE Output file for the instance ID.
      -h,--help                Show this help text

## Follow

``` bash
marlowe-cli pab follow --help
```

    Usage: marlowe-cli pab follow --pab-url URL --instance-file INSTANCE_FILE
                                  --params-file PARAMS_FILE

      Follow a Marlowe contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --instance-file INSTANCE_FILE
                               Input file for the instance ID.
      --params-file PARAMS_FILE
                               JSON input file for the Marlowe parameters.
      -h,--help                Show this help text

## Companion

``` bash
marlowe-cli pab companion --help
```

    Usage: marlowe-cli pab companion --pab-url URL --wallet WALLET_ID [--loop] 
                                     [--out-file INSTANCE_FILE]

      Start the Marlowe companion contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --wallet WALLET_ID       Wallet ID for the contract.
      --loop                   Whether to listen to PAB messages until the contract
                               stops.
      --out-file INSTANCE_FILE Output file for the instance ID.
      -h,--help                Show this help text

## Stop

``` bash
marlowe-cli pab stop --help
```

    Usage: marlowe-cli pab stop --pab-url URL --instance-file INSTANCE_FILE

      Stop a Marlowe contract.

    Available options:
      --pab-url URL            URL for the Marlowe PAB.
      --instance-file INSTANCE_FILE
                               Input file for the instance ID.
      -h,--help                Show this help text
