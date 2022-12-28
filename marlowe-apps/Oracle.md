***WORK IN PROGRESS***


# General-Purpose Oracle for Marlowe Runtime

This oracle watches the blockchain for Marlowe contracts that have a `Choice` action ready for input. The start of the contract remaining on chain must be of the form `When [... (Case (Choice (ChoiceId symbol address) ...)) ...] ...`, where `address` is the address of the oracle and `symbol` is the symbol that the oracle should report a value for (see [the list of data feeds](#data-feeds-available)). If the `When` contract contains several `Case` terms for oracle input, the oracle arbitrarily selects one of them. Note that the `ChosenNum` provided by the oracle is an `Integer` representing the scaled value of the raw real number. For example, a 4.30% annual interest rate might be reported as `4300`. Needless to say, it is *critically important* that the Marlowe contract correctly interpret the integer that the oracle reports to it.

![Block for oracle input in a Marlowe contract](oracle-block.png)


A [video demonstrates the Marlowe oracle](https://youtu.be/n1Mv3I7QoTE).


## Security Considerations

*The security of a Marlowe contract that uses oracle(s) depends upon trust in the oracle(s). For the oracle presented here, this means that the parties to the contract must trust the holder of the signing (private) key for the oracle address and the infrastructure running the oracle.*

The security of an oracle-reliant Marlowe contract might be increased by combining reports from several independent, trusted oracles. The diagram below shows an example of averaging the reports of three oracles. Note that a more secure approach would be to use the [median](https://en.wikipedia.org/wiki/Median) (middle value) of the three reports instead of the [mean](https://en.wikipedia.org/wiki/Mean) (average) of them.

![Averaging three oracles in a Marlowe contract](oracle-averaging.png)


## Data Feeds Available

The `SOFR` data feed provides the [Secured Overnight Financing Rate from the New York Federal Reserve Board](https://www.newyorkfed.org/markets/reference-rates/sofr), *measured in basis points*. Thus a report of `430` corresponds to a 4.30% annual rate.

The `BTCETH`, `BTCEUR`, `BTCGBP`, `BTCJPY`, `BTCUSD`, `ADABTC`, `ADAETH`, `ADAEUR`, `ADAGBP`, `ADAJPY`, `ADAUSD`, `ETHBTC`, `ETHEUR`, `ETHGBP`, `ETHJPY`, and `ETHUSD` data feeds provide cryptocurrency prices from [CoinGecko](https://www.coingecko.com/), measured in parts per hundred million (/ 100,000,000). The first three letters of the symbol are the *base currency* and the last three letters are the *quote currency*. Thus a report of `20970500` for `ADAGBP` corresponds to £0.209705/₳.

One can add data feeds to the oracle by adding a new oracle module (like [`Network.Oracle.Sofr`](oracle/Network/Oracle/Sofr.hs)) modifying [`Network.Oracle`](oracle/Network/Oracle.hs).


## Running the Oracle

The oracle takes three command-line arguments:

1. The configuration file for the Marlowe Runtime backend.
2. The address of the oracle.
3. The signing key file for the oracle.

For example, executing the oracle as

```bash
marlowe-oracle \
  preprod.config \
  addr_test1qqzg379vgpjnm3n5kffdqq86sr6veng453rfnu07c8y9umdn3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2swc7lhj \
  payment.skey
```

might result in the following output:

```console
Contract ID: 1029b6a81fa3278f6179bce4c51f648cbf40a6dd1c2bb8aff2168b187ae78bd7#1
  Ready for oracle: "ADAUSD"
  Available from oracle: ADAUSD
  Failed: ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed "TEIntervalError (InvalidInterval (POSIXTime {getPOSIXTime = 1672255867000},POSIXTime {getPOSIXTime = 1672240010999}))")

Contract ID: 43d663aae2d19fbfa39a9d1f32c45a06f3ac266eb6098f4a55dfc996a6bb1a5f#1
  Ready for oracle: "XYZ"
  Available from oracle: 
  Ignored.

Contract ID: 5aca6d23973da2ad2b1927ee7bd95cf4f99c7eb47cfd267672711326bf8bb98c#1
  Ready for oracle: "ADAGBP"
  Available from oracle: ADAGBP
  ADAGBP = 20941100 [scaled integer units]
  Confirmed.
```

In the above, the oracle attempted to provide input to the first contract, but the time for oracle input had already passed. The oracle ignored the second contract because the oracle does not provide a feed for the `XYZ` symbol. The oracle confirmed that it reported a value for `ADAGBP` to the third contract.


## Creating Example Contracts

The `bash` script [create-example-contracts.sh](create-example-contracts.sh) is provided for creating Marlowe contracts that require oracle input. It takes three arguments:

1. The symbol that the oracle should report.
2. The address of the oracle.
3. The address creating the transaction.
4. The signing key file for the oracle.

For example,

```bash
./create-example-contract.sh \
  ADAGBP \
  addr_test1qqzg379vgpjnm3n5kffdqq86sr6veng453rfnu07c8y9umdn3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2swc7lhj \
  addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
  payment.skey
```  

results in the following output:

```console
Address: addr_test1qqzg379vgpjnm3n5kffdqq86sr6veng453rfnu07c8y9umdn3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2swc7lhj
Symbol: ADAGBP
Timeout: 1672342136000 = Thu Dec 29 12:28:56 PM MST 2022
contractId: 5aca6d23973da2ad2b1927ee7bd95cf4f99c7eb47cfd267672711326bf8bb98c#1
blockHeaderHash: 689ca1d8d072b9cd409a53bbe2fc2fe32b2c9840be5a72ce6c8916ff7d05e4a9
blockNo: 454180
slotNo: 16572556
```

This creates a minimalist contract `example.contract` that requires oracle input:

```console
$ json2yaml example.contract 
timeout: 1672342136000
timeout_continuation: close
when:
- case:
    choose_between:
    - from: 0
      to: 1000000000000000000
    for_choice:
      choice_name: ADAGBP
      choice_owner:
        address: addr_test1qqzg379vgpjnm3n5kffdqq86sr6veng453rfnu07c8y9umdn3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2swc7lhj
  then: close
```


## Design

The oracle uses a Marlowe Runtime *discovery follower* to watch the blockchain for new Marlowe contracts. In parallel with that, it examines the creation and subsequent transaction of the contract to see if the contract might ever need input from the oracle. If the contract will not need oracle input, then it stops watching the contract. If the oracle will need oracle input, then it waits for the contract's last unspent transaction and provide oracle input to that if the current state of the contract requires such input. The oracle construction a transaction with input consisting of the correct `IChoice` to report the value for the requested symbol.

![Design of the Marlowe Oracle](oracle-design.png)

A future version of this oracle will allow requiring that a fee be paid to the oracle in return for the oracle's successful report. The diagram below shows that construct. The oracle would ignore contracts that did not have a `Pay` with sufficient Ada to the oracle.

![Block for a fee-based oracle input in a Marlowe contract](oracle-fee.png)
