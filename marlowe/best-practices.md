# Best Practices for Marlowe on Cardano


## Executive Summary

Marlowe's design and safety enables the deployment of a variety of powerful smart contracts on the Cardano blockchain with a minimum of development effort and with easy auditing of contract logic. Many standard financial use cases have already been implemented in Marlowe, as have example contracts for escrows, auctions, and swaps. This document provides guidance on what types of contracts are best suited for Marlowe on Cardano, how the specifics of the Cardano blockchain interact with the semantics of the Marlowe language, and how to design and verify complex Marlowe contracts for successful execution on Cardano.


## Contents

- [Summary of Capabilities](#summary-of-capabilities)
- [Examples of Types of Contracts Well-Suited to Marlowe on Cardano](#examples-of-types-of-contracts-well-suited-to-marlowe-on-cardano)
- [Limitations Imposed by the Marlowe Language](#limitations-imposed-by-the-marlowe-language)
- [Limitations in Marlowe Playground](#limitations-in-marlowe-playground)
- [Limitations Imposed by the Cardano Blockchain](#limitations-imposed-by-the-cardano-blockchain)
	- [One Marlowe Contract per Transaction](#one-marlowe-contract-per-transaction)
	- [Limitations Due to Native Token Names on Cardano](#limitations-due-to-native-token-names-on-cardano)
	- [Limitations Due to the Initial State of a Contract](#limitations-due-to-the-initial-state-of-a-contract)
	- [Limitations Due to Cardano's Protocol Parameters](#limitations-due-to-cardano-s-protocol-parameters)
		- [Minimum UTxO Requirement](#minimum-utxo-requirement)
		- [Maximum Value Bytes](#maximum-value-bytes)
		- [Maximum Transaction Size](#maximum-transaction-size)
		- [Maximum Execution Steps and Memory](#maximum-execution-steps-and-memory)
- [Tools for Detecting Potential Locking/Blocking of Marlowe on Cardano](#tools-for-detecting-potential-locking-blocking0of-marlowe-on-cardano)
- [Designing Marlowe Contracts to Avoid Protocol Limits](#designing-marlowe-contracts-to-avoid-protocol-limits)
- [Monetary Policy for Role Tokens](#monetary-policy-for-role-tokens)
- [Interaction with Plutus Contracts](#interaction-with-plutus-contracts)


## Summary of Capabilities

See [Marlowe Finance](https://marlowe-finance.io/) or the [Marlowe Developer Documentation Index](https://developers.cardano.org/docs/smart-contracts/marlowe) for links to documentation for the Marlowe domain-specific language (DSL). In brief, Marlowe is an expressive and safe language for running smart contracts on the Cardano blockchain:
-   Focus on transactional contracts
-   Account-based model
-   Participants identified by either tokenized roles or by public-key hashes
-   Handling of any assets supported by the underlying blockchain
-   Support for off-chain inputs such as oracles or participants' choices
-   Arithmetic and logical computations
-   Simple control flow and branching
-   Timeouts ensuring that contract progresses
-   Payment of residual funds when contract closes

Marlowe provides several safety guarantees.
- Execution always terminates
- Contracts expire eventually and deterministically
- No assets retained on close
- Conservation of value

Several proofs of the safety of Marlowe are derived using formal verification methods.
-   Conservations of funds
-   Contracts always terminate
-   Positivity of account balances
-   Quiescence
-   Idempotency of reductions
-   Composition of inputs


## Examples of Types of Contracts Well-Suited to Marlowe on Cardano

Marlowe is well-suited for financial contracts such as those codified in the [ACTUS](https://www.actusfrf.org/) and other standards. An [ACTUS implementation](../marlowe-actus/README.md) is already available for Marlowe on Cardano. The [Marlowe Contract Examples](./example-contracts.md) index a diversity of contracts that cover the following domains:
- bonds, forwards, options, futures, swaps, etc.
- structured financial products
- escrows
- auctions
- peer-to-peer loans
- token swaps
- airdrops

In general, Marlowe contracts that are easy to design, audit, and deploy share the following characteristics:
- a small number of predetermined parties
- a finite life cycle for the contract
- input from on- or off-chain oracle(s)
- settlement in Ada, native tokens, or a stablecoin
- straightforward arithmetical and logical computations
- one or more repetitions of opportunities for choices and deposits
- a modest amount of state internal to the contract
- special logic for handling cases where parties fail to take action

A Marlowe contract might be considered "simple" or "straightforward" even if it is large: for example, a 30-year loan with monthly payments would involve 360 deposits, but the logic of each of the 360 deposits is identical: the Haskell or TypeScript source code for generating such a loan contract would be very compact, even though the Marlowe output of that code would be too large to display in a visual editor such as [Marlowe Playground](https://play.marlowe-finance.io/).


## Limitations Imposed by the Marlowe Language

The safety-first principle used in the design of the [Marlowe domain-specific language (DSL)](https://github.com/input-output-hk/marlowe/) has limited its functionality for good purpose. In particular, there are two limitations imposed when a Marlowe contract is created: (1) number of participants in a contract is limited to that predetermined when the contract was designed; (2) existing Marlowe contract logic cannot be revised. Additionally, some programming-language constructs are absent from the Marlowe language in order to ensure Marlowe’s safety:
- Recursion is not allowed.
- Looping is not supported.
- Functions or macros may not be defined.
- Timeouts must be numeric constants.
- Only `Case` continuations may be merkleized.
The [Faustus programming language](https://www.uwyo.edu/wabl/faustus.html) relaxes some of the limitations above, yet it compiles to safe Marlowe.


## Limitations in Marlowe Playground

[Marlowe Playground](https://play.marlowe-finance.io/) currently lacks the capabilities to construct, display, or simulate merkleized Marlowe contracts. Large contracts containing many terms also pose difficulties for the Playground: simulating a large contract may exceed the computing resource limits in the server for Marlowe Playground. Similarly, static analysis of large Marlowe contracts typically exceeds the resources of the Playground. For example, static analysis of auctions involving many parties may take tens or hundreds of minutes. 


## Limitations Imposed by the Cardano Blockchain

Marlowe’s implementation on particular blockchains typically inherits some of the limitations of the underlying blockchain. The ledger rules and the protocol limits of the Cardano blockchain somewhat restrict the Marlowe contracts that can be deployed there. Most of these limitations are trivial and easily avoided, but a few do impact complex Marlowe contracts and hence need careful consideration. The next sections discuss these limitations in detail.


### One Marlowe Contract per Transaction

In order to avoid double-satisfaction attacks, no more than one Marlowe contract may be run in a single Marlowe transaction.


### Limitations Due to Native Token Names on Cardano

Although Marlowe semantics place no limit on the length of a role name or the length of a name of a native token used in a Marlowe contract, the Cardano ledger rules impose a limit of 32 bytes for token names. Because Marlowe roles are realized as native (authorization) tokens on the Cardano blockchain, role names are also limited to 32 bytes there. Thus, a Marlowe contract designed to run on the Cardano block chain must have role names no longer than 32 bytes long and native token names no longer than that, too.

Although a Marlowe contract violating the 32-byte limit can be successfully *created* on the Cardano blockchain, any contract logic involving a role or native token whose name is longer than 32 bytes could never be *executed* in a Cardano transaction. This will result in the execution paths of the contract that violate this ledger rule being impossible to run on Cardano.


### Limitations Due to the Initial State of a Contract

A Plutus validator cannot validate the *creation* of a smart contract: a Plutus validator, such as the Marlowe validator, only validates the spending of the contract's already-existing UTxO. Thus, it is possible to create a Marlowe contract on chain that deeply violates Marlowe semantics or the constraints for Marlowe on Cardano. In particular, the following types of invalid initial state will prevent a Marlowe contract from executing properly on Cardano:
1. A total value in the Marlowe state that does not match the value in the contract's creation UTxO.
2. An internal account with a zero or negative initial balance.
3. Any role or token names longer than 32 bytes.
4. Duplicate internal accounts.
5. Duplicate choices (like those set by `Choice` terms in a contract).
6. Duplicate bound values (like those set by `Let` terms in a contract).

Items 1 and 2 will prevent the contract from ever executing, thus making the contract's creation UTxO unspendable. Item 3 may block some execution paths of the contract. Items 4, 5, and 6 violate preconditions for Marlowe semantics, so the contract's subsequent execution is not guaranteed to conform to Marlowe semantics.


### Limitations Due to Cardano's Protocol Parameters

Ledger rules and protocol parameters define the validity of transactions on the Cardano blockchain and limit the resources that a Plutus script may consume. These rules and parameters also affect the on-chain viability of semantically-valid Marlowe contracts and transactions.

A Marlowe contract on the Cardano blockchain may be forever *locked* (inaccessible) if all possible transactions exceed protocol parameters for script execution. Similarly, certain paths of contract execution will be *blocked* if some (but not all) semantically allowable transactions exceed protocol limits. The next two sections describe [how to detect whether a contract is at-risk of this behavior](#tools-for-detecting-potential-locking-blocking-of-carlowe-on-cardano) and [how to design contracts so that they do not exhibit such behavior](#designing-marlowe-contract-to-avoid-protocol-limits).

The table below summarizes the situation. Items are marked "locks" if they would prevent further execution of the contract if they are the top-level construct or the timeout continuation of a `When` contract; otherwise, they just prevent some (but not all) execution paths for the contract. They are marked "blocks" if they can never lock the contract, but can prevent some execution path.

| Protocol Limit   | Protocol Parameter           | `Pay` | `Let` | `If`/`Assert` | `When` | `Close` | `Case` | `Deposit` | `Choice` | `Notify` |
|------------------|------------------------------|-------|-------|---------------|--------|------|---------|-----------|----------|----------|
| Transaction size | `maxTxSize`                  | locks | locks |               |       | locks   | blocks |           | blocks   |          |
| Execution steps  | `maxTxExecutionUnits.steps`  | locks | locks | locks         | blocks | locks   | blocks | blocks    | blocks   | blocks   |
| Execution memory | `maxTxExecutionUnits.memory` | locks | locks | locks         | blocks | locks   | blocks | blocks    | blocks   | blocks   |
| Minimum ADA      | `utxoCostPerWord`            | locks |       |               |        |         |        | blocks    |          |          |
| Value size       | `maxValueSize`               | locks |       |               |        |         |        | blocks    |          |          |

Next we discuss these in more detail. See the [Marlowe Test Report](test/test-report.md#funds-locked-or-input-blocked-due-to-exceeding-protocol-parameters) for further discussion, or the example [Jupyter notebook demonstrating protocol limitations](../marlowe-cli/test/exceed-protocol-parameters.ipynb).


#### Minimum UTxO Requirement

Each transaction output on the Cardano ledger must include a minimum lovelace value in the UTxO. The protocol parameter `utxoCostPerWord` specifies that value in proportion to the size (in words) of the UTxO. The size is affected by the presence of native tokens, a datum hash, and other constituents of the UTxO. In particular, the UTxO for the Marlowe contract at the Marlowe script address is also subject to this rule, so the value held in a Marlowe contract on Cardano must never be less than the minimum UTxO requirement.

A trivial example of a Marlowe contract that violates this ledger rule is when a Marlowe contract contains a total of one lovelace over all of its accounts: one lovelace is well below the minimum UTxO requirement. Hence, if a Marlowe contract ever attempted to pay out in a transaction so much of its value that it was left with just a single lovelace at the script UTxO, the ledger would reject the transaction. This could have been avoided by designing the contract and setting its initial state so that such a situation would never be encountered on any of the contract's execution paths. If the payment were part of a `Case` term within the contract, then that `Case` could never be executed; this wouldn't effect the viability of other `Case` terms in the `When` term or the timeout continuation for the `When` term. Hence, this execution path would be *blocked*.

Another example is when a Marlowe contract receives a deposit that would cause the ledger rule to be violated. Say that the contract currently holds 1.5 Ada and that exactly satisfies the minimum UTxO requirement, but a native token is deposited into the contract, except without any accompanying lovelace, which means that the resulting UTxO has a higher minimum UTxO requirement that it no longer meets: the ledger would reject the transaction. A `Deposit` in Marlowe always occurs within a `Case`, so this particular execution path would be *blocked*.


#### Maximum Value Bytes

The Cardano ledger also contains a rule that the size (in words) of the bytes storing the information about native tokens in a UTxO cannot exceed a protocol parameter named `maxValueSize`. In practice, this means that there is a limit to the number of different types of tokens that can be held in a single UTxO. A Marlowe contract might attempt to violate this ledger rule in two types of circumstances:
1. A `Pay` might send so many types of native tokens to the Marlowe payout script address that the UTxO exceeds the maximum value size.
2. A `Deposit` might place so many tokens at the Marlowe script address that the contract's UTxO exceeds the maximum value size.

In either case such a transaction would be rejected by the ledger and the execution path could never become realized.


#### Maximum Transaction Size

The size (in bytes) of a transaction is limited by a Cardano ledger rule, specified by the protocol parameter `maxTxSize`. There are quite a few types of situations where a transaction executing a step in a Marlowe contract might become too large:
1. A `Pay` might exceed transaction size if the payments it makes require a `ScriptContext` that makes the transaction too large.
2. Similarly, a `Close` might result in numerous payments that enlarge the `ScriptContext`.
3. It might be impossible to demerkleize a `Case` term within a `When` because the continuation of the contract is too large.
4. A `Let` or `Choice` might increase the `Datum` size too much by adding a bound value or chosen value.


#### Maximum Execution Steps and Memory

Plutus execute costs are limited to a specific usage of "memory" and CPU "steps", defined by the protocol parameters `maxTxExecutionUnits.memory` and `maxTxExecutionUnits.steps`, respectively. If a Marlowe contract does too much computation or uses too much memory in a transaction, then it may violate this ledger rule. There are a wide variety of situations when execution steps might be exceeded:
1. A `Pay`, `If`, `Let`, or `Assert` might require an extensive evaluation of `Value`s and `Observation`s, thus exceeding execution-step limits.
2. A `When` might include many `Case`s, so that evaluating the `Action`s within them (any of which might require extensive evaluation) exhausts the execution-step budget.
3. A `Close` is unlikely to exceed execution steps unless it triggers too many payments, but this is theoretically possible.
4. Because the execution of `Contract` terms is continued until a quiescent state is reached, the sequence of continuations might exceed step limits even though individual steps do not.

The same considerations apply to execution memory as to execution steps. Furthermore, this includes the sequence of `Contract` continuations mentioned above because execution memory is the measured as a sum (not a maximum) over the execution's steps.


## Tools for Detecting Potential Locking/Blocking of Marlowe on Cardano

The [Marlowe CLI](../marlowe-cli/ReadMe.md) tool includes capabilities for analyzing the suitability of a Marlowe contract for execution on the Cardano blockchain. It provides options to check that a contract does not exceed any of the on-chain limitations discussed above. Its command-line interface allows the user to select which checks to perform and its output highlights the causes and location of violations.

It is important to realize that the tool tests whether it is *possible* to avoid violating the ledger rules in executing a contract. This does not mean that it is *trivial* to construct every valid transaction for the contract. For example, if a wallet contains a deficiency such as only numerous low-value unspent UTxOs and the contract transaction requires a high input value, then the `ScriptContext` for the transaction might end up containing so many input UTxOs that the transaction-size limit is exceeded and the transaction would fail to validate. The remedy for such a situation would be to first "defragment" the wallet so that it contains a single sufficiently large unspent UTxO, so that the transaction could consume it and then would validate. In general, the tool's analysis assumes that the wallets involved in the transaction would contain (or be made to contain) the appropriate unspent UTxOs for a "parsimonous" `ScriptContext` that would avoid needlessly violating ledger rules. As is the case for many Plutus contracts (not just for Marlowe), sloppy or inefficient transaction construction by a wallet or coin selector will result in excess fees or even in failure to validate a nominally valid Plutus transaction.

```console
$ marlowe-cli run analyze --help
Usage: marlowe-cli run analyze (--mainnet | --testnet-magic INTEGER)
                               --socket-path SOCKET_FILE
                               --marlowe-file MARLOWE_FILE [--preconditions] 
                               [--roles] [--tokens] [--maximum-value] 
                               [--minimum-utxo] [--execution-cost] 
                               [--transaction-size] [--best] [--verbose]

  Analyze a Marlowe contract for on-chain viability.

Available options:
  --mainnet                    Execute on mainnet.
  --testnet-magic INTEGER      Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value.
  --socket-path SOCKET_FILE    Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.
  --marlowe-file MARLOWE_FILE  JSON file with the state and contract.
  --preconditions              Whether to check preconditions for valid Marlowe state.
  --roles                      Whether to check lengths of role names.
  --tokens                     Whether to check lengths of token names.
  --maximum-value              Whether to check the `maxValueSize` protocol limit.
  --minimum-utxo               Whether to check the `utxoCostPerWord` protocol limit.
  --execution-cost             Whether to check the `maxTxExecutionUnits` protocol limit.
  --transaction-size           Whether to check the `maxTxSize` protocol limit.
  --best                       Whether to compute tight estimates of worst-case bounds, instead of generous estimates of those bounds.
  --verbose                    Whether to include worst-case example in output.
  -h,--help                    Show this help text
```

The `--preconditions` flag causes the initial state of the contract to be checked for [limitations due to the initial state of a contract](#limitations-due-to-the-initial-state-of-a-contract), except it does not check that the total value in the initial state of the contract matches the value in the creation UTxO because that creation has not occurred yet. Off-chain tools like [Marlowe Runtime](../marlowe-runtime/doc/ReadMe.md) typically ensure that correspondence between total value in the initial state and the creation transaction.

The `--roles` and the `--tokens` flags check that role and token names in the contract are no longer than 32 bytes, respectively, [as required by the ledger rule](#limitations-due-to-native-token-names-on-cardano).

The `--maximum-value` flag checks conformance to the [maximum value size](#Maximum-value-bytes) ledger rule, and can run with or without the `--best` flag. Without the `--best` flag, the tool does a quick, worst-case analysis (i.e., an overestimate) that computes the value size under the hypothesis that at some point in the contract there is a UTxO containing every native token ever mentioned in the contract. With the `--best` flag, the tool checks the ledger rule for every possible transaction the contract could ever execute and provides a precise (tight) bound on the size of any value in the contract. If the contract is complex, then analysis with the `--best` flag may be slow and may require significant computational resources.

Similarly, the `--minimum-utxo` flag checks conformance to the [minimum UTxO requirement](#minimum-utxo-requirement), either quickly and generously without the `--best` flag or slowly and strictly with the `--best` flag. Any value that is not smaller than the value reported by the tool would be safe as the initial total value in the Marlowe contract when it is created. That value must match the total value in the internal accounts of the Marlowe state, of course.

The `--execution-cost` and the `--transaction-size` flags check for violations on the [execution cost](#maximum-execution-steps-and-memory) and the [transaction size](#maximum-transaction-size) limits, respectively. These analyses assess every possible transaction for the contract, regardless of whether `--best` is set or not.

Without the `--verbose` flag, the analysis report simply prints how severe the violations are. With the `--verbose` flag, the analysis report also prints the location in the contract that offends worse.

A [Jupyter notebook](../marlowe-cli/cookbook/repair.ipynb) illustrates the use of `marlowe-cli run analyze` on a poorly designed contract.

In principle, if a Marlowe contract passes all of the checks in `marlowe run analyze`, then the contract should execute on the Cardano blockchain according to Marlowe semantics (i.e., as it would execute in a simulator such as [Marlowe Playground](https://play.marlowe-finance.io/)). However, it is probably not advisable to run a contract that is extremely close to the limits imposed by the ledger because running close to those limits would require that off-chain code (wallets, coin selectors, etc.) construct a parsimonious `ScriptContext` that doesn't unnecessarily enlarge the size of the transaction or increase its execution cost beyond the theoretical minimums. Also, if the transaction size limit or the Plutus execution cost limits were lowered in the future by a protocol-parameter change, then the Marlowe contract might be at risk of having unexecutable transactions in the future.


## Designing Marlowe Contracts to Avoid Protocol Limits

Although the protocol parameters and ledger rules do not put typical, simple Marlowe contracts at risk, it nevertheless is safest either to run all execution paths of the contract on a Cardano testnet or to use the `marlowe-cli run analyze` tool to check the contract. For complex contracts, such testing or analysis is essential. We list best practices and remediation methods below.


#### Choose role and token names that are no more than 32 bytes long.

Unicode contains characters that occupy more than one byte, so make sure that role and token names are no longer than 32 *bytes*, not simply 32 *characters*.


#### Ensure that the contract's initial state is valid.

Use a Marlowe tool (such as [Marlowe Runtime](../marlowe-runtime/doc/ReadMe.md)) that ensures the construction of a valid initial Marlowe state in the contract's creation transaction, as described above in [Limitations Due to the Initial State of a Contract](#limitations-due-to-the-initial-state-of-a-contract). If manually constructing the initial state, check that it contains positive accounts, valid role and token names, and no duplicate entries to accounts, choices, or bound values. *It is also critically important that the value sent to the Marlowe contract address in the creation transaction **exactly** matches the total value in the accounts in the initial state of the contract.*


#### Ensure that the initial value in the contract contains sufficient lovelace.

Use the `marlowe-cli run analyze --minimum-utxo` analysis to compute the worst-case scenario for value ever held in the contract. If that violates the [minimum UTxO requirement](#minimum-utxo-requirement), then increase the lovelace in the initial accounts when the contract is created and re-run the analysis. Simply increasing the initial total lovelace is not always sufficient for Marlowe contracts that employ complex payment logic such as payments of `AvailableMoney` or payments that generate static-analysis warnings, *so it is critically important to rerun the analysis after adjusting initial lovelace in the contract's state.*


#### Avoid contracts that use an excessive number of native token types.

Using more than 100 different unique native token *types* (i.e., policy ID plus token name) may put the Marlowe contract at risk of exceeding the [maximum value requirement](#maximum-value-bytes), so be sure to use `marlowe-cli run analyze --maximum-value` if many tokens are used in the contract. Note that it does not matter how many of each token are in the contract; only the number of *unique* tokens matters.


#### Avoid or break up complex logic in the contract.

It is not difficult to exceed the [execution steps or memory limits](#maximum-execution-steps-and-memory) in a contract that contains many `Case` terms, nested `Pay` terms, or complicated arithmetical or logical computations. Run all execution paths of the contract on a test network and/or use `marlowe-cli run analyze --execution-cost` to check the contract's safety. These remedies for excess execution cost can generally reduce the cost of any contract below the execution limits, but they might need to be applied assiduously.
1. Break complex nested terms into several transactions by inserting `Notify` terms into the contract. For example, instead of making six successive `Pay`s in the same transaction, break that construct into three `Pay`s, a `When [Case Notify ...]`, and three `Pays`, which will split the six-pay transaction into a three-pay transaction followed by an `INotify` that triggers three more payments. Choose the timeout for the `When` term carefully and use [Marlowe Playground's](https://play.marlowe-finance.io/) static analysis tool to understand the consequences and safely of splitting the transaction.
2. Similarly, break extensive arithmetic computations into multiple transactions by storing results in an intermediate bound value (via `Let`) and then a `When [Case Notify ...]`.
3. A `When` term with many `Case`s can be split into a `When` with the first several `Case`s and an extra `Case Notify` that continues to another `When` with the remaining `Case`s. Once again, use static analysis and simulation to check that the split has the intended behavior.
4. Carefully re-use bound-value names and choices, in order to reduce the size of the contract's internal state.
5. Take advantage of the fact that a bound value or choice defaults to zero in `UseValue` or `ChosenValue` if the value has never been bound or the choice has never been made. Although this reduces execution cost, it will cause warnings in [Marlowe Playground's](https://play.marlowe-finance.io/) static analysis, so proceed carefully.
6. Use very short names for role tokens and (where feasible) native tokens. Every byte counts, so expressive user-friendly role and token names costs more. An application's front end or user interface can translate terse role or token names into human-readable ones.
7. Use Marlowe's merkleization feature (e.g., `marlowe-cli run initialize --merkelize`) to store the contract's future logic off the blockchain, so it does not incur execution-memory costs. The [English Auction](../marlowe-cli/cookbook/english-auction.ipynb) example features aggressive merkleization.
8. Remember that the continuation of a `When` cannot be merkleized, with the implication that a contract with `When` continuations containing many terms or (especially) additional `When` continuations will be large. One can mimic a merkleization of a `When` continuation by having the `When` continue to a `When [Case Notify ...] timeout Close` where the `timeout` is in the far distant future. That timeout needs to be chosen carefully and safely. The [Stabilized Collective Loan](../marlowe-cli/cookbook/collective-loan.ipynb) provides an example of merkleized continuations.

The [Marlowe Cookbook](../marlowe-cli/cookbook/) contains examples that use the above techniques to run complex and lengthy contract on the blockchain.


#### Use reference scripts and merkleization to reduce on-chain size.

A Marlowe transaction will be approximately 12k bytes smaller if one uses the reference script for the Marlowe validator instead of including the Marlowe validator script itself inside the transaction. [Marlowe Runtime](../marlowe-runtime/doc/ReadMe.md) does this automatically and [Marlowe CLI](../marlowe-cli/ReadMe.md) will do this if the `--at-address` option is used in `marlowe-cli run initialize`. Using the reference script also substantially reduces the fee for the transaction. Empirically, execution cost becomes a limiting factor before the size of the transaction does, if a reference script has been used.

Even with reference scripts, transaction size can be large if either the Marlowe contract itself is large or the internal state of the contract grows large. The standard merkleization technique and the trick (mentioned in the previous section) to merkleize the continuation of a `When` term will reduce the on-chain size of a contract to manageable proportions.


#### Be parsimonious in the use of the contract's internal state.

The `Datum` of a Marlowe contract UTxO can only hold a limited amount of internal state without exceeding execution costs or transaction size. This is the most difficult limitation to remediate in Marlowe on Cardano, and is a subject of active research for the next version of Marlowe. Several techniques are helpful in reducing the size of the internal state, however:
1. Use short names for bound values and for choice names. Every byte counts! A front end or user interface can translate the terse names used by the contract into human-readable text.
2. Reuse names of bound values and choice IDs when it is safe to do so. Rather than letting bound values and chosen values accumulate in the contract's state, reuse them instead of creating new ones later in the contract, if that doesn't impact contract logic.
3. Compare using roles vs addresses in the contract. An address takes up more space in the internal state of a Marlowe contract than a role name does, so consider using roles in preference to addresses if the size of the state (and of the contract itself) is a concern.


#### When all else fails . . . .

In extreme situations, such as a complex contract with many simultaneous possible actions, many roles, and many state variables, consider introducing semi-centralization into the contract. Instead of distributing role tokens at the onset of a contract, let an administrative role hold all or some of them initially and distribute them to participants as needed. This technique collapses a `When` with a `Case` for each role into a `When` with a single `Case` where the administrator must distribute the role token for the `Action` in the `Case` in a just-in-time fashion to the party who will take that `Action`.  The [Collective Loan with 30 Lenders and 70 Borrowers](../marlowe-cli/cookbook/collective-loan-30-70.ipynb) illustrates how light semi-centralization works in a Marlowe contract with a large number of parties. Similarly, in some contracts the administrator can make the `Choice` instead of a party, avoiding the recording of choices by numerous parties in the contract's internal state.

Role tokens can also be used creatively to overcome apparent limitations of Marlowe on Cardano. For example, a role token *for the contract itself* can be deposited by one party and paid to another. This opens somewhat bizarre use cases where one or more Marlowe contracts manage some of their own role tokens. For some use cases, this technique can be exploited to record choices of multiple parties without increasing the internal state of chosen values, as parties pass a role token among themselves when recording the choice.

Also review the [Marlowe Debugging Cookbook](./debugging-cookbook.md) for technical approaches to debugging Marlowe contracts on Cardano.


## Examples of Contracts Poorly Suited to Marlowe on Cardano

Some types of contracts are not well suited for using Marlowe on the Cardano blockchain:
- Contracts that contain timeouts spaced close to the Cardano block-production rate.
- Contracts that require floating-point arithmetic.
- Contracts that require very many bound values in their internal state.
- Contracts that make very many differently-named choices by many parties.
- Contracts that have many alternative actions possible at the same time.
So, for example, a distributed exchange (DEX), order book, market maker, or token marketplace would be poorly suited for Marlowe unless the Marlowe contracts were supervised by Plutus contracts or semi-centralized off-chain infrastructure.

Aggressive merkeization of a contract exhibiting a combinatorial explosion of possibilities can mitigate that last challenge on the list above, *if the merkleization occurs as the contract is constructed programmatically and the map from the continuation's contract hash to the continuation's contract is managed efficiently.* The [Collective Loan](../marlowe-cli/cookbook/collective-loan.ipynb) example features such just-in-time merkleization.


## Monetary Policy for Role Tokens

By default, Marlowe Runtime uses a one-shot monetary policy for role tokens that allows them to be burned by any holder at any time: see [`Language.Marlowe.Runtime.Plutus.V2.Scripts.MarloweV1.RoleTokensPolicy`](../marlowe-runtime/plutus-scripts/Language/Marlowe/Runtime/Plutus/V2/Scripts/MarloweV1/RoleTokensPolicy.hs) for details. This enforces that additional role tokens may never be minted in the future, after the initial minting event. Thus, a possessor of a role token can verify via the blockchain history that they hold the unique token for that role and that no one can "impersonate" them in the contract.

Although this default monetary policy is convenient for most use cases, Marlowe's Plutus validator allows role tokens with any monetary policy to be used in a Marlowe contract. This opens up novel possibilities for role-based authorization in Marlowe contracts.
* A monetary policy that mints multiple copies of some role tokens can be used for a contract where several parties share the same identity in the contract. The extra copies can also be used as backup tokens, placed in a "cold" wallet, for use in case the "hot" token is lost or burned.
* A monetary policy that allows for minting additional role tokens after the initial minting transaction could be used to add participants to a Marlowe contract.
* A monetary policy that burns some role tokens while minting new ones, which would allow for morphing identities during a contract.

It is also possible for a Marlowe contract to manage its own role tokens or for a Marlowe contract to manage the role tokens for another Marlowe contract. This allows use cases like the securitization of a Marlowe contract, or hierarchies/networks/graphs of Marlowe contracts.


## Interaction with Plutus Contracts

Other Plutus contracts can be run in the same transaction as a Marlowe contract. A typical situation where this might occur is a when a Plutus oracle acts as a party to Marlowe contract via Marlowe's `Choose` construct: the Plutus contract would hold the role token and ensure that it authorizes the `IChoice` with the correct oracle data. A more advanced application that hybridizes Plutus and Marlowe would be an ecosystem where Plutus contract(s) supervise one or more Marlowe contracts: this would combine the safety of Marlowe with the flexibility of Plutus.
