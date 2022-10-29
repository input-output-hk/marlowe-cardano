# Test Report for Marlowe's Plutus Validators


## Executive Summary

This document summarizes the testing status of Marlowe's Cardano implementation prior to the audit of Version 1 of the Marlowe validators, which are coded in Plutus V2. Marlowe uses a *Semantics Validator* to validate transitions (application of inputs, timeouts, payments, etc.) in Marlowe contracts and a *Payout Validator* to validate receipt of funds from roles in Marlowe contracts. The [On-Chain Transaction Specification for Marlowe](../marlowe/specificiation/marlowe-cardano-specification.md) defines the behavior of these validators. The [Isabelle Specification for Marlowe](https://github.com/input-output-hk/marlowe/tree/master/isabelle) defines the behavior of the Marlowe DSL (domain-specific language) itself.

This test report covers the tests of the Haskell implementation of Marlowe semantics and of the Plutus validators that connect those semantics to the Cardano blockchain. Testing involves approximately two hundred property-based tests and dozens of on-chain tests. This document summarizes [two risk areas](#summary-of-testing-history-and-results) that were discovered and demonstrated during testing:
1. Marlowe contracts that are created with an initially invalid state are either unexecutable or their execution behavior may vary from Marlowe's abstract semantics. 
2. Semantically valid Marlowe transactions may [fail to validate on the blockchain](#funds-locked-or-input-blocked-due-to-exceeding-protocol-parameters) due to Cardano ledger limits set by protocol parameter. The document concludes by highlight areas for further investigation and development.


## Contents

-   [Introduction](#introduction)
-   [Formal Guarantees for Marlowe](#formal-guarantees-for-marlowe)
-   [Property-Based Tests of Marlowe's Haskell Implementation](#property-based-tests-of-marlowe's-haskell-implementation)
    -   [Tests of Marlowe Semantics Implementation](#tests-of-marlowe-semantics-implementation)
    -   [ACTUS Contract Tests](#actus-contract-tests)
    -   [Tests of Plutus Functions Used by the Marlowe Implementation](#tests-of-plutus-functions-used-by-the-marlowe-implementation)
    -   [Tests of Marlowe's Plutus Validators](#tests-of-marlowes-plutus-validators)
        -   [Semantics Validator](#semantics-validator)
        -   [Payout Validator](#payout-validator)
    -   [Miscellaneous Property-Based Tests](#miscellaneous-property-based-tests)
-   [Test Oracle for Marlowe](#test-oracle-for-marlowe)
-   [On-Chain Tests of Marlowe](#on-chain-tests-of-marlowe)
    -   [Formal Tests](#formal-tests)
    -   [Informal Tests](#informal-tests)
-   [Testing Tools](#testing-tools)
-   [Summary of Testing History and Results](#summary-of-testing-history-and-results)
    -   [Funds Locked or Input Blocked Due to Exceeding Protocol Parameters](#funds-locked-or-input-blocked-due-to-exceeding-protocol-parameters)
-   [Areas of Concern](#areas-of-concern)


## Introduction

Two specifications define the behavior of Marlowe on the Cardano blockchain:
- The [Isabelle Specification for Marlowe](../marlowe/specificiation/marlowe-isabelle-specification-4f9fa249fa51ec09a4f286099d5399eb4301ed49.pdf) defines the Marlowe DSL and its operational/executable semantics. The language and the semantics are independent of the particular blockchain where Marlowe contracts are run.
- The [On-Chain Transaction Specification for Marlowe](../marlowe/specificiation/marlowe-cardano-specification.md) defines the realization of Marlowe on the Cardano blockchain. In particular, this specification relates abstract Marlowe types and constructs to the concrete types present in Cardano's Plutus language in such a way the Marlowe transactions conform to Cardano's [Ledger Specifications](https://github.com/input-output-hk/cardano-ledger#cardano-ledger) and the [Formal Specification of the Plutus Core Language](https://github.com/input-output-hk/plutus/blob/master/plutus-core-spec/draft-new-specification/plutus-core-specification.pdf).

The bridge between abstract semantics and concrete Plutus is the [`Language.Marlowe.Core.V1.Semantics`](../marlowe/src/Language/Marlowe/Core/V1/Semantics.hs) function `computeTransaction`:
```haskell
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
```
where the blockchain-agnostic types  `TransactionInput` , `State`, `Contract`, and `TransactionOutput` are the input to the contract, its pre-transaction state, its pre-transaction contract, and the result of applying the input to the contract. Note that applying input to a Marlowe contract results in a new, smaller contract that is the sub-contract of the original pre-transaction contract: essentially, a Marlowe contract consists of a directed acyclic graph (DAG) of contract continuations, where branching is determined by applying inputs or experiencing timeouts. This DAG-continuation has advantages not only for proving guarantees on Marlowe semantics, but also for minimizing the on-chain storage needed for a contract.

Marlowe transactions utilize one of two Plutus V2 validators, residing in the [`Language.Marlowe.Scripts`](../marlowe/src/Language/Marlowe/Scripts.hs) module:
- The *Semantics Validator* is Plutus code that verifies that a transaction obeys the Marlowe DSL semantics along with the Cardano ledger rules. This validator resides in the `smallUntypedValidator` function that marshals Plutus types to call `computeTransaction` and checks constraints on input and output.
- The *Payout Validator* is Plutus code that verifies that a transaction is properly authorized by a Marlowe role to withdraw funds paid by the Semantics Validator. This validator resides in the  `rolePayoutValidator`  function that simply verifies authorization.

Marlowe's property-based and golden tests reside in the `marlowe-test` executable of the [marlowe-test](.) package of the [marlowe-cardano](https://github.com/input-output-hk/marlowe-cardano/) repository, while Marlowe's on-chain tests reside in the [`run-nonpab-test.sh`](../marlowe-cli/run-nonpab-tests.sh) script of the [marlowe-cli](../marlowe-cli/) package of the repository. Informal tests are scattered throughout the Marlowe repositories.


## Formal Guarantees for Marlowe

The theorems in the [Isabelle Specification for Marlowe](https://github.com/input-output-hk/marlowe/tree/master/isabelle) prove the formal guarantees of the Marlowe DSL and semantics:
- Funds are preserved, never locked forever.
- Contracts always close eventually.
- Internal account balances are always positive.
- Applying input to a contract transitions it to a quiescent state where further input is required to transition it further.
- Reducing a contract to quiescence is idempotent.
- Applying a sequence of several inputs has the same end result as applying them separately.
- All contracts terminate.
- All contracts have a maximum lifetime.
- Contracts do not hold funds after they close.
- There is an upper bound on the number of transactions a contract has.

How to run the proofs is described [here](https://github.com/input-output-hk/marlowe/blob/master/README.md#isabelle-proofs).


## Property-Based Tests of Marlowe's Haskell Implementation

The property-based tests (generally using [Tasty](https://hackage.haskell.org/package/tasty) and [QuickCheck](https://hackage.haskell.org/package/QuickCheck)) for the Haskell implementation of Marlowe run in the `marlowe-test` executable of the [marlowe](../marlowe/) package and are implemented in the [`Spec.Marlowe`](src/Spec/Marlowe/) modules. The property-based tests are automatically run in the Github CI for the Marlowe-Cardano repository.


### Tests of Marlowe Semantics Implementation

The property-based tests for the Haskell implementation of Marlowe semantics are implemented in the [`Spec.Marlowe.Semantics`](src/Spec/Marlowe/Semantics/) modules. The table below lists categories of these tests and the name of each test.
- The *golden tests* compare the output of the Haskell `computeTransaction` to a constraint-solver analysis of all possible valid paths through a reference set of Marlowe contracts; that analysis is independent of the Haskell implementation. Manually created sets of invalid inputs to the contract are also tested. Additional contracts could easily be added to this category of tests, but it is labor intensive to manually develop the invalid inputs.
- The *holistic tests* verify the response of `computeTransaction` to specific types of invalid inputs and to specific side effects of applying inputs. The testing framework implements a monad that checks preconditions, invariants, and postconditions in the transaction computation that is being tested. This makes the test cases succinct and readable: only minimal effort would be required to include additional tests.
- The tests of *support functions* used by `computeTransaction` exhaustively cover each behavior of nearly every function. Other semantic functions like `applyInputs`, `reduceUntilQuiescent`, etc. are tested indirectly by exhaustively testing `computeTransaction`.
- Finally, tests of the *suitability of data generation* (via [`Arbitrary`](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Arbitrary.html) and [`Gen`](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html#t:Gen)) ensure that the test inputs are neither too repetitious nor too unrealistic. [We compute the entropy of the diversity of values generated by `arbitrary`](src/Spec/Marlowe/Semantics/Entropy.hs) to ensure that central, realistic, edge, simple, and complex cases are generated. Semantically interesting test cases should have meaningful and stressful correlation between `Environment`, `State`, `Contract`, and `Input`. To achieve this, we have implemented a `SemiArbitrary` typeclass that uses a carefully generated arbitrary  `Context` from which state, contract, input, and other values are drawn, but with some probability those values might be perturbed or completely random. Further work in generating correlated test values is warranted, since this would yield more demonstrably stressful test values.

We have used Haskell code-coverage profiling ([HPC](https://wiki.haskell.org/Haskell_program_coverage)) to confirm that 100% of the logic paths in the [`Language.Marlowe.Core.V1.Semantics`](../marlowe/src/Language/Marlowe/Core/V1/Semantics/) module are explored by the tests. There are, however, a couple of minor code fragments, such as the text of particular warning messages, that aren't evaluated.

The table below summarizes the property-based tests for the semantics implementation.

| Category | Subcategory | Test |
|----------|-------------|------|
| Golden tests of specific contracts | Escrow | Valid inputs |
|                                    |        | Invalid inputs |
|                                    | Pangram | Valid inputs |
|                                    |         | Invalid inputs |
|                                    | Swap | Valid inputs |
|                                    |      | Invalid inputs |
|                                    | Trivial | Valid inputs |
|                                    |         | Invalid inputs |
|                                    | Zero-Coupon Bond | Valid inputs |
|                                    |                  | Invalid inputs |
| Semantics Oracle | Escrow | |
|                  | Forward | |
|                  | Future | |
|                  | Swap | |
|                  | Trivial | |
|                  | Zero-Coupon Bond | |
| Holistic tests of `computeTransaction` | Detect invalid time interval | |
|                                        | Detect time interval in past | |
|                                        | Ambiguous interval for timeout | |
|                                        | Applying no inputs is useless until timeout | |
|                                        | Closing no accounts is useless | |
|                                        | `Pay` all accounts on close | |
|                                        | No matching input | |
|                                        | `Let` sets variable | |
|                                        | `If` branches | |
|                                        | `Assert` warns | |
|                                        | Static-analysis input | |
| Functions called by `computeTransaction` | `fixInterval` | Invalid interval |
|                                          |               | Interval in past |
|                                          |               | Interval trimmed |
|                                          | `evalValue` | `AvailableMoney` |
|                                          |             | `Constant` |
|                                          |             | `NegValue` |
|                                          |             | `AddValue` |
|                                          |             | `SubValue` |
|                                          |             | `MulValue` |
|                                          |             | `DivValue` |
|                                          |             | `ChoiceValue` |
|                                          |             | `TimeIntervalStart` |
|                                          |             | `TimeIntervalEnd` |
|                                          |             | `UseValue` |
|                                          |             | `Cond` |
|                                          | `evalObservation` | `AndObs` |
|                                          |                   | `OrObs` |
|                                          |                   | `NotObs` |
|                                          |                   | `ChoseSomething` |
|                                          |                   | `ValueGE` |
|                                          |                   | `ValueGT` |
|                                          |                   | `ValueLT` |
|                                          |                   | `ValueLE` |
|                                          |                   | `ValueEQ` |
|                                          |                   | `TrueObs` |
|                                          |                   | `FalseObs` |
|                                          | `refundOne` | No accounts |
|                                          |             | One account |
|                                          |             | Multiple accounts |
|                                          |             | Non-positive account |
|                                          | `moneyInAccount` | |
|                                          | `updateMoneyInAccount` | |
|                                          | `addMoneyToAccount` | |
|                                          | `giveMoney` | |
|                                          | `reduceContractStep` | `Close` |
|                                          |                      | `Pay` |
|                                          |                      | `If` |
|                                          |                      | `When` |
|                                          |                      | `Let` |
|                                          |                      | `Assert` |
|                                          | `reduceContractUntilQuiescent` | |
|                                          | `applyAction` | `Input` does not match action |
|                                          |               | `IDeposit` |
|                                          |               | `IChoice` |
|                                          |               | `INotify` |
|                                          | `getContinuation` | |
|                                          | `applyCases` | |
|                                          | `applyInput` | |
|                                          | `isClose` | |
|                                          | `notClose` | |
|                                          | `playTrace` | |
| Suitability of `Arbitrary` instances | Sufficient entropy | `PubKeyHash` |
|                                      |                    | `CurrencySymbol` |
|                                      |                    | `TokenName` |
|                                      |                    | `Token` |
|                                      |                    | `Party` |
|                                      |                    | `ChoiceName` |
|                                      |                    | `ChoiceId` |
|                                      |                    | `ValueId` |
|                                      |                    | `accounts` |
|                                      |                    | `choices` |
|                                      |                    | `boundValues` |


### ACTUS Contract Tests

The [marlowe-actus](../marlowe-actus/) package runs Marlowe contracts derived from the [ACTUS](https://github.com/actusfrf/actus-tests) test suite: namely, the examples
- Principal at maturity (PAM),
- Linear Amortizer (LAM),
- Negative Amortizer (NAM),
- Annuity (ANN),
- Option (OPTNS), and
- Commodity (COM).
These tests utilize `computeTransaction` to execute the Marlowe contract, and the output is compared to the expected ACTUS results.


### Tests of Plutus Functions Used by the Marlowe Implementation

Marlowe's Haskell implementation relies on the Plutus Haskell functions behaving as advertised, but some Plutus documentation is terse. (There is at least [one documented historical case where a misinterpretation of the behavior of a Plutus function was not detected during a Dapp audit](https://www.tweag.io/blog/2022-03-25-minswap-lp-vulnerability/), with the result that a Dapp containing a critical vulnerability was deployed on `mainnet` .) To supplement the preexisting tests of Plutus, we have written independent tests for the Plutus functions that Marlowe's validators use: see the table below. Particular emphasis was placed on `AssocMap` which is an association list that is used in Marlowe's Haskell as a map with unique keys. (Notably, `AssocMap` arguably has a misleading `Eq` instances and some other odd behavior.) Furthermore, the multiasset `Value`-comparison function, `(==)`, `leq` and `geq` are critical to Marlowe. Finally, inadequate documentation of `findDatum` and `findDatumHash` in Plutus V1 had formerly caused validation failures in Marlowe.

| Module | Test |
|--------|------|
| `PlutusTx.Prelude` | `snd` matches standard prelude |
|                    | `(&&)` matches standard prelude |
|                    | `any` matches standard prelude |
|                    | `all` matches standard prelude |
|                    | `fmap` matches standard prelude |
|                    | `foldMap` matches standard prelude |
|                    | `filter` matches standard prelude |
|                    | `find` matches standard prelude |
|                    | `instance Eq (Maybe a)` matches standard prelude |
|                    | `instance Eq DatumHash` |
|                    | `instance Eq ValidatorHash` |
|                    | `instance Eq TxOutRef` |
| `PlutusTx.AssocMap` | `fromList` is the inverse of `toList` |
|                     | `toList` is the inverse of `fromList` |
|                     | `empty` creates an empty list |
|                     | `null` detects an empty list |
|                     | `singleton` creates a one-item list |
|                     | `member` detects a key |
|                     | `lookup` finds a value |
|                     | `insert` adds a value |
|                     | `delete` removes a key |
| `Plutus.V2.Ledger.Value` | `mempty` has no tokens |
|                          | `mappend` sums corresponding tokens |
|                          | `(==)` detects equality |
| `Plutus.V2.Ledger.Value` | `leq` is partial order |
|                          | `geq` is partial order |
|                          | `valueOf` extracts quantity |
|                          | `singleton` creates a single token |
|                          | `zero` has no value |
| `Plutus.V2.Ledger.Contexts` | `findDatum` |
|                             | `findDatumHash` |
|                             | `txSignedBy` |
|                             | `valuePaidTo` |
|                             | `valueSpent` |


### Tests of Marlowe's Plutus Validators

Requirements for bridging `computeTransaction` to Plutus validation are defined as seventeen constraining properties in the [On-Chain Transaction Specification for Marlowe](../marlowe/specificiation/marlowe-cardano-specification.md). These property based tests run arbitrary transactions using Marlowe's serialized Plutus validators via `Plutus.ApiCommon.evaluateScriptCounting`: this provides a mock execution environment that performs the same Phase 2 validation that would occur at a node on the live blockchain. To this end, the [`Spec.Marlowe.Plutus.Script`](src/Spec/Marlowe/Plutus/Script.hs) package provides a pair of functions for simulating Marlowe transactions:
```haskell
-- | Run the Plutus evaluator on the Marlowe semantics validator's UPLC serialization.
evaluateSemantics :: MarloweParams           -- ^ The parameters.
                  -> Data                    -- ^ The datum.
                  -> Data                    -- ^ The redeemer.
                  -> Data                    -- ^ The script context.
                  -> These String LogOutput  -- ^ The result, including an error message and/or logging messages.

-- | Run the Plutus evaluator on the Marlowe payout validator's UPLC serialization.
evaluatePayout :: MarloweParams           -- ^ The parameters.
               -> Data                    -- ^ The datum.
               -> Data                    -- ^ The redeemer.
               -> Data                    -- ^ The script context.
               -> These String LogOutput  -- ^ The result, including an error message and/or logging messages.
```
This allows one to feed arbitrary datum, redeemer, and script context to either of the Marlowe validators, where those validators have previously been serialized to `ShortByteString`. This executes Marlowe directly as Untyped Plutus Core (UPLC).

The [`Spec.Marlowe.Plutus.Types`](src/Spec/Marlowe/Plutus/Types.hs) module includes data structures for representing Plutus transactions in general and Marlowe transactions specifically:
```haskell
-- | A Plutus transaction.
data PlutusTransaction a =
  PlutusTransaction
  {
    _parameters    :: a              -- ^ Parameters providing context.
  , _datum         :: Datum          -- ^ The datum.
  , _redeemer      :: Redeemer       -- ^ The redeemer.
  , _scriptContext :: ScriptContext  -- ^ The script context.
  }

-- | A Marlowe semantics transaction.
data SemanticsTransaction =
  SemanticsTransaction
  {
    _params   :: MarloweParams      -- ^ The parameters.
  , _state    :: State              -- ^ The incoming state.
  , _contract :: Contract           -- ^ The incoming contract.
  , _input    :: TransactionInput   -- ^ The transaction input.
  , _output   :: TransactionOutput  -- ^ The transaction output.
  }

-- | A Marlowe payout transaction.
data PayoutTransaction =
  PayoutTransaction
  {
    _params' :: MarloweParams  -- ^ The parameters.
  , _role    :: TokenName      -- ^ The role name.
  , _amount  :: Value          -- ^ The value paid.
  }
```
In [`Spec.Marlowe.Plutus.Transaction`](src/Spec/MarlowePlutus/Transaction.hs), functions are provided to generate arbitrary Marlowe transactions which are initially valid, but which may be modified before the construction process or afterwards.
```haskell
-- | An arbitrary Plutus transaction.
type ArbitraryTransaction p a = StateT (PlutusTransaction p) Gen a

-- | Generate a valid Marlowe semantics transaction.
validSemanticsTransaction :: Bool                                          -- ^ Whether to add noise to the script context.
                          -> ArbitraryTransaction SemanticsTransaction ()  -- ^ The generator.

-- | Generate an arbitrary, valid Marlowe semantics transaction: datum, redeemer, and script context.
arbitrarySemanticsTransaction :: ArbitraryTransaction SemanticsTransaction ()  -- ^ Modifications to make before building the valid transaction.
                              -> ArbitraryTransaction SemanticsTransaction ()  -- ^ Modifications to make after building the valid transaction.
                              -> Bool                                          -- ^ Whether to add noise to the script context.
                              -> Gen (PlutusTransaction SemanticsTransaction)  -- ^ The generator.

-- | Generate a valid Marlowe payout transaction.
validPayoutTransaction :: Bool                                       -- ^ Whether to add noise to the script context.
                       -> ArbitraryTransaction PayoutTransaction ()  -- ^ The generator.

-- | Generate an arbitrary, valid Marlowe payout transaction: datum, redeemer, and script context.
arbitraryPayoutTransaction :: ArbitraryTransaction PayoutTransaction ()  -- ^ Modifications to make before building the valid transaction.
                           -> ArbitraryTransaction PayoutTransaction ()  -- ^ Modifications to make after building the valid transaction.
                           -> Bool                                       -- ^ Whether to add noise to the script context.
                           -> Gen (PlutusTransaction PayoutTransaction)  -- ^ The generator.
```
Each test case modifies the arbitrary valid transaction using actions within `ArbitraryTransaction PayoutTransaction ()` that tailor the test case to the property being tested. Lenses are provided for simplifying the process of succinctly modifying the prospective transaction within its state monad. The "noising" switch provides the option to generate a transaction that just includes the bare-bones content for Marlowe versus a transaction that contains additional inputs, outputs, datum, and signatures that are not strictly necessary for the Marlowe transaction to be valid.


#### Semantics Validator

We use the aforementioned simulation technique to test properties constraining the semantics validator, `marloweValidator` in [`Language.Marlowe.Scripts`](../marlowe/src/Language/Marlowe/Scripts.hs). The first tests run arbitrary valid transactions; the later tests run transactions that must fail for particular reasons. In three cases manual code inspection was used instead of a property-based tests: implementing those tests would have involved instrumenting the Plutus validator itself.

| Property | Test |
|----------|------|
| Valid transaction succeeds | Noiseless |
|                            | Noisy |
| Constraint 1. Typed validation | Valid datum deserializes |
|                                | Valid redeemer deserializes |
|                                | Valid script context deserializes |
|                                | Invalid datum does not deserialize |
|                                | Invalid redeemer does not deserialize |
|                                | Invalid script context does not deserialize |
| Constraint 2. Single Marlowe script input | Invalid attempt to run two Marlowe scripts |
| Constraint 3. Single Marlowe output | Invalid attempt to split Marlowe output |
| Constraint 4. No output to script on close | Invalid attempt to output to Marlowe on close |
| Constraint 5. Input value from script | Invalid mismatch between state and script input |
| Constraint 6. Output value to script | Invalid mismatch between state and script output |
| Constraint 7. Input state | (Manual code inspection instead of property-based test) |
| Constraint 8. Input contract | (Manual code inspection instead of property-based test) |
| Constraint 9. Marlowe parameters | (Manual code inspection instead of property-based test) |
| Constraint 10. Output state | Invalid mismatch between state and script output |
| Constraint 11. Output contract | Invalid mismatch between contract and script output |
| Constraint 12. Merkleized continuations | Mismatch between continuation and its merkleization |
| Constraint 13. Positive balances | Invalid non-positive balance |
| Constraint 14. Inputs authorized | Invalid missing authorization |
| Constraint 15. Sufficient payment | Invalid insufficient payment |


#### Payout Validator

We use the aforementioned simulation technique to test properties constraining the payout validator, `rolePayoutValidator` in [`Lanugage.Marlowe.Scripts`](../marlowe/src/Language/Marlowe/Scripts.hs). The first tests run arbitrary valid transactions; the later tests run transactions that must fail for particular reasons.

| Property | Test |
|----------|------|
| Valid transaction succeeds | Noiseless |
|                            | Noisy |
| Constraint 16. Typed validation | Valid datum deserializes |
|                                 | Valid redeemer deserializes |
|                                 | Valid script context deserializes |
|                                 | Invalid datum does not deserialize |
|                                 | Invalid redeemer does not deserialize |
|                                 | Invalid script context does not deserialize |
| Constraint 17. Payment authorized | Invalid authorization for withdrawal |
|                                   | Missing authorization for withdrawal |


### Miscellaneous Property-Based Tests

Several other property-based tests, in the [`Spec.Marlowe.Marlowe`](src/Spec/Marlowe/Marlowe.hs) module, exercise parts of the Marlowe Haskell implementation:

| Category | Test |
|-----------|------|
| Serialization | Token Show instance respects HEX and Unicode |
| | Pangram Contract serializes into valid JSON |
| | State serializes into valid JSON |
| | Input serializes into valid JSON |
| | `Value`s can be serialized to JSON |
| Validator Size | Validator size is reasonable |
| | Typed validator size |
| | Untyped validator size |
| On-chain arithmetic | `Mul` analysis |
| | `Div` analysis |
| | `Div` tests |
| | `Value` equality is reflexive, symmetric, and transitive |
| | `Value` double negation |
| | `Value`s form Abelian group |
| | Multiply by zero |
| | Divide zero and by zero |
| | `DivValue` rounding |
| State | Transfers between accounts work |
| Contract | `extractContractRoles` |
| Static Analysis | No false positives |
| Address serialisation | Compare to Cardano API serialisation to Bech32 |
| | Serialise to bytes then deserialise |
| | Serialise to Bech32 then deserialise |
| Party serialization | Serialise toJSON then deserialise |
| Core Contract Serialization | Serialise deserialise Contract loops |
| | Serialise deserialise MarloweParams loops |
| | Serialise deserialise IntervalError loops |
| Extended Contract Serialization | Golden Swap contract |
| | Golden Swap module |
 


## Test Oracle for Marlowe

A test oracle service, based on Marlowe's PureScript export from Isabelle is available for testing the Haskell implementation. This is currently work in progress.


## On-Chain Tests of Marlowe

On-chain tests of Marlowe mostly center on verifying the execution of valid transactions, but some tests verify that invalid transactions fail. Formal tests are codified as scripts for the [`marlowe-cli`](../marlowe-cli/ReadMe.md) tool or as scripts for Marlowe's testing DSL.


### Formal Tests

A dozen on-chain tests, embodied as `bash` scripts, are regularly run for quality assurance on test networks such as `preview` (magic = 2) and `pioneers` (magic = 1567).

| File | Test |
|-----|------|
| [marlowe-cli/test/double-satisfaction.sh](../marlowe-cli/test/double-satisfaction.sh) | Double-satisfaction, where one Marlowe contract steals funds from another. |
| [marlowe-cli/examples/actus/run-actus.sh](../marlowe-cli/examples/actus/run-actus.sh) | An ACTUS contract (Zero-Coupon Bond). |
| [marlowe-cli/examples/cfd/run-cfd.sh](../marlowe-cli/examples/cfd/run-cfd.sh) | A contract for differences, using an oracle. |
| [marlowe-cli/examples/coveredCall/run-coveredCall.sh](../marlowe-cli/examples/coveredCall/run-coveredCall.sh) | A covered-call contract. |
| [marlowe-cli/examples/escrow/run-confirm-claim.sh](../marlowe-cli/examples/escrow/run-confirm-claim.sh) | The "confirm claim" path through the example escrow contract. |
| [marlowe-cli/examples/escrow/run-confirm-problem.sh](../marlowe-cli/examples/escrow/run-confirm-problem.sh) | The "confirm problem" path through the example escrow contract. |
| [marlowe-cli/examples/escrow/run-dismiss-claim.sh](../marlowe-cli/examples/escrow/run-dismiss-claim.sh) | The "dismiss claim" path through the example escrow contract. |
| [marlowe-cli/examples/escrow/run-everything-is-alright.sh](../marlowe-cli/examples/escrow/run-everything-is-alright.sh) | The "everything is alright " path through the example escrow contract. |
| [marlowe-cli/examples/simple/run-simple.sh](../marlowe-cli/examples/simple/run-simple.sh) | A simple notify-then-close contract. |
| [marlowe-cli/examples/simpleSelcoins/run-simpleSelcoins.sh](../marlowe-cli/examples/simpleSelcoins/run-simpleSelcoins.sh) | A variant of the simple notify-and-close test. |
| [marlowe-cli/examples/swap/run-swap.sh](../marlowe-cli/examples/swap/run-swap.sh) | A swap of two tokens. |
| [marlowe-cli/examples/zcb/run-zcb.sh](../marlowe-cli/examples/zcb/run-zcb.sh) | A zero-coupon bond. |

A new off- and on-chain testing DSL is being readied for release (executable via `marlowe-cli test script`) and will include tests migrated from Marlowe's previous PAB-based testing DSL:

| File | Test |
|-----|------|
| [marlowe-cli/test/test-contract-for-differences-with-oracle.yaml](../marlowe-cli/test/test-contract-for-differences-with-oracle.yaml) | A contract for differences, with an oracle. |
| [marlowe-cli/test/test-contract-for-differences.yaml](../marlowe-cli/test/test-contract-for-differences.yaml) | A contract for differences, without an oracle. |
| [marlowe-cli/test/test-coupon-bond-guaranteed.yaml](../marlowe-cli/test/test-coupon-bond-guaranteed.yaml) | A guaranteed bond. |
| [marlowe-cli/test/test-escrow-with-collateral.yaml](../marlowe-cli/test/test-escrow-with-collateral.yaml) | An escrow contract that includes collateral. |
| [marlowe-cli/test/test-escrow.yaml](../marlowe-cli/test/test-escrow.yaml) | An escrow contract that does not include collateral. |
| [marlowe-cli/test/test-simple.yaml](../marlowe-cli/test/test-simple.yaml) | A simple notify-then-close contract. |
| [marlowe-cli/test/test-swap-of-ada-and-dollar-tokens.yaml](../marlowe-cli/test/test-swap-of-ada-and-dollar-tokens.yaml) | A swap of Ada for USD tokens. |
| [marlowe-cli/test/test-swap-of-ada-for-ada.yaml](../marlowe-cli/test/test-swap-of-ada-for-ada.yaml) | A swap of Ada for Ada. |
| [marlowe-cli/test/test-zero-coupon-bond-delayed-timeout.yaml](../marlowe-cli/test/test-zero-coupon-bond-delayed-timeout.yaml) | A zero-coupon bond where a timeout occurs late in the contract. |
| [marlowe-cli/test/test-zero-coupon-bond-immediate-timeout.yaml](../marlowe-cli/test/test-zero-coupon-bond-immediate-timeout.yaml) | A zero-coupon bond where a timeout occurs early in the contract. |
| [marlowe-cli/test/test-zero-coupon-bond-too-late.yaml](../marlowe-cli/test/test-zero-coupon-bond-too-late.yaml) | A zero-coupon bond where input fails due to a timeout. |
| [marlowe-cli/test/test-zero-coupon-bond.yaml](../marlowe-cli/test/test-zero-coupon-bond.yaml) | A zero-coupon bond. |

Two test scenarios are provided as Jupyter notebooks:

| File | Test |
|-----|------|
| [marlowe-cli/test/min-ada.ipynb](../marlowe-cli/test/min-ada.ipynb) | Execution failure due to insufficient minimum UTxO value. |
| [marlowe-cli/test/exceed-protocol-parameters.ipynb](../marlowe-cli/test/exceed-protocol-parameters.ipynb) | Execution failure due to exceeding protocol limits. |


### Informal Tests

A set of informal tests are provided in the [Marlowe Contract Cookbook](../marlowe-cli/cookbook/):

| File | Test |
|-----|------|
| [marlowe-cli/cookbook/collective-loan-30-70.ipynb](../marlowe-cli/cookbook/collective-loan-30-70.ipynb) | A collective loan involving over 500 transactions. |
| [marlowe-cli/cookbook/collective-loan.ipynb](../marlowe-cli/cookbook/collective-loan.ipynb) | A collective loan with ad-hoc distribution of role tokens. |
| [marlowe-cli/cookbook/english-auction.ipynb](../marlowe-cli/cookbook/english-auction.ipynb) | An English Auction involving massive merkleization. |
| [marlowe-cli/cookbook/guessing-game.ipynb](../marlowe-cli/cookbook/guessing-game.ipynb) | A guessing game involving complex arithmetic. |
| [marlowe-cli/cookbook/payment-using-djed.ipynb](../marlowe-cli/cookbook/payment-using-djed.ipynb) | Payments using the DJED stablecoin. |
| [marlowe-cli/cookbook/reference-script.ipynb](../marlowe-cli/cookbook/reference-script.ipynb) | Using CIP-33 reference scripts with Marlowe. |
| [marlowe-cli/cookbook/revenue-based-loan.ipynb](../marlowe-cli/cookbook/revenue-based-loan.ipynb) | A revenue-based loan. |
| [marlowe-cli/cookbook/simple-babbage.ipynb](../marlowe-cli/cookbook/simple-babbage.ipynb) | A proof of Marlowe in the Babbage Era. |
| [marlowe-cli/cookbook/stabilized-collective-loan.ipynb](../marlowe-cli/cookbook/stabilized-collective-loan.ipynb) | A stabilized collective loan that exceeds protocol limits. |
| [marlowe-cli/cookbook/swap.ipynb](../marlowe-cli/cookbook/swap.ipynb) | A swap contract with automated coin selection. |
| [marlowe-cli/cookbook/zcb.ipynb](../marlowe-cli/cookbook/zcb.ipynb) | A zero-coupon bond contract with automated coin selection. |


## Testing Tools

- Property-based tests use the Haskell [Tasty](https://hackage.haskell.org/package/tasty) and [QuickCheck](https://hackage.haskell.org/package/QuickCheck) packages.
    - `marlowe-test` runs the main semantics and validator tests.
    - `marlowe-actus-test` runs the ACTUS tests.
- Scripted on-chain tests use `marlowe-cli` commands in `bash` scripts or its embedded testing DSL accessible via `marlowe-cli test script`.
- Informal on- and off-chain tests use Jupyter notebooks.
- Code coverage was checked using the GHC [HPC](https://wiki.haskell.org/Haskell_program_coverage) capability.


## Summary of Testing History and Results

The Marlowe validator implementation currently passes all property-based and on-chain tests. The testing code and process outlined above revealed several issues that were further scrutinized:
1. The test for ["Constraint 4: No output to script on close"](../marlowe/specificiation/marlowe-cardano-specification.md#constraint-4-no-output-to-script-on-close) of the Marlowe-Cardano specification originally failed. This failure was eliminated with a minor modification to the semantics validator.
2. Testing revealed an ambiguity regarding how a payment of no value should be warned as `ReduceNonPositivePay` versus `ReducePartialPay`. In documentation, we clarify that an intentional payment of zero results in the warning `ReduceNonPositivePay`, which means "the contract was meant to pay zero or less but that is impossible so it pays nothing", whereas a payment that happens to be zero because of insufficient funds results in the warning `ReducePartialPay`, which means "the contract is meant to pay more than there is, so it pays what there is".
3. The `computeTransaction` function does not guard for the preconditions (1) that internal account balances are positive and (2) that duplicate `(Account, Token)` pairs are not present in the association map `accounts :: PlutusTx.Map (Account, Token) Integer`.
    1. Marlowe's semantics validator *guards* against the first case by failing to execute any transaction that begins with a non-positive balance in an internal account. *This causes permanent locking of funds in the contract.*
    2. The semantics validator *does not guard* against duplicate `accounts`, `choices`, or `boundValues` in the incoming `State`, in which circumstance an invalid initial state would be provided to `computeTransaction`, resulting in behavior that violates the semantics specification. *This potentially causes semantically invalid behavior of the contract.* In general, we have not tested Marlowe semantics when it is presented with a state that violates the preconditions for its proofs.
4. A role-based payment to a party with a blank token name in a contract where no roles currency was defined (effectively making Ada the roles currency) can be withdrawn from the payout validator by anyone at all.
5. Slot number to POSIX millisecond time conversion from the node to the Plutus script context necessitates meticulous attention to time values in Marlowe contracts. In some cases, sub-second mismatches will cause a validation failure, but that can be mitigated by resubmitting the transaction with the correct slot-number validity interval.
6. The POSIX-millisecond validity intervals in the Plutus script context may be open, closed, or half-open/half-closed, but the slot-number validity intervals at the node are always closed on the left and open on the right. This situation has a potential (similar to Issue 5 above) to make the temporal aspect of validation finicky.
7. Input that is valid off-chain may not be valid on-chain due to interactions between Marlowe semantics and the Cardano ledger rules. See the [next subsection](#funds-locked-or-input-blocked-due-to-exceeding-protocol-parameters) for details.

Issues 1 and 2 above have been eliminated. Issues 3.1, 3.2, 4, and 7 must be dealt with via linting and static-analysis tools that warn users of contract vulnerabilities. Issues 5 and 6 are under study, and likely require more detailed documentation with guidance for submitting well-formed Marlowe transactions.


### Funds Locked or Input Blocked Due to Exceeding Protocol Parameters

Funds in a Marlowe contract on the Cardano blockchain may be forever *locked* (inaccessible) if all possible transactions exceed protocol parameters for script execution. Similarly, certain paths of contract execution will be *blocked* if some (but not all) semantically allowable transactions exceed protocol limits. This behavior is demonstrated on-chain in [marlowe-cli/test/exceed-protocol-parameters.ipynb](../marlowe-cli/test/exceed-protocol-parameters.ipynb).

The following protocol parameters affect the viability of semantically-valid Marlowe transactions.
1. Transaction size (`maxTxSize`)
    1. A `Pay` might exceed transaction size if the payments it makes require a `ScriptContext` that makes the transaction too large.
    2. Similarly, a `Close` might result in numerous payments that enlarge the `ScriptContext`.
    3. It might be impossible to demerkleize a `Case` term within a `When` because the continuation of the contract is too large.
    4. A `Let` or `Choice` might increase the `Datum` size.
2. Execution steps (`maxTxExecutionUnits.steps`)
    1. A `Pay`, `If`, `Let`, or `Assert` might require an extensive evaluation of `Value` s and `Observations`, thus exceeding execution-step limits.
    2. A `When` might include many `Case`s, so that evaluating the `Action`s within them (any of which might require extensive evaluation) exhausts the execution-step budget.
    3. A `Close` is unlikely to exceed execution steps unless it triggers too many payments.
    4. Because the execution of `Contract` terms is continued until a quiescent state is reached, the sequence of continuations might exceed step limits even though individual terms do not.
3. Execution memory (`maxTxExecutionUnits.memory`)
    1. The same considerations apply to execution memory as to execution steps.
    2. This includes the sequence of `Contract` continuations mentioned above because execution memory is the measured as a *sum* not a *maximum* over the execution's steps.
4. Minimum Ada in a UTxO (`utxoCostPerWord`)
    1. A `Deposit` might add tokens to the script address in excess of the minimum Ada requirement for the script's UTxO.
5. Value size (`maxValueSize`)
    1. A `Pay` might send so many token in the same UTxO that the UTxO exceeds the maximum value size.
    2. A `Deposit` might place so many tokens at the script address that the UTxO exceeds the maximum value size.

The table below summarizes the situation. Items are marked "locks" if they would prevent further execution of the contract if they are the top-level construct or the timeout continuation of a `When` contract; otherwise, they just block some (but not all) execution paths for the contract. They are marked "blocks" if they can never lock the contract.

| Protocol Limit   | Protocol Parameter           | `Pay` | `Let` | `If`/`Assert` | `When` | `Close` | `Deposit` | `Choice` | `Notify` |
|------------------|------------------------------|-------|-------|---------------|--------|---------|-----------|----------|----------|
| Transaction size | `maxTxSize`                  | locks | locks |               | blocks | locks   |           | blocks   |          |
| Execution steps  | `maxTxExecutionUnits.steps`  | locks | locks | locks         | blocks | locks   | blocks    | blocks   | blocks   |
| Execution memory | `maxTxExecutionUnits.memory` | locks | locks | locks         | blocks | locks   | blocks    | blocks   | blocks   |
| Minimum ADA      | `utxoCostPerWord`            |       |       |               |        |         | blocks    |          |          |
| Value size       | `maxValueSize`               | locks |       |               |        |         | blocks    |          |          |

Some of these limitations might be remedied by modifying the Cardano-Marlowe specification to allow more complex transactions (e.g., splitting payments so as to avoid the `maxValueSize` limit), but that would be at the expense of making the semantics validator quite a bit larger and more costly. Many of the limitations cannot be remedied by without a complete redesign of both the Plutus code for Marlowe and the `computeTransaction` implementation: it is likely that protocol parameters may limit *any* Marlowe transaction on Cardano. This highlights the importance of performing static analysis of a Marlowe contract *before* creating the contract on the blockchain. To verify that a contract will not experience these failure modes, one may need to attempt to execute off-chain every possible path the contract could take during its complete execution or equivalently prove that execution would not violate protocol limits.


## Areas of Concern

Our experience testing Marlowe's on-chain code highlights several areas of concern:
1. *Gap between Isabelle proofs and Haskell implementation:*
    1. The Haskell implementation was not created with a verified code generator. (The new "test oracle" for Marlowe will somewhat alleviate this situation by enabling direct comparison of Isabelle-guaranteed behavior against the Marlowe-Cardano implementation in Haskell.)
    2. Merkleization is absent from the Isabelle semantics. (The simplicity of the merkleization approach and extensive off- and on-chain testing of merkleization provides fairly strong assurance that the merkleization does not affect semantics, however.)
2. *Limitations of property-based testing:*
    1. We use a limited set of golden test cases.
    2. We currently do not have a technique for generating large numbers of nearly-valid transactions.
    3. Correlations between `State`, `Contract`, `Input`, and `Environment` are extremely important for arbitrary-value generation that adequately explores realistic cases and edge cases.
    4. Marlowe's test oracle will be based on the Isabelle export of Haskell code rather than directly executing the Isabelle specification itself.
    5. Although the constraints in the Marlowe-Cardano specifications typically interact linearly, we do not have test cases that examine nonlinear interactions between violations of the specified constraints.
3. *Protocol limits:*
    1. We have demonstrated that semantic guarantees like "funds cannot be locked forever in a Marlowe contract" do not hold on the Cardano blockchain.
    2. No tools exist to vet Marlowe contracts for potential protocol-limit violations.
    3. Protocol limits may change in the future.
4. *Time:*
	1. The conversion from slot numbers to POSIX milliseconds is requires meticulous attention in constructing valid transactions.
	2. The script context may have open, closed, or mixed validity intervals for POSIX milliseconds, but the node has a half-open validity interval for slot numbers.
5. *Contract threads:*
    1. The Marlowe validators do not use the "thread NFT" paradigm where the presence of a verifiably unique non-fungible token (NFT) guards against double satisfaction attacks.
    2. Instead, the Marlowe semantics validator restrictively forbids multiple Marlowe scripts from running in the same transaction.
6. *Monetary policy:*
    1. Marlowe contracts use role tokens, but the Marlowe specifications and validators do not enforce any particular monetary policy.
    2. Monetary policy was omitted from the Marlowe-Cardano specification in order to provide flexibility in use cases, but it is possible to enforce a specific monetary policy for Marlowe.
7. *Static analysis and linting:*
    1. Marlowe's existing static analysis cannot handle large contracts and it does not handle merkleization.
    2. No linting tool has yet been written for Marlowe to detect invalid initial state and other fatal conditions.
8. *Plutus:*
    1. The Plutus compiler has not been formally verified.
