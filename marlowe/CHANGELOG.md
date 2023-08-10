
<a id='changelog-0.1.2.0'></a>
# 0.1.2.0 — 2023-08-10

## Added

- "next" possibilities for a given contract

- JSON instances for safety-analysis types.

## Fixed

- Execution-cost analysis of merkleized contracts (PLT-6907).

<a id='changelog-0.1.1.0'></a>
# 0.1.1.0 — 2023-06-15

## Added

- Safety checks for Marlowe contracts.

- Implemented `marlowe-validators` executable that benchmark Plutus execution costs of the Marlowe validators.
- Instrumeted `marlowe-test` to optionally generate benchmark cases.

- Safety checks for valid Plutus addresses.

## Changed

PLT-5462 adds a cabal flag `--flag trace-plutus` for the `marlowe-cardano` and `marlowe-test` packages. In production this flag should not be set because it enlarges the validator and changes its hash. If the flag is turned on, however, the test suite `test:marlowe-test` will check the Plutus logs to see that the correct error is occurring in cases where the validator should fail.

This flag is also useful in `marlowe-cli` for creating a tracing validator for on-chain usage.


# SCP-5126: Differences between Marlowe's Isabelle semantics and its Plutus validator

Based on audit findings, we annotated Marlowe's Plutus validator with comments indicating how differences between `PlutusTx.AssocMap` and Isabelle's `MList` are inconsequential with respect to behavior of the validator, provided the Marlowe contract's initial state does not contain duplicate accounts, choices, or bound values.


# SCP-5123: Semantically negative deposits do not withdraw funds from the script

Based on audit findings, we corrected the accounting equation so that negative deposits are treated as zero by the validator. This prevents a negative deposit from removing value from the script's UTxO, which would have left the total value in accounts unequal to the value in the script UTxO.


# SCP-5129: Fixed `(==)` for `ReduceAssertionFailed`

Based on audit finding, we corrected `instance Eq ReduceWarning` so that when a comparison of `ReduceAssertionFailed` to itself yields `True`.


# SCP-5128: Rationale for multiple payments to address parties

Based on audit finding, we added detailed comment in the Marlowe semantics validator about why multiple outputs to an address are allowed.


# SCP-5124: Validator checks consistency of script output and internal accounts

Based on audit findings, we added "Constraint 18. Final balance" to the Marlowe-Cardano specification, requiring that the value output to the script address match the total value of the accounts in the output state, along with a corresponding check in the Marlowe semantics validator.


# SCP-5143: Removed tracing from validator

In order to reduces the validator size, calls to Plutus tracing functions were removed.


# SCP-5141: Valid initial and final states

Based on audit findings, we added "Constraint 19. No duplicates" to the Marlowe-Cardano specification and require that the validator ensure that both the intial and final states of the contract obey the invariants of correct total value, positive balances, and non-duplication of keys for accounts, choices, and bound values.


# SCP-5215: Single statisfaction

Based on audit findings, we added "Constraint 20. Single satisfaction" to the Marlowe-Cardano specification and require that the validator be the only script running in the transaction if any payments are made.


# Start of changelog, December 2022

The tag SCP-4415 (commit `23f3d56f22bf992ddb0b0c8a52bb7a5a188f9e9`) marks the version of this package that was subject to audit.
