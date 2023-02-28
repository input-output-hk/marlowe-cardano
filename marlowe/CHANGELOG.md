# Changelog for the `marlowe-cardano` Package


## Start of changelog, December 2022

The tag SCP-4415 (commit `23f3d56f22bf992ddb0b0c8a52bb7a5a188f9e9`) marks the version of this package that was subject to audit.


## SCP-5126: Differences between Marlowe's Isabelle semantics and its Plutus validator

Based on audit findings, we annotated Marlowe's Plutus validator with comments indicating how differences between `PlutusTx.AssocMap` and Isabelle's `MList` are inconsequential with respect to behavior of the validator, provided the Marlowe contract's initial state does not contain duplicate accounts, choices, or bound values.

