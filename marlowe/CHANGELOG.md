# Changelog for the `marlowe-cardano` Package


## Start of changelog, December 2022

The tag SCP-4415 (commit `23f3d56f22bf992ddb0b0c8a52bb7a5a188f9e9`) marks the version of this package that was subject to audit.


## SCP-5126: Differences between Marlowe's Isabelle semantics and its Plutus validator

Based on audit findings, we annotated Marlowe's Plutus validator with comments indicating how differences between `PlutusTx.AssocMap` and Isabelle's `MList` are inconsequential with respect to behavior of the validator, provided the Marlowe contract's initial state does not contain duplicate accounts, choices, or bound values.


## SCP-5123: Semantically negative deposits do not withdraw funds from the script

Based on audit findings, we corrected the accounting equation so that negative deposits are treated as zero by the validator. This prevents a negative deposit from removing value from the script's UTxO, which would have left the total value in accounts unequal to the value in the script UTxO.
