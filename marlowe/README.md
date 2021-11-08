# Marlowe: financial contracts on Cardano Computation Layer

Here we present a reference implementation of Marlowe, domain-specific language targeted at
the execution of financial contracts in the style of Peyton Jones et al
on Cardano Computation Layer.

The implementation is based on semantics described in paper
['Marlowe: financial contracts on blockchain'](https://iohk.io/research/papers/#2WHKDRA8)
by Simon Thompson and Pablo Lamela Seijas

To run tests, from this folder:
```bash
$(nix-build ../default.nix -A marlowe.haskell.packages.marlowe.components.tests.marlowe-test)/bin/marlowe-test
```

