# Cardano Integration

This library provides a general-purpose integration testing container for
running tests against a local Cardano testnet.

## Usage

From code:

```hs
myTest = withLocalTestnet \testnet -> do
  -- Test code here
```

From the command line:

```bash
cabal run create-testnet # Note: this must be run from the root directory of the repo.
```

## Why not use `cardano-testnet`?

This library is based on the `cardano-testnet` library / package in
`cardano-node`. So why not just use that? Three reasons:

- `cardano-testnet` is tightly coupled to `hedgehog` integration tests.
- `cardano-testnet` has lots of unnecessary cabal dependencies.
- `cardano-testnet` is incomplete, especially the Babbage testnet setup.
