# Marlowe Security Guide

- [Contracts](#contracts)
- [Transactions](#transactions)
- [Role-Tokens](#role-tokens)
- [Marlowe Runtime](#marlowe-runtime)
- [Marlowe CLI](#marlowe-cli)


## Contracts

See the [Best Practices for Marlowe on Cardano](./best-practices.md) for guidelines on verifying that a Marlowe contract will execute correctly on the Cardano blockchain. Use Marlowe Playground or `marlowe-cli run prepare` to simulate the execution of the contract.  Use the `marlowe-cli run analyze` tool to check for Cardano-specific problems in the contract, or run the contract itself on one of the Cardano testnets. In general, one should not participate in a smart contract whose execution paths one does not understand.


## Transactions

The spending of a UTxO for a Marlowe contract or a payout is validated by a Plutus script, either the semantics validator or the payout validator. For maximum security, one should not sign a transaction unless one has reviewed and understood the contents and implications of the unsigned transaction. For Marlowe, this means verifying the Marlowe contract and its input.


### Marlowe Semantics Validator

Before signing a Marlowe transaction, one should understand the following:
1. Does the transaction operate a Marlowe contract?
2. What is the current contract and its state?
3. What input is being applied to the contract?
4. What else is occurring in the transaction?


#### Does the transaction operate a Marlowe contract?

Because Marlowe's Plutus validator script is a universal interpreter for all Marlowe contracts of a specified version, the UTxO for a Marlowe contract resides at a well-known *script hash*. If one verifies that a transaction spends from an address that has the Marlowe script hash as its *payment part*, then one knows that the true Marlowe validator will run to validate the spending of that UTxO.

One can compute the script hash of the Marlowe validator from first principles by compiling the validator and computing its hash, assuming that one trusts [the Marlowe script source code](src/Language/Marlowe/Scripts.hs) in this repository. Here is a recipe:
```bash
nix run 'github:input-output-hk/marlowe-cardano#marlowe-cli' -- contract validator --mainnet --out-file /dev/null --print-hash
```
```console
addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf
Validator hash: "6a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0"
```
Here `addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf` is the address of the unstaked Marlowe validator and  `6a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0` is the well-known script hash of the Marlowe V1 validator.

A staking credential may be associated with the Marlowe validator so that any Ada residing in Marlowe contracts there accrues staking rewards. Thus, the same Marlowe validator might have many different addresses.  For example, here are several addresses associated with the same validator hash:
```console
$ nix run 'github:input-output-hk/marlowe-cardano#marlowe-cli' -- contract address --mainnet
addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf

$ nix run 'github:input-output-hk/marlowe-cardano#marlowe-cli' -- contract address --mainnet --stake-address stake1u8plvp2a7dythh6yxutd0qlzzkncr3gy5ftvxj02d3etafq4kz6s2
addr1z94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvxr7cz4mu6gh005gdck67p7y9d8s8zsfgjkcdy75mrjh6jqe7ks37

$ nix run 'github:input-output-hk/marlowe-cardano#marlowe-cli' -- contract address --mainnet --stake-address stake1u8f6d2307veldu24l0u8gck9yp2f0mvxjqtja9ras2wzacss7utvx
addr1z94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvxn564zluen7mc4t7lcw33v2gz5jlkcdyqh9628mq5u9m3qfuhp8s
```
Representing an address as base 16 bytes reveals the payment script hash, starting at the second byte:
```console
$ cardano-cli address info --address addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf | jq -r .base16
716a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0

$ cardano-cli address info --address addr1z94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvxr7cz4mu6gh005gdck67p7y9d8s8zsfgjkcdy75mrjh6jqe7ks37 | jq -r .base16
116a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0c3f6055df348bbdf443716d783e215a781c504a256c349ea6c72bea4

$ cardano-cli address info --address addr1z94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvxn564zluen7mc4t7lcw33v2gz5jlkcdyqh9628mq5u9m3qfuhp8s | jq -r .base16
116a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0d3a6aa2ff333f6f155fbf87462c5205497ed8690172e947d829c2ee2
```

#### What is the current contract and its state?

The pre-transaction state of the contract is defined in the Plutus `Datum` associated with the UTxO being spent from the Marlowe script address, and this datum must be provided in the transaction.  This datum contains the following:
- the balances of the accounts internal to the contract,
- the history of choices previously made up to this point in the contract's execution,
- the current values of the contract's bound variables, and
- the part of the contract that remains to be executed.

The `Datum` can be extracted from the unsigned transaction body and deserialized to [`Language.Marlowe.Core.V1.Semantics.MarloweData`](src/Language/Marlowe/Core/V1/Semantics.hs) using the function `Plutus.V2.Ledger.Api.fromData`. Alternatively, the command-line tool `marlowe log --show contract` will display the on-chain history of the contract.


#### What input is being applied to the contract?

The input being applied to the contract in the transaction is defined in the Plutus `Redeemer` associated with spending the UTxO from the Marlowe script address, along with the slot validity interval for the transaction specified in the transaction body. The input is a sequence of zero or more deposits, choices, and notifications. The consequences of applying this input to the contract can be studied using a tool like Marlowe Playground or `marlowe-cli run prepare`.

The `Redeemer` can be extracted from the unsigned transaction body and deserialized to [`Language.Marlowe.Scripts.MarloweInput`](src/Language/Marlowe/Scripts.hs) using the function `Plutus.V2.Ledger.Api.fromData`. The command-line tool `marlowe-cli util slotting` will compute the relationship between the slots mentioned in the validity interval to the POSIX times in the contract.


#### What else is occurring in the transaction?

The unsigned transaction may contain other spending and payments beyond that specified for the Marlowe contract. This can be examined with the tool `cardano-cli transaction view`.


### Marlowe Payout Validator

In addition to the *semantics validator* discussed above, Marlowe has a *payout validator*. The same security considerations hold for it. Its script hash can be computed as follows:
```bash
nix run 'github:input-output-hk/marlowe-cardano#marlowe-cli' -- role validator --mainnet --out-file /dev/null --print-hash
addr1w9yswm4tyqjrmj2xy5glhx9fe7m3n7rwj6fz3qfekly3mucd3rynq
```
```console
addr1w9yswm4tyqjrmj2xy5glhx9fe7m3n7rwj6fz3qfekly3mucd3rynq
Validator hash: "49076eab20243dc9462511fb98a9cfb719f86e9692288139b7c91df3"
```

Deserialization of the `Datum` and `Redeemer` should target `(Plutus.V2.Ledger.Api.CurrencySymbol, Plutus.V2.Ledger.Api.TokenName)` and `()`, respectively.


## Role Tokens

The Marlowe validator does not enforce [a particular monetary policy for role tokens](./best-practices.md#monetary-policy-for-role-tokens), in order to make possible novel use cases using role tokens. However, the security of authorizations in a role-based Marlowe contract depends critically upon the role-token monetary policy. Thus, one should carefully scrutinize both the monetary policy and the on-chain disbursement of the tokens before participating in a Marlowe contract. Verifying the monetary policy of a simple script just involves retrieving the script off of the blockchain and studying it; verifying the monetary policy of a Plutus script involves obtaining and studying the Plutus source code for the script and hashing the source code to check the monetary policy ID.

One typically uses a monetary policy (like that supported in Marlowe Runtime) that enforces a single minting event and single tokens for each role: such a monetary policy ensures that the role tokens are true non-fungible tokens (NFTs) so that the holders of the role tokens are provably the only ones who can act as the parties in the contract.

Nevertheless, monetary policies that mint multiple copies of a particular role token or ones with an open minting policy support non-standard use cases. For instance, minting two copies of each role token and distributing them to the same party allows that party to place one token in cold storage as a backup if their wallet containing the "hot" role token becomes inaccessible. Some novel crowdsourcing contracts might involve assigning the role (via identical role tokens that may be minted even after the contract commences) to many participants. Finally, a Plutus contract's minting policy for role tokens can coordinate with the operation of one or more Marlowe contracts.


## Marlowe Runtime

[Marlowe Runtime](../marlowe-runtime/) provides off-chain services that discover Marlowe contract history and build transactions that apply input to Marlowe contracts. The Marlowe Runtime deployment does not have to be trusted if one carefully examines the Marlowe transactions it produces, as discussed in the previous section. If Marlowe Runtime is deployed as part of a web service, then one needs to be aware of the possibility of person-in-the-middle, cross-site scripting, and other attacks.

Marlowe Runtime contains a [registry of known Marlowe script versions](../marlowe-runtime/src/Language/Marlowe/Runtime/Core/ScriptRegistry.hs) that it uses to create new Marlowe transactions. One should only use Marlowe Runtime to create contracts if one trusts the script hashes in that registry. The Marlowe test suite verifies that [the script registry has not been inadvertently or maliciously altered](https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-runtime/test/Language/Marlowe/Runtime/Core/ScriptRegistrySpec.hs). (Of course, this does not guarantee that the test itself has not been altered.


## Marlowe CLI

[Marlowe CLI](../marlowe-cli/) provides utilities for running and managing Marlowe contracts, including simulating and analyzing them. Transactions constructed with `marlowe-cli` can be examined using the same techniques as for studying any other Marlowe transaction.
