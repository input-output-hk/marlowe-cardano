# Documentation Index for Marlowe Testing and Audit


## Overview of Marlowe Language

-   [Marlowe website](https://marlowe-finance.io/)
-   [Marlowe language github repository](https://github.com/input-output-hk/marlowe/)
-   [Tutorial](https://play.marlowe-finance.io/doc/marlowe/tutorials/index.html)
-   Publications:
    -   Dmytro Kondratiuk, Pablo Lamela, Alexander Nemish, Simon Thompson, *[Standardized crypto-loans on the Cardano blockchain](https://iohk.io/en/research/library/papers/standardized-crypto-loans-on-the-cardano-blockchain/]*, February/2021, Workshop on Trusted Smart Contracts (Financial Cryptography 2021).
    -   Pablo Lamela, David Smith, Simon Thompson, *[Efficient static analysis of Marlowe contracts](https://iohk.io/en/research/library/papers/efficient-static-analysis-of-marlowe-contracts/)*, September/2020, ISoLA 2020.
    -   Pablo Lamela, Alexander Nemish, David Smith, Simon Thompson, *[Marlowe: implementing and analysing financial contracts on blockchain](https://iohk.io/en/research/library/papers/marlowe-implementing-and-analysing-financial-contracts-on-blockchain/)*, January/2020, Workshop on Trusted Smart Contracts (Financial Cryptography 2020).
    -   Pablo Lamela, Simon Thompson, *[Marlowe: financial contracts on blockchain](https://iohk.io/en/research/library/papers/marlowe-financial-contracts-on-blockchain/)*, October/2018, ISoLA 2018.


## Marlowe Language and Semantics

-   [Executable specification in Isabelle](https://github.com/input-output-hk/marlowe/isabelle/)
-   [Marlowe Specification, Version 3](../specification/marlowe-isabelle-specification-4f9fa249fa51ec09a4f286099d5399eb4301ed49.pdf)


## Marlowe Implementation on Cardano

-   [Marlowe-Cardano specification](../specification/marlowe-cardano-specification.md)
-   [Marlowe-Cardano github repository](https://github.com/input-output-hk/marlowe-cardano/)
-   [Test report](./test-report.md)
-   [Testing framework](../marlowe-test/)


## Testing tools:

-   [Marlowe CLI](../../marlowe-cli/ReadMe.md)
-   [Scripted on-chain tests](../../marlowe-cli/run-nonpab-tests.sh)
-   [DSL-based off- and on-chain tests](../../marlowe-cli/test/non-pab)
-   [Debugging cookbook](../debugging-cookbook.md)


## General Resources for Audits

-   [CIP-52](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0052)
-   [Common Weaknesses in Plutus Applications](https://plutus--4604.org.readthedocs.build/en/4604/reference/common-weaknesses/index.html)
-   [Common Plutus Vulnerabilities](https://github.com/Plutonomicon/plutonomicon/blob/main/vulnerabilities.md)
-   [Gerwin's Style Guide for Isabelle/HOL](https://proofcraft.org/blog/isabelle-style.html)
