# Deploying Marlowe Runtime Backend Services Using Docker

Deploying the Marlowe Runtime requires running six backend services:
- four [marlowe-cardano](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc) services:
	- `marlowe-chain-sync` for indexing the blockchain
	- `marlowe-history` for tracking the on-chain history of Marlowe contract instances
	- `marlowe-discovery` for discovering the presence of Marlowe contract instances on the blockchain
	- `marlowe-tx` for building and submitting Marlowe transactions
- [cardano-node](https://github.com/input-output-hk/cardano-node/blob/master/README.rst) for blockchain connectivity
- [PostgreSQL](https://www.postgresql.org/) for persistent storage

This document describes how to deploy using Docker. For a manual service-by-service deployment, see [here](deployment.md).
