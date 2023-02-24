# Deploying Marlowe Runtime Backend Services Using Docker

Deploying the Marlowe Runtime requires running seven backend services:
- five [marlowe-cardano](https://github.com/input-output-hk/marlowe-cardano/blob/main/README.adoc) services:
	- `marlowe-chain-indexer` for indexing the blockchain
	- `marlowe-chain-sync` for querying the blockchain
	- `marlowe-indexer` for indexing marlowe contracts
	- `marlowe-sync` for querying marlowe contracts
	- `marlowe-proxy` for providing a unified API over a single port
	- `marlowe-tx` for building and submitting Marlowe transactions
- [cardano-node](https://github.com/input-output-hk/cardano-node/blob/master/README.rst) for blockchain connectivity
- [PostgreSQL](https://www.postgresql.org/) for persistent storage

This document describes how to deploy using Docker. For a manual service-by-service deployment, see [here](deployment.md).
