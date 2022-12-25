# Podman Scripts for Creating Rollbacks on a Cardano Test Network

The podman scripts in this folder enable the creation of rollbacks in a Cardano testnet. The network consists of nine stakepool nodes, six of them in cluster "a" and three of them in cluster "b". When the network is started, all nine nodes are in communication and maintain consensus. A script is provided to disconnect the topology of the nodes into the two clusters, which then evolve their separate consensi. Another script reconnects the topology, forcing global consensus unless so many out-of-consensus blocks have been produced (controlled by the security parameter) that the fork is permanent. Rootless containers are used, so no `root` privilege is needed to run the services.

One can test the effects of rollback on Cardano applications by breaking the consensus, submitting conflicting transactions, and then restoring consensus.

A [video demonstrating induced rollbacks on a Cardano network](https://youtu.be/VNa2kORVctM) uses these scripts.

Also see the [`cardano-node` workbench](https://github.com/input-output-hk/cardano-node/blob/master/CONTRIBUTING.rst#workbench-a-local-cluster-playground).


## Prerequisites

The scripts require the following tools:

- `podman`
- `podman-compose`
- `jq`
- `sed`
- `bash`
- `cardano-cli`

The scripts have been tested under NixOS 22.11.


## Configuration

The following genesis files and node configuration are used:

- [config/byron-genesis.json](config/byron-genesis.json)
- [config/shelley-genesis.json](config/shelley-genesis.json)
- [config/alonzo-genesis.json](config/alonzo-genesis.json)
- [config/configuration.yaml](config/configuration.yaml)

These may be lighly customized before launching the clusters. In particular, it may be useful to edit the `epochLength`, `activeSlotsCoeff`, `protocolParams`, `securityParam`, or `slotLength` of [config/shelley-genesis.json](config/shelley-genesis.json) or the `costModels` of [config/alonzo-genesis.json](config/alonzo-genesis.json). These genesis and configuration files are modified and copied to the folder `config/` in the podman volume `rollback_ipc`: use those modified files if you are connecting an external node to the test network.

The scripts create a pod `rollback` with two services `cluster-a` and `cluster-b`. They use a network `rollback` and they share the volumes `rollback_ipc` and `rollback_db`. The following files in those volumes may be of use:

- `rollback_ipc` volume
    - `node-spo-?.socket` are the sockets for the nine nodes.
    - `config/{{byron,shelley,alonzo}-genesis.json,configuration.yaml}` are the configuration files for the nodes.
    - `config/topology.json` is the topology of the fully-connected nodes.
    - `config/topology{a,b}.json` are the topologies of the "a" and "b" clusters.
- `rollback_db` volume
    - `node-spo-?.log` are the log files for the nine nodes.

By default, a network `rollback` with address range `10.100.0.0/29` is used and clusters "a" and "b" are assigned IP addresses `10.100.0.2` and `10.100.0.3`, respectively. These parameters can be changed by editing the following files:

- [CIDR](CIDR)
- [cluster-a.ip](cluster-a.ip)
- [cluster-b.ip](cluster-b.ip)


## Setup

First create the network `rollback`:

```bash
./create-network.sh
```

Next create the volumes `rollback_ipc` and `rollback_db`:

```bash
./create-volume.sh
```

Finally, load the configuration into the volumes:

```bash
./setup-volumes.sh
```

If you later decide to alter the configuration, you need to clean up the volumes and set them up again:

```bash
./cleanup-volumes.sh
./setup-volumes.sh
```


## Running the test network


### Startup

Start the network by launching the pod containing the services using `podman-compose up` or `podman-compose up --detach`. If the services launched, you should see output like the following

```console
$ podman-compose up
using podman version: 4.3.1
** excluding:  set()
podman volume inspect rollback_db || podman volume create rollback_db
['podman', 'volume', 'inspect', 'rollback_db']
podman volume inspect rollback_ipc || podman volume create rollback_ipc
['podman', 'volume', 'inspect', 'rollback_ipc']
['podman', 'network', 'exists', 'rollback']
podman create --name=cluster-a --label io.podman.compose.config-hash=123 --label io.podman.compose.project=rollback --label io.podman.compose.version=0.0.1 --label com.docker.compose.project=rollback --label com.docker.compose.project.working_dir=/extra/iohk/marlowe-cardano.SCP-4756/marlowe-scaling/rollback --label com.docker.compose.project.config_files=docker-compose.yml --label com.docker.compose.container-number=1 --label com.docker.compose.service=cluster-a -v rollback_db:/data -v rollback_ipc:/ipc --net rollback --network-alias cluster-a --log-driver=json-file --log-opt=max-size=200k --log-opt=max-file=10 --entrypoint ["/data/run-cluster-a.sh"] inputoutput/cardano-node:1.35.4
619d75e0ce7a686df827fbdf3e9335c93974c72f453d8eef9538fc792e2725d8
exit code: 0
podman volume inspect rollback_db || podman volume create rollback_db
['podman', 'volume', 'inspect', 'rollback_db']
podman volume inspect rollback_ipc || podman volume create rollback_ipc
['podman', 'volume', 'inspect', 'rollback_ipc']
['podman', 'network', 'exists', 'rollback']
podman create --name=cluster-b --label io.podman.compose.config-hash=123 --label io.podman.compose.project=rollback --label io.podman.compose.version=0.0.1 --label com.docker.compose.project=rollback --label com.docker.compose.project.working_dir=/extra/iohk/marlowe-cardano.SCP-4756/marlowe-scaling/rollback --label com.docker.compose.project.config_files=docker-compose.yml --label com.docker.compose.container-number=1 --label com.docker.compose.service=cluster-b -v rollback_db:/data -v rollback_ipc:/ipc --net rollback --network-alias cluster-b --log-driver=json-file --log-opt=max-size=200k --log-opt=max-file=10 --entrypoint ["/data/run-cluster-b.sh"] inputoutput/cardano-node:1.35.4
5e2affdaa41850aee41de528dc7cd23dfe84f4ed28f1f1511c520b07946461c5
exit code: 0
podman start -a cluster-a
podman start -a cluster-b
```

### Monitoring

Run the scripts `./watch-cluster-a.sh` and `./watch-cluster-b.sh` to view the tip of the clusters. It is often convenient to run these in separate terminal windows. They should display output like the following:

```console
Every 2.0s: cardano-cli query tip --testnet-magic 1564 | jq '{cluster:"a",tip:.}' | json2yaml    oryx: Sun Dec 11 10:58:32 2022

cluster: a
tip:
  block: 202
  epoch: 0
  era: Babbage
  hash: cbece816e835ef2a8b43d9bb62db86ba1706067a7b7398406af6c3362e47acda
  slot: 1422
  syncProgress: '100.00'
```

If the clusters are in global consensus, then the same block hash will be displayed for both clusters.


### Funding

The folder [config/utxo-keys/](config/utxo-keys/) contains the keys to the network's funded addresses. One can use `cardano-cli transaction build` to create transactions that fund other addresses using these keys. For convenience, a script is provided to fund an arbitrary address using 10% of the funds held by the first key: e.g.,

```console
$ ./fund-address.sh addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j
Estimated transaction fee: Lovelace 165721
Transaction successfully submitted.
```


### Breaking consensus

While running `./watch-cluster-a.sh` and `./watch-cluster-b.sh` in separate terminal windows, fragment the node topology into two clusters using the following command:

```bash
./disconnect-clusters.sh
```

After a few moments, you should see that the clusters are evolving separately and their block hashes and counts no longer match.


### Executing transactions

Separate node sockets are provided for clusters "a" and "b":

-  `cluster-a.socket`
-  `cluster-b.socket`

The testnet magic is `1564`.

When the nodes are in global consensus, it does not matter which socket one connects external tools to, but when the consensus is fragmented, these two sockets provide access to the separate clusters. (One can also access the sockets for the nine nodes in the volume `rollback_ipc`.)

One can test the effects of rollback on Cardano applications by breaking the consensus, submitting conflicting transactions, and then restoring consensus. Use the sockets for the two clusters when orchestrating test scenarios.


### Restoring consensus

So long as neither cluster has produced more than `securityParam` number of blocks while the topology was fragmented, running the following command will bring the clusters back into global consensus:

```bash
./reconnect-clusters.sh
```

Once again, this may take a few moments. Transactions made on one cluster, but not on the other, *may* be rolled back. Note that a transaction made on a rolled-back fork may be placed back in the memory pool and re-executed on the main branch of the blockchain. However, if different transactions on the two clusters consume the *same UTxO*, then one will be rolled back permanently and the other will appear on the main branch of the blockchain.

Because cluster "a" contains six nodes and cluster 'b" contains three nodes, there is a higher probability that transactions on cluster "a" will survive if there are conflicting transactions on the two clusters, but this is not guaranteed.


## Shutdown

You can shut down the nodes with the `podman-compose down` command. The nodes can be restarted with `podman-compose up`.


## Cleanup

First destroy the volumes `rollback_ipc` and `rollback_db`:

```bash
./destroy-volumes.sh
```

Next destroy the network `rollback`:

```bash
./destroy-network.sh
```
