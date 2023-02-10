# Stress Testing for Marlowe Runtime

This document summarizes the several techniques and tools that can be used for stress testing a [Marlowe Runtime deployment](../marlowe-runtime/).


# Fetching History of All Marlowe Contracts

The `marlowe-finder` tool discovers and fetches the history of all Marlowe contracts of registered Marlowe versions. It stresses the synchronization API within the `marlowe-sync` backend service. See the [`marlowe-finder` documentation](../marlowe-apps/Finder.md) for details.

If Marlowe Runtime is available on the default host and ports, this application can be run at the command line with no arguments:
```bash
marlowe-finder
```
It will write voluminous output the standard error. That output can be filtered as follows, so it just lists and watches the currently active contracts:
```bash
marlowe-finder |& jq 'select(.FinderProcess.fields.action == "wait") | .FinderProcess.fields | del(.action)'
```

The Cardano public  `preview`  test network currently contains well over 100,000 Marlowe transactions, which makes it an ideal network on which to run `marlowe-finder` . A successful stress test is to simply run `marlowe-finder` and observe whether any backend services report errors in the logs or crash.


## Running Multiple Marlowe Contracts Simultaneously

The transaction-building aspects of Marlowe Runtime can be tested by running many Marlowe contracts simultaneously against a single set of backend services. See the [`marlowe-scaling` documentation](../marlowe-apps/Scaling.md) for more details.

Multiple addresses funded with Ada are required for using `marlowe-scaling`. For example, here is a script that creates fifty addresses and signing keys from a single seed phrase stored in the file `tmp/william-shakespeare.wallet.seed`:
```bash
n=tmp/william-shakespeare

cat $n.wallet.seed | cardano-wallet key from-recovery-phrase Shelley > $n.root.prv
cat $n.wallet.seed | cardano-wallet key from-recovery-phrase Shelley | cardano-wallet key public --without-chain-code > $n.root.pub

cat $n.wallet.seed | cardano-wallet key from-recovery-phrase Shelley | cardano-wallet key child 1852H/1815H/0H/2/0 > $n.stake.prv
cat $n.wallet.seed | cardano-wallet key from-recovery-phrase Shelley | cardano-wallet key child 1852H/1815H/0H/2/0 | cardano-wallet key public --without-chain-code > $n.stake.pub
cardano-cli key convert-cardano-address-key --shelley-stake-key --signing-key-file $n.stake.prv --out-file $n.stake.skey
cardano-cli key verification-key --signing-key-file $n.stake.skey --verification-key-file $n.stake.vkey

for i in $(seq 1 50)
do
  cat $n.wallet.seed | cardano-wallet key from-recovery-phrase Shelley | cardano-wallet key child 1852H/1815H/0H/1/$i > $n.change-$i.prv
  cat $n.wallet.seed | cardano-wallet key from-recovery-phrase Shelley | cardano-wallet key child 1852H/1815H/0H/1/$i | cardano-wallet key public --without-chain-code > $n.change-$i.pub
  cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file $n.change-$i.prv --out-file $n.change-$i.skey
  cardano-cli key verification-key --signing-key-file $n.change-$i.skey --verification-key-file $n.change-$i.vkey
  cardano-cli address build --testnet-magic 1 --payment-verification-key $(cat $n.change-$i.pub) --stake-verification-key $(cat $n.stake.pub) > $n.change-$i.testnet.address
  cardano-cli address build --mainnet --payment-verification-key $(cat $n.change-$i.pub) --stake-verification-key $(cat $n.stake.pub) > $n.change-$i.mainnet.address
done
```

Once the addresses and keys have been created, they can be funded with a script like the following, which assumes the address in the file `payment.testnet.address` contains ample Ada and corresponds to the signing key in the file `payment.skey`:
```bash
marlowe-cli transaction simple ${MAGIC[@]} \
  $(marlowe-cli util select ${MAGIC[@]} --lovelace-only 1 $(cat ../networks/treasury/payment.testnet.address) | sed -e 's/^TxIn "\(.*\)" (TxIx \(.*\))$/ --tx-in \1#\2/') $(for i in tmp/william-shakespeare.change-.testnet.address; do echo "--tx-out $(cat $i)+500000000"; done) \
  --change-address $(cat payment.testnet.address) \
  --required-signer payment.skey \
  --out-file /dev/null \
  --submit 600
```

After the addresses have been funded, contracts can be run simultaneously against the Marlowe Runtime backend. For instance, the following command will run sequences of thirty contracts in twenty-fold parallelism. This will produce voluminous log output to standard error; that can be filtered with JSON query tools.
```bash
marlowe-scaling 30 $(for i in $(seq 1 20); do echo " $(cat tmp/william-shakespeare.change-$i.testnet.address)=tmp/william-shakespeare.change-$i.skey"; done)
```

Aside from the host and port parameters for Marlowe Runtime, `marlowe-scaling` includes command-line options from varying the level of stress on the `marlowe-tx` service:
```console
  --timeout-seconds INTEGER
                           Timeout in seconds for transaction confirmation.
                           (default: 600)
  --build-seconds INTEGER  Wait specified seconds before transaction
                           construction. No waiting occurs if a non-positive
                           number of seconds is specified. The specified wait
                           period is randomly increased up to a factor of two.
                           Increasing this value will increase the probability
                           that Marlowe Runtime's node has seen the transactions
                           that the submitting node has seen. (default: 3)
  --confirm-seconds INTEGER
                           Wait specified seconds after transaction
                           confirmation. No waiting occurs if a non-positive
                           number of seconds is specified. The specified wait
                           period is randomly increased up to a factor of two.
                           Increasing this value will increase the probability
                           that the submitting node has seen the transactions
                           that Marlowe Runtime has seen. (default: 3)
  --retry-seconds INTEGER  Wait specified seconds after after a failed
                           transaction before trying again. No retries occur if
                           a non-positive number of seconds is specified.
                           (default: 10)
  --retry-limit INTEGER    Maximum number of attempts for trying a failed
                           transaction again. Each subsequent retry waits twice
                           as long as the previous retry. No retries occur if a
                           non-positive number of retries is specified.
                           (default: 5)
  NATURAL                  The number of contracts to run sequentially for each
                           party.
  ADDRESS=KEYFILE          The addresses of the parties and the files with their
                           signing keys.
```
Setting `--build-seconds` and `--confirm-seconds` to zero creates the maximum rate of requests to `marlowe-tx`. Increasing the number of addresses running contract in parallel proportionally increases the number of requests on the backend.

We have observed that `marlowe-scaling` achieves greater than 99.99% success when running 20 contracts (addresses) in parallel against a single Marlowe Runtime backend, if the default settings for delays and retries are used. Note that the current protocol parameters for public Cardano networks make possible running approximately 10 Marlowe transactions per block (i.e., approximately 30 per minute), so attempting to run even 20 Marlowe contracts in parallel as quickly as possible exceeds the Plutus budget for a Cardano block.


## Summary Recommendations

Empirical observations indicate that a single Marlowe Runtime backend scales to handle the following well:
- Wait 6 seconds after the previous confirmation of a transaction before building another Marlowe transaction that is based on the addresses used in the previous transaction. (There is no need to wait after the transactions at irrelevant addresses.)
- Wait 10 seconds before retrying a failed Marlowe transaction.
- Retry up to 5 times, doubling the delay from each subsequent retry.
- Limit (throttle) the demand for building and executing Marlowe transactions on Marlowe Runtime to no more than 20 simultaneous requests.

Also note that requests to Marlowe Runtime are stateless, so that any request can be routed to any instance of Marlowe Runtime.
