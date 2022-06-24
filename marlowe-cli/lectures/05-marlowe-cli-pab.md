---
date: 24 June 2022
version: marlowe-cli 0.0.5.0
---

# Running Marlowe Contracts with the Marlowe Backend

In this lecture we demonstrate how to use `marlowe-cli` to run Marlowe
contracts using the Marlowe Backend.

## Components for Marlowe Deployment

-   **Marlowe using its backend**
    -   Cardano node, `cardano-node`, interacts with the Cardano
        blockchain at large.
    -   Cardano wallet, `cardano-wallet`, manages users\' wallets.
    -   Plutus chain index, `plutus-chain-index`, indexes historical and
        unspent transactions and Plutus data.
    -   Plutus application backend, `marlowe-pab`, constructs Marlowe
        transactions.
-   Marlowe Run
    -   Marlowe Run server, `marlowe-dashboard-server`, provides proxies
        to the wallet, chain index, and PAB.
    -   Marlowe Run client, `marlowe-dashboard-client`, serves the user
        interface.
-   Marlowe Playground
    -   Marlowe Playground server, `marlowe-playground-server`, provides
        backend services.
    -   Marlowe Playground client, `marlowe-playground-client`, serves
        the user interface.

## Marlowe Deployment Using Its Backend

![Components in a Marlowe Backend
deployment.](diagrams/testnet-deployment.svg)

## Running the Marlowe Backend

All five backend services (`cardano-node`, `cardano-wallet`,
`plutus-chain-index`, `marlowe-pab`, and `marlowe-dashboard-server`) can
be launched from within a Nix shell using a script for Docker Compose.

``` console
[nix-shell] $ start-marlowe-run
```

If needed, the Marlowe Run frontend can be launched similarly.

``` console
[nix-shell] $ npm run start
```

See
\<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-dashboard-client/README.md>\>
for details.

## Marlowe\'s Backend Contracts

-   The Marlowe Backend uses the term \"contract\" to describe groups of
    backend endpoints.
    -   These \"contracts\" are not smart contracts or Marlowe
        contracts.
-   Marlowe implements three generic backend contracts.
    -   The `MarloweApp` contract submits transactions related to a
        Marlowe contract.
    -   The `MarloweFollower` contract reports information about Marlowe
        transactions that have occurred on the blockchain.
    -   The `WalletCompanion` contract discovers Marlowe contracts that
        have appeared on the blockchain.

### Marlowe Application Contract and Endpoints

The `MarloweApp` backend contract has endpoints that can be called by
applications.

1.  `create` submits a new Marlowe contract to the blockchain and mints
    any needed role tokens.
2.  `apply-inputs` submits a transaction that advances a Marlowe
    contract to is next state. Inputs may be any combination of choices,
    deposits, and notifications.
3.  `auto` submits a transaction that advances a Marlowe contract
    without applying any inputs, as in the case where a `When` clause
    has timed out.
4.  `redeem` submits a transaction that withdraws funds from a Marlowe
    contract\'s payout validator address.

These endpoints closely correspond to the non-backend commands that
`marlowe-cli` supports Marlowe transactions. The caller receives a
WebSocket message each after the endpoint\'s transaction is confirmed on
the blockchain:

``` haskell
data MarloweEndpointResult =
    CreateResponse MarloweParams
  | ApplyInputsResponse
  | AutoResponse
  | RedeemResponse
  | CloseResponse
```

### Marlowe Follower Contract and Endpoints

The `MarloweFollower` backend contract just has a single endpoint.

Calling the `follow` endpoint with the parameters of a Marlowe contract
results in a stream of WebSockets messages that notify the caller about
transactions involving the contract and the contract\'s progression.

``` haskell
data ContractHistory =
  ContractHistory
    { chParams         :: MarloweParams      -- ^ The "instance id" of the contract.
    , chInitialData    :: MarloweData        -- ^ The initial Contract and State.
    , chHistory        :: [TransactionInput] -- ^ All the transactions that affected the contract.
    , chAddress        :: Address            -- ^ The script address of the marlowe contract
    , chUnspentPayouts :: UnspentPayouts     -- ^ All UTxOs associated with our payout script.
    }
```

### Wallet Companion Contract

The `WalletCompanion` backend contract has not endpoints.

It produces a stream of WebSockets messages that notify the creator
about new Marlowe contracts in its wallet, represented by a map from
contract parameters to contract and state.

``` haskell
newtype CompanionState = CompanionState (Map MarloweParams MarloweData)
```

## Calling Backend Endpoints with Marlowe CLI

The `marlowe-cli pab` commands enable one to create any of the three
backend contracts or call their endpoints.

``` bash
marlowe-cli pab --help
```

    Usage: marlowe-cli pab COMMAND

      Run a contract via the PAB.

    Available options:
      -h,--help                Show this help text

    Commands for running contracts on the PAB:
      app                      Start the Marlowe application contract.
      create                   Create a Marlowe contract.
      apply-inputs             Apply inputs to a Marlowe contract.
      redeem                   Redeem funds from a Marlowe contract.
      follower                 Start the Marlowe follower contract.
      follow                   Follow a Marlowe contract.
      companion                Start the Marlowe companion contract.
      stop                     Stop a Marlowe contract.

## Running a Simple Contract using the Backend

-   Documentation at
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/doc/backend-tutorial.md>\>
    provides a full example of using all three backend contracts and
    their endpoints, along with the log messages expected from all of
    the backend services.
-   Here we just illustrate the use of the transaction-submitting
    backend contract, `MarloweApp`.

### Designing a Contract

We create a simple contract that receives a deposit and then pays it out
after a notification.

``` bash
NOW=$(( $(date -u +%s) * 1000 ))
HOUR=$(( 60 * 60 * 1000 ))
cat > contract.json << EOI
{
  "when": [
    {
      "case": {
        "party": {"role_token": "PAB"}, "deposits": 15000000,
        "of_token": {"currency_symbol": "", "token_name": ""},
        "into_account": {"role_token": "PAB"}
      },
      "then": {
        "when": [{"case": {"notify_if": true}, "then": "close"}],
      "timeout": $(( NOW + 12 * HOUR )), "timeout_continuation": "close"
      }
    }
  ],
  "timeout": $(( NOW + 10 * HOUR )), "timeout_continuation": "close"
}
EOI
```

### Restoring the Wallet

Because the PAB operates on wallets served by `cardano-wallet`, we need
to create a wallet in that service. We use the same wallet as in the
earlier lecture \"Installing Marlowe CLI and Associated Tools\".

``` bash
cat << EOI > wallet.restore
{
  "name": "Example Wallet",
  "mnemonic_sentence": [
    "broccoli", "tool", "napkin", "scale", "lab", "liquid",
    "staff", "turn", "equal", "city", "sail", "company",
    "govern", "hold", "rent", "act", "nurse", "orbit",
    "torch", "normal", "update", "master", "valley", "twenty"
  ],
  "passphrase": "fixme-allow-pass-per-wallet"
}
EOI
```

``` bash
curl -X POST -H "Content-type: application/json" -d @wallet.restore http://localhost:8090/v2/wallets
```

    {"passphrase":{"last_updated_at":"2022-05-02T20:18:24.422037003Z"},"name":"Example Wallet","address_pool_gap":20,"state":{"progress":{"quantity":0,"unit":"percent"},"status":"syncing"},"id":"45ef9f5fd955028cd6c434be8ab4085b6ef89d0e","delegation":{"active":{"status":"not_delegating"},"next":[]},"assets":{"available":[],"total":[]},"balance":{"available":{"quantity":0,"unit":"lovelace"},"total":{"quantity":0,"unit":"lovelace"},"reward":{"quantity":0,"unit":"lovelace"}},"tip":{"absolute_slot_number":0,"time":"2022-04-14T14:52:01Z","slot_number":0,"epoch_number":0,"height":{"quantity":0,"unit":"block"}}}

We record the wallet ID and retrieve its first address.

``` bash
WALLET_ID=45ef9f5fd955028cd6c434be8ab4085b6ef89d0e
WALLET_ADDRESS=$(curl -s http://localhost:8090/v2/wallets/$WALLET_ID/addresses | jq -r '.[0].id')
echo $WALLET_ADDRESS
```

    addr_test1qzhk6c3qwlgh6dtdmrhhe2857pvzw73t2q8gdyd95f6mvs9dyukrsjz88gz9889rmf63dtluc2glauja64krx25cyzmsa00l2y

Send 100 test ADA to that wallet address before running the rest of this
example.

### Starting the `MarloweApp`

The `marlowe-cli pab app` command takes a Cardano Wallet ID and a
connection to the Marlowe PAB as input and it outputs a file containing
the Marlowe parameters of the contract and the PAB instance ID.

``` bash
marlowe-cli pab app --pab-url http://localhost:9080  \
                    --wallet $WALLET_ID              \
                    --out-params-file app.params     \
                    --out-instance-file app.instance \
                    --loop                           &
```

    [1] 1173015


The `app.params` file will not be written until the `create` endpoint
has been called. We can run this command in the background via bash\'s
`&`. It will print out WebSocket messages from the PAB until the
contract instance stops.

### Calling the Create Endpoint

When calling the `create` endpoint, simply supply the contract to the
PAB along with the instance ID and the addresses for owners of role
tokens.

``` bash
marlowe-cli pab create --pab-url http://localhost:9080 \
                       --instance-file app.instance    \
                       --contract-file contract.json   \
                       --owner PAB=$WALLET_ADDRESS
```

    New active endpoints: []

Wait for the transaction to be confirmed on the blockchain.

``` bash
sleep 90
```

    New observable state: Just (EndpointSuccess 2b3460b3-b639-4d60-a38b-bae2367e511d (CreateResponse (MarloweParams {rolePayoutValidatorHash = 362d3928e20d33fda0e44fb73f348b78e1a6708cac2128d506d2a770, rolesCurrency = debff7b749b6fb9d96578fdc6f27d2fbcb116869b9e845279836fa23})))
    New active endpoints: [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}]

### Making a Deposit to the Contract

Simply call the `apply-inputs` endpoint with `--deposit` flags.

``` bash
marlowe-cli pab apply-inputs --pab-url http://localhost:9080 \
                             --instance-file app.instance    \
                             --params-file app.params        \
                             --deposit-account Role=PAB      \
                             --deposit-party Role=PAB        \
                             --deposit-amount 15000000       \
                             --invalid-before "$NOW"         \
                             --invalid-hereafter "$((NOW+HOUR))"
```

    New active endpoints: []

Wait for the transaction to be confirmed on the blockchain.

``` bash
sleep 90
```

    New observable state: Just (EndpointSuccess c144ef5b-4b6c-48ed-a886-11cf762a933b ApplyInputsResponse)
    New active endpoints: [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}]

### Notifying a Contract

Simply call the `apply-inputs` endpont with the `--notify` flag.

``` bash
marlowe-cli pab apply-inputs --pab-url http://localhost:9080 \
                             --instance-file app.instance    \
                             --params-file app.params        \
                             --notify                        \
                             --invalid-before "$NOW"         \
                             --invalid-hereafter "$((NOW+HOUR))"
```

    New active endpoints: []

Wait for the transaction to be confirmed on the blockchain.

``` bash
sleep 90
```

    New observable state: Just (EndpointSuccess cfd7791f-f9cd-4a08-9e58-bc63ca16881c ApplyInputsResponse)
    New active endpoints: [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}]

### Withdrawing Funds from the Payout Validator

Simply call the `redeem` endpoint with the name of the owner and their
address.

``` bash
marlowe-cli pab redeem --pab-url http://localhost:9080 \
                       --instance-file app.instance    \
                       --params-file app.params        \
                       --owner PAB=$WALLET_ADDRESS
```

    New active endpoints: []

Wait for the transaction to be confirmed on the blockchain.

``` bash
sleep 90
```

    New observable state: Just (EndpointSuccess 96619ae4-0e58-46fb-a327-d2f913a39e0d RedeemResponse)
    New active endpoints: [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}]

### Stop the `MarloweApp`

Simpley tell the PAB to stop the contract.

``` bash
marlowe-cli pab stop --pab-url http://localhost:9080 \
                     --instance-file app.instance
```

    Contract finished.


## Querying the On-Chain State and History of a Marlowe Contract

Marlowe CLI can query the Plutus chain index to find the current statet
of a contract.

Record the roles currency from the previous example and query to see if
there is any live state for the Marlowe contract with that roles
currency.

``` bash
ROLES_CURRENCY=debff7b749b6fb9d96578fdc6f27d2fbcb116869b9e845279836fa23
marlowe-cli query app --index-url http://localhost:9083 --roles-currency $ROLES_CURRENCY
```

    []

The query is empty because the contract already completed. Query again
with the `--spent` flag to see its prior states.

``` bash
marlowe-cli query app --index-url http://localhost:9083 --roles-currency $ROLES_CURRENCY --spent | json2yaml | head -n 10
```

    - marloweDatum:
        marloweContract:
          timeout: 1651565881000
          timeout_continuation: close
          when:
          - case:
              notify_if: true
            then: close
        marloweState:
          accounts:

One can also query the detailed history of a contract to see which
inputs were applied, etc.

``` bash
marlowe-cli query history --index-url http://localhost:9083 --roles-currency $ROLES_CURRENCY | json2yaml | head -n 10
```

    - historyData:
        marloweContract:
          timeout: 1651558681000
          timeout_continuation: close
          when:
          - case:
              deposits: 15000000
              into_account:
                role_token: PAB
              of_token:

## Testing Marlowe Contracts with its Backend

Marlowe CLI can also be used to run test scripts for the PAB. Those test
scripts contain a series of endpoint calls and other operations, along
with assertions to check. See
\<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/test/ReadMe.md>\>
for details.

``` bash
marlowe-cli test contracts --help
```

    Usage: marlowe-cli test contracts [--testnet-magic INTEGER]
                                      --socket-path SOCKET_FILE --wallet-url URL
                                      --pab-url URL --faucet-key SIGNING_FILE
                                      --faucet-address ADDRESS
                                      --burn-address ADDRESS --passphrase PASSWORD
                                      TEST_FILE

      Test Marlowe contracts using the Marlowe PAB.

    Available options:
      --testnet-magic INTEGER  Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE
                               Location of the cardano-node socket file.
      --wallet-url URL         URL for Cardano Wallet.
      --pab-url URL            URL for the Marlowe PAB.
      --faucet-key SIGNING_FILE
                               The file containing the signing key for the faucet.
      --faucet-address ADDRESS The address of the faucet.
      --burn-address ADDRESS   Burn address for discarding used tokens.
      --passphrase PASSWORD    The passphrase used for the Marlowe PAB.
      TEST_FILE                JSON file containing a test case.
      -h,--help                Show this help text

## Troubleshooting

-   See the previous lecture, \"Running Marlowe Contacts on the
    Blockchain\", for hints on troubleshooting Marlowe transactions in
    general.
-   Correct operation of the Marlowe backend depends upon having all of
    the ports correctly configured.
-   Failures may occur when wallets do not have sufficient funds.
-   Run the Marlowe backend test suite to verify that the backend
    components are working:
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/run-tests.sh>\>.
-   Study the log files of the backend components to see whether
    requests are reaching the backend, chain-index, wallet, and node.

## Resources

-   Marlowe CLI documentation:
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/ReadMe.md>\>.
-   Marlowe Debugging Cookbook:
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe/debugging-cookbook.md>\>.
-   Test suite for Marlowe backend with `marlowe-cli`:
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/test/ReadMe.md>\>.
-   Detailed example for the Marlowe backend:
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/doc/backend-tutorial.md>\>.
-   Marlowe Playground: \<<https://playground.marlowe.iohkdev.io/#/>\>.
-   Marlowe Run
    -   On pioneers testnet:
        \<<https://marlowe-run-marlowe-pioneers.plutus.aws.iohkdev.io/>\>.
    -   On mock network: \<<https://marlowe-finance.io/>\>.
-   Plutus Pioneers Program:
    \<<https://github.com/input-output-hk/plutus-pioneer-program>\>.
-   Plutus Community:
    \<<https://plutus-community.readthedocs.io/en/latest/>\>.

## Exercises (Difficult)

1.  Repeat the Marlowe backend tutorial
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/doc/backend-tutorial.md>\>.
2.  Run the Marlowe backend tests
    \<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/run-tests.sh>\>.

## Summary

-   The Marlowe backend consists of six components working together.
-   The `marlowe-cli pab` command lets one create PAB contracts and call
    their endpoints.
-   The `marlowe-cli query` command will query the Plutus chain index to
    determine the state or history of a Marlowe contract.
-   The `marlowe-cli test contracts` command runs test scripts that
    check the correct operation of contracts on the Marlowe PAB.

## Other Lectures

Lectures on Marlowe CLI:
\<<https://github.com/input-output-hk/marlowe-cardano/blob/mpp-cli-lectures/marlowe-cli/lectures/ReadMe.md>\>

-   [Overview of Marlowe CLI](01-marlowe-cli-overview.md)
-   [Installing Marlowe CLI and Associated
    Tools](02-marlowe-cli-installation.md)
-   [Running Marlowe Contracts without Blockchain
    Transactions](03-marlowe-cli-abstract.md)
-   [Running Marlowe Contacts on the
    Blockchain](04-marlowe-cli-concrete.md)
-   ~~Running Marlowe Contracts with the Marlowe Backend~~
