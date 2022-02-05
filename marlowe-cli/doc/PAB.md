# Example of Using Marlowe CLI to Run Contracts via the Marlowe PAB


The following example illustrates the execution of a simple Marlowe contract using `marlowe-cli pab`, which makes calls a Marlowe PAB server. This mimics the communication between Marlowe Run and the Marlowe PAB. The `marlowe-cli pab` command can be used to run any Marlowe contract in this manner. Contracts in JSON format can be downloaded from the Marlowe Playground.


## Prerequisites

*   Install [`marlowe-cli`](../ReadMe.md).
*   Set up the backend services for Marlowe (instructions [here](../../marlowe-dashboard-client/docs/testnet-deployment.md)).


## Start the `WalletCompanion` Contract

In a new terminal window, run the companion contract:

```
marlowe-cli pab companion --pab-url http://localhost:9080                   \
                          --wallet ffad420107082691531cad6e434ab73dce064edf \
                          --out-file companion.instance                     \
                          --loop
```

A list of active contracts will appear on the console:

```console
New observable state: CompanionState (fromList [])
New active endpoints: []
Partial transactions that need balancing: []
```

Furthermore, the log for `marlowe-pab` will report that the companion instance has been activated:

```console
[pab:Info:44] [2022-02-04 23:52:16.32 UTC] Initialising contract WalletCompanion with ID edcf9cfc-93ca-49a0-8e8b-123c8bc02b48
[pab:Info:44] [2022-02-04 23:52:16.39 UTC] Activated instance edcf9cfc-93ca-49a0-8e8b-123c8bc02b48 on Wffad420
```

The file `companion.instance` contains the instance ID for this companion contract.

```
cat companion.instance
```

```console
{
    "unContractInstanceId": "edcf9cfc-93ca-49a0-8e8b-123c8bc02b48"
}
```


## Start the `MarloweFollower` Contract

In a new terminal window, run the follower contract:

```
marlowe-cli pab follower --pab-url http://localhost:9080                   \
                         --wallet ffad420107082691531cad6e434ab73dce064edf \
                         --out-file follower.instance                      \
                         --loop
```

The console will indicate that now contracts are being followed:

```console
New observable state: ContractHistory {chParams = First {getFirst = Nothing}, chHistory = []}
New active endpoints: [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "follow"}, aeMetadata = Nothing}]
Partial transactions that need balancing: []
```

Furthermore, the log for `marlowe-pab` will report that the follower instance has been activated:

```console
[pab:Info:33] [2022-02-04 23:51:33.34 UTC] Initialising contract MarloweFollower with ID ec4cafe8-9a72-445e-a431-0800162da64d
[pab:Info:33] [2022-02-04 23:51:33.42 UTC] Activated instance ec4cafe8-9a72-445e-a431-0800162da64d on Wffad420
```

The file `follower.instance` contains the instance ID for this follower contract.

```
cat follower.instance
```

```console
{
    "unContractInstanceId": "ec4cafe8-9a72-445e-a431-0800162da64d"
}
```


## Start the `MarloweApp` Contract

In a new terminal window, run the Marlowe application contract:

```
marlowe-cli pab app --pab-url http://localhost:9080                   \
                    --wallet ffad420107082691531cad6e434ab73dce064edf \
                    --out-params-file app.params                      \
                    --out-instance-file app.instance                  \
                    --loop
```

A list of active contracts will appear on the console:

```console
New observable state: Nothing
New active endpoints: [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}]
Partial transactions that need balancing: []
```

Furthermore, the log for `marlowe-pab` will report that the application instance has been activated:

```console
[pab:Info:39] [2022-02-04 23:51:38.82 UTC] Initialising contract MarloweApp with ID bfb1dfc5-b2fe-4334-94e5-f97f8347620f
[pab:Info:39] [2022-02-04 23:51:38.90 UTC] Activated instance bfb1dfc5-b2fe-4334-94e5-f97f8347620f on Wffad420
```

The file `app.instance` contains the instance ID for this follower contract.

```
cat app.instance
```

```console
{
    "unContractInstanceId": "bfb1dfc5-b2fe-4334-94e5-f97f8347620f"
}

```


### Call the "Create" Endpoint

For illustration, we run a simple contract involving one role:

```
cat contract.json
```

```console
{
    "timeout": 99888777,
    "when": [
        {
            "then": {
                "timeout": 99888778,
                "when": [
                    {
                        "then": "close",
                        "case": {
                            "notify_if": true
                        }
                    }
                ],
                "timeout_continuation": "close"
            },
            "case": {
                "deposits": 15000000,
                "party": {
                    "role_token": "PAB"
                },
                "of_token": {
                    "currency_symbol": "",
                    "token_name": ""
                },
                "into_account": {
                    "role_token": "PAB"
                }
            }
        }
    ],
    "timeout_continuation": "close"
}
```

In a new terminal window, now create this contract:

```
marlowe-cli pab create --pab-url http://localhost:9080                                      \
                       --instance-file app.instance                                         \
                       --contract-file contract.json                                        \
                       --owner PAB=2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a
```

The terminal for the application contract will show the success of the call to "create".

```console
New observable state: Just (EndpointSuccess f06a2436-94f7-40fc-a62e-152d884a33b2 (CreateResponse (MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})})))
```

Similarly, the terminal for the companion contract will report that this contract has been created:

```console
New observable state: CompanionState (fromList [(MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})},MarloweData {marloweState = State {accounts = Map {unMap = [((PK "2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a",Token "" ""),2000000)]}, choices = Map {unMap = []}, boundValues = Map {unMap = []}, minSlot = Slot {getSlot = 5803475}}, marloweContract = When [Case (Deposit "PAB" "PAB" (Token "" "") (Constant 15000000)) (When [Case (Notify TrueObs) Close] (Slot {getSlot = 99888778}) Close)] (Slot {getSlot = 99888777}) Close})])
```

Furthermore, the log for `marlowe-pab` will report that the contract was created and that the companion detected it:

```console
[pab:Info:40] [2022-02-04 23:52:37.33 UTC] bfb1dfc5-b2fe-4334-94e5-f97f8347620f: "MarloweApp contract creation confirmed for parameters MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})}."

[pab:Info:45] [2022-02-04 23:52:37.64 UTC] edcf9cfc-93ca-49a0-8e8b-123c8bc02b48: "WalletCompanion found currency symbol 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed with on-chain state (MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})},MarloweData {marloweState = State {accounts = Map {unMap = [((PK \"2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a\",Token \"\" \"\"),2000000)]}, choices = Map {unMap = []}, boundValues = Map {unMap = []}, minSlot = Slot {getSlot = 5803475}}, marloweContract = When [Case (Deposit \"PAB\" \"PAB\" (Token \"\" \"\") (Constant 15000000)) (When [Case (Notify TrueObs) Close] (Slot {getSlot = 99888778}) Close)] (Slot {getSlot = 99888777}) Close})."
```

Also, the application contract now will have written the `app.params` file that contains the Marlowe parameters for this particular contract:

```
cat app.params
```

```console
{
    "rolesCurrency": {
        "unCurrencySymbol": "067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed"
    },
    "rolePayoutValidatorHash": "3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab",
    "slotConfig": [
        1000,
        1638215277100
    ]
}
```


## Call the "Follow" Endpoint

In order to follow this contact, we call the "follow" endpoint of the follower contract:

```
marlowe-cli pab follow --pab-url http://localhost:9080   \
                       --instance-file follower.instance \
                       --params-file app.params
```

The terminal for the follower contract will show discovery of the call to "create":

```console
New observable state: CompanionState (fromList [(MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})},MarloweData {marloweState = State {accounts = Map {unMap = [((PK "2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a",Token "" ""),2000000)]}, choices = Map {unMap = []}, boundValues = Map {unMap = []}, minSlot = Slot {getSlot = 5803475}}, marloweContract = When [Case (Deposit "PAB" "PAB" (Token "" "") (Constant 15000000)) (When [Case (Notify TrueObs) Close] (Slot {getSlot = 99888778}) Close)] (Slot {getSlot = 99888777}) Close})])
```

Furthermore, the log for `marlowe-pab` will report that the contract creation was detected by the follower:

```console
[pab:Info:35] [2022-02-04 23:53:02.84 UTC] ec4cafe8-9a72-445e-a431-0800162da64d: "MarloweFollower found contract created with MarloweData {marloweState = State {accounts = Map {unMap = [((PK \"2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a\",Token \"\" \"\"),2000000)]}, choices = Map {unMap = []}, boundValues = Map {unMap = []}, minSlot = Slot {getSlot = 5803475}}, marloweContract = When [Case (Deposit \"PAB\" \"PAB\" (Token \"\" \"\") (Constant 15000000)) (When [Case (Notify TrueObs) Close] (Slot {getSlot = 99888778}) Close)] (Slot {getSlot = 99888777}) Close} by TxOutRef {txOutRefId = 86e74967c5d7c5a87e776f57524f19c3ffe63b2c910a72b6e8bdf90355b0c1f0, txOutRefIdx = 0}."
```


## Call the "Apply-Inputs" Endpoint to Make a Deposit

Now advance the contract by making a deposit:

```
marlowe-cli pab apply-inputs --pab-url http://localhost:9080 \
                             --instance-file app.instance    \
                             --params-file app.params        \
                             --deposit-account Role=PAB      \
                             --deposit-party Role=PAB        \
                             --deposit-amount 15000000       \
                             --invalid-before 4000000        \
                             --invalid-hereafter 6000000
```

The terminal for the application contract will show that inputs have been applied:

```console
New observable state: Just (EndpointSuccess b8074330-470e-42dc-8dcf-9cf824068315 ApplyInputsResponse)
```

Similarly, the terminal for the follower contract will show the updated history of the contract:

```console
New observable state: ContractHistory {chParams = First {getFirst = Just (MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})},MarloweData {marloweState = State {accounts = Map {unMap = [((PK "2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a",Token "" ""),2000000)]}, choices = Map {unMap = []}, boundValues = Map {unMap = []}, minSlot = Slot {getSlot = 5803475}}, marloweContract = When [Case (Deposit "PAB" "PAB" (Token "" "") (Constant 15000000)) (When [Case (Notify TrueObs) Close] (Slot {getSlot = 99888778}) Close)] (Slot {getSlot = 99888777}) Close})}, chHistory = [TransactionInput {txInterval = (Slot {getSlot = 4000000},Slot {getSlot = 6000000}), txInputs = [NormalInput (IDeposit "PAB" "PAB" (Token "" "") 15000000)]}]}
```

Furthermore, the log for `marlowe-pab` will report that the inputs were applied and that the follower detected it:

```console
[pab:Info:40] [2022-02-04 23:53:57.76 UTC] bfb1dfc5-b2fe-4334-94e5-f97f8347620f: "MarloweApp contract input-application confirmed for inputs [ClientInput (IDeposit \"PAB\" \"PAB\" (Token \"\" \"\") 15000000)]."

[pab:Info:35] [2022-02-04 23:54:01.50 UTC] ec4cafe8-9a72-445e-a431-0800162da64d: "MarloweFollower found contract transitioned with TransactionInput {txInterval = (Slot {getSlot = 4000000},Slot {getSlot = 6000000}), txInputs = [NormalInput (IDeposit \"PAB\" \"PAB\" (Token \"\" \"\") 15000000)]} by TxOutRef {txOutRefId = 89090574ca6fa48765cc267ddf111bcf82fd069151518fa533a3b7617736bd0c, txOutRefIdx = 1}."
```


## Call the "Apply-Inputs" Endpoint to Make a Notification

Now advance the contract to a close by making a notification:

```
marlowe-cli pab apply-inputs --pab-url http://localhost:9080 \
                             --instance-file app.instance    \
                             --params-file app.params        \
                             --notify                        \
                             --invalid-before 4000000        \
                             --invalid-hereafter 6000000
```

The terminal for the application contract will show that inputs have been applied:

```console
New observable state: Just (EndpointSuccess 09c642ce-ce61-4b11-9e9e-e7ac97cd94f8 ApplyInputsResponse)
```

Similarly, the terminal for the follower contract will show the updated history of the contract:

```console
New observable state: ContractHistory {chParams = First {getFirst = Just (MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})},MarloweData {marloweState = State {accounts = Map {unMap = [((PK "2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a",Token "" ""),2000000)]}, choices = Map {unMap = []}, boundValues = Map {unMap = []}, minSlot = Slot {getSlot = 5803475}}, marloweContract = When [Case (Deposit "PAB" "PAB" (Token "" "") (Constant 15000000)) (When [Case (Notify TrueObs) Close] (Slot {getSlot = 99888778}) Close)] (Slot {getSlot = 99888777}) Close})}, chHistory = [TransactionInput {txInterval = (Slot {getSlot = 4000000},Slot {getSlot = 6000000}), txInputs = [NormalInput (IDeposit "PAB" "PAB" (Token "" "") 15000000)]},TransactionInput {txInterval = (Slot {getSlot = 4000000},Slot {getSlot = 6000000}), txInputs = [NormalInput INotify]}]}
```

Furthermore, the log for `marlowe-pab` will report that the inputs were applied and that the follower detected its closing:

```console
[pab:Info:40] [2022-02-04 23:54:36.58 UTC] bfb1dfc5-b2fe-4334-94e5-f97f8347620f: "MarloweApp contract input-application confirmed for inputs [ClientInput INotify]."

[pab:Info:35] [2022-02-04 23:54:39.96 UTC] ec4cafe8-9a72-445e-a431-0800162da64d: "MarloweFollower found contract closed with TransactionInput {txInterval = (Slot {getSlot = 4000000},Slot {getSlot = 6000000}), txInputs = [NormalInput INotify]} by TxId 18d27c792303662073a834a2cec6882eb2ce5df86889287ae3e467108e59ca40."
[pab:Info:35] [2022-02-04 23:54:39.96 UTC] ec4cafe8-9a72-445e-a431-0800162da64d: "MarloweFollower found finished contract with MarloweParams {rolePayoutValidatorHash = 3d8af487805f74629cec4fcf9eb4a0a6416a2ab006112057222944ab, rolesCurrency = 067da93829adecf9d67c9405158259a60627ca1a2d6592c3b52313ed, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277100})}."
```


## Call the "Redeem" Endpoint to Withdraw the Funds

Now call "redeem" to remove the funds from the payout validator's address:

```
marlowe-cli pab redeem --pab-url http://localhost:9080                                      \
                       --instance-file app.instance                                         \
                       --params-file app.params                                             \
                       --owner PAB=2f01a6ce044227256d6e867bdf3359ce1a60eb2934c0fab5a3ddbf1a
```

The terminal window for the application contract will show that the funds have been redeemed:

```console
New observable state: Just (EndpointSuccess cda3b517-2490-4813-bee0-b9521047514f RedeemResponse)
```

Furthermore, the log for `marlowe-pab` will report that the redemption occurred.

```console
[pab:Info:40] [2022-02-04 23:55:08.82 UTC] bfb1dfc5-b2fe-4334-94e5-f97f8347620f: "MarloweApp contract redemption confirmed for role \"PAB\"."
```


## Stop the Contracts

Now stop the three contracts:

```
for f in {app,follower,companion}.instance
do
  marlowe-cli pab stop --pab-url http://localhost:9080 \
                       --instance-file $f
```

The terminal windows for the application, follower, and companion contracts will each report `Contract Finished.`.
