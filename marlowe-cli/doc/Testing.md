# Testing Contracts with Marlowe CLI

The `marlowe-cli test contracts` command reads a series test specifications for Marlowe contracts and runs them on the Marlowe PAB, checking the results for errors and for correct movement of funds.


## Usage

```
marlowe-cli test contracts --help
```

```console
Usage: marlowe-cli test contracts [--testnet-magic INTEGER]
                                  --socket-path SOCKET_FILE --wallet-url URL
                                  --pab-url URL --faucet-key SIGNING_FILE
                                  --faucet-address ADDRESS
                                  --burn-address ADDRESS --passphrase PASSWORD 
                                  [TEST_FILE]
  Test Marlowe contracts using the Marlowe PAB.

Available options:
  --testnet-magic INTEGER    Network magic, or omit for mainnet.
  --socket-path SOCKET_FILE  Location of the cardano-node socket file.
  --wallet-url URL           URL for Cardano Wallet.
  --pab-url URL              URL for the Marlowe PAB.
  --faucet-key SIGNING_FILE  The file containing the signing key for the faucet.
  --faucet-address ADDRESS   The address of the faucet.
  --burn-address ADDRESS     Burn address for discarding used tokens.
  --passphrase PASSWORD      The passphrase used for the Marlowe PAB.
  TEST_FILE                  JSON file containing a test case.
  -h,--help                  Show this help text
```


## Input File

Tests are specified as instances of [Language.Marlowe.CLI.Test.Types.PabTest](../src/Language/Marlowe/CLI/Test/Types.hs), which includes operations on contracts such as activing them and calling endpoints but also operations on wallets such as creating them and checking available funds. Test can be read from YAML or JSON files, such as [the one below](example-test.yaml):

```
cat example-test.yaml
```

```yaml
ptTestName: Example

ptPabOperations:

- tag: CreateWallet
  poOwner: PAB

- tag: FundWallet
  poOwner: PAB
  poValue:
    lovelace: 100000000

- tag: ActivateApp
  poOwner: PAB
  poInstance: App

- tag: CallCreate
  poInstance: App
  poOwners:
  - PAB
  poContract:
    when:
    - case:
        party:
          role_token: PAB
        deposits: 15000000
        into_account:
          role_token: PAB
        of_token:
          currency_symbol: ''
          token_name: ''
      then:
        when:
        - case:
            notify_if: true
          then: close
        timeout: 99888778
        timeout_continuation: close
    timeout: 99888777
    timeout_continuation: close

- tag: AwaitCreate
  poInstance: App

- tag: CallApplyInputs
  poInstance: App
  poOwner: PAB
  poInputs:
  - tag: ClientInput
    contents:
      tag: IDeposit
      contents:
      - role_token: PAB
      - role_token: PAB
      - currency_symbol: ''
        token_name: ''
      - 15000000
  poSlots:
  - getSlot: 1000000
  - getSlot: 9999999

- tag: AwaitApplyInputs
  poInstance: App

- tag: CallApplyInputs
  poInstance: App
  poOwner: PAB
  poInputs:
  - tag: ClientInput
    contents:
      tag: INotify
  poSlots:
  - getSlot: 1000000
  - getSlot: 9999999

- tag: AwaitApplyInputs
  poInstance: App

- tag: CallRedeem
  poInstance: App
  poOwner: PAB

- tag: AwaitRedeem
  poInstance: App

- tag: Stop
  poInstance: App

- tag: CheckFunds
  poOwner: PAB
  poValue:
    lovelace: 100000000
  poMaximumFees: 4000000
  poInstances:
  - App
```


## Example Output

Running the above test yields the result below:

```
marlowe-cli test contracts --testnet-magic 1564                                                             \
                           --socket-path node.socket                                                        \
                           --wallet-url http://localhost:8090                                               \
                           --pab-url http://localhost:9080                                                  \
                           --faucet-key payment.skey                                                        \
                           --faucet-address addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
                           --burn-address addr_test1vqxdw4rlu6krp9fwgwcnld6y84wdahg585vrdy67n5urp9qyts0y7   \
                           --passphrase pab123456789                                                        \
                           example-test.yaml 
```

```console
***** Test "Example" *****                                        
[CreateWallet] Created wallet 5436632a46f2580761249c6abcb1bc5655b32b82 for role "PAB".                                               
[FundWallet] Funded wallet 5436632a46f2580761249c6abcb1bc5655b32b82 for role "PAB" with valueFromList [(AdaAssetId,100000000)].
[ActivateApp] Activated MarloweApp instance 44b5a82d-9db7-4ae1-be26-10b28326ad52 for role "PAB".                                     
[CallCreate] Endpoint "create" called on instance 44b5a82d-9db7-4ae1-be26-10b28326ad52 for owners [("PAB",06d181fe9be0582abb1482e1a0ce28d67ba01b1d824834df6faffd58)].
[AwaitCreate] Creation confirmed with MarloweParams {rolePayoutValidatorHash = 52118df82b7dacfb428f549583a09d499f04a44ad0ce8ffd46b8bdeb, rolesCurrency = e0d952a74b84e7e64e794718394af29a3785694700c1c0dea9afd91c, slotConfig = (1000,POSIXTime {getPOSIXTime = 16382152770
00})}.                                                            
[CallApplyInputs] Endpoint "apply-inputs" called on 44b5a82d-9db7-4ae1-be26-10b28326ad52 for inputs [ClientInput (IDeposit "PAB" "PAB" (Token "" "") 15000000)] and slots Just (Slot {getSlot = 1000000},Slot {getSlot = 9999999}).
[AwaitApplyInputs] Input application confirmed.                   
[CallApplyInputs] Endpoint "apply-inputs" called on 44b5a82d-9db7-4ae1-be26-10b28326ad52 for inputs [ClientInput INotify] and slots Just (Slot {getSlot = 1000000},Slot {getSlot = 9999999}).
[AwaitApplyInputs] Input application confirmed.                   
[CallRedeem] Endpoint "redeem" called on 44b5a82d-9db7-4ae1-be26-10b28326ad52 for role "PAB".                                        
[AwaitRedeem] Redemption confirmed.                               
[Stop] Instance 44b5a82d-9db7-4ae1-be26-10b28326ad52 stopped.                                                                        
[CheckFunds] Wallet for role "PAB" contains valueFromList [(AdaAssetId,96725080),(AssetId "e0d952a74b84e7e64e794718394af29a3785694700c1c0dea9afd91c" "PAB",1)].
***** SUCCEEDED *****                                             
```
