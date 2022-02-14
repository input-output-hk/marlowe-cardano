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


## Examples

*   Simple one-party contract
    *   [input file](test-simple.yaml)
    *   [output file](test-simple.log)
*   Zero-coupon bond
    *   [input file](test-zcb.yaml)
    *   [output file](test-zcb.log)


## Input File

Tests are specified as instances of [Language.Marlowe.CLI.Test.Types.PabTest](../src/Language/Marlowe/CLI/Test/Types.hs), which includes operations on contracts such as activing them and calling endpoints but also operations on wallets such as creating them and checking available funds. Test can be read from YAML or JSON files, such as [the one below](test-zcb.yaml):

```
cat test-zcb.yaml
```

```yaml
ptTestName: Zero-Coupon Bond

ptPabOperations:

  # Create the wallet for the lender, fund it with 80 ADA, and activate the
  # Marlowe endpoints.

- tag: CreateWallet
  poOwner: Lender

- tag: FundWallet
  poOwner: Lender
  poValue:
    lovelace: 80000000

- tag: ActivateApp
  poOwner: Lender
  poInstance: LenderApp

  # Create the wallet for the borrower and fund it with 10 ADA, and activate the
  # Marlowe endpoints.

- tag: CreateWallet
  poOwner: Borrower

- tag: FundWallet
  poOwner: Borrower
  poValue:
    lovelace: 10000000

- tag: ActivateApp
  poOwner: Borrower
  poInstance: BorrowerApp

  # Show the contents of the wallets.

- tag: PrintWallet
  poOwner: Lender

- tag: PrintWallet
  poOwner: Borrower

  # The lender creates the ZCB contract, which was downloaded from Marlowe
  # Playground. The loan amount is 50 ADA and the interest amount is 3 ADA.

- tag: CallCreate
  poInstance: LenderApp
  poOwners:
  - Lender
  - Borrower
  poContract:
    when:
    - case:
        party:
          role_token: Lender
        deposits: 50000000
        of_token:
          currency_symbol: ''
          token_name: ''
        into_account:
          role_token: Lender
      then:
        pay: 50000000
        token:
          currency_symbol: ''
          token_name: ''
        from_account:
          role_token: Lender
        to:
          party:
            role_token: Borrower
        then:
          when:
          - case:
              party:
                role_token: Borrower
              deposits:
                add: 50000000
                and: 3000000
              of_token:
                currency_symbol: ''
                token_name: ''
              into_account:
                role_token: Borrower
            then:
              pay:
                add: 50000000
                and: 3000000
              token:
                currency_symbol: ''
                token_name: ''
              from_account:
                role_token: Borrower
              to:
                party:
                  role_token: Lender
              then: close
          timeout: 100000002
          timeout_continuation: close
    timeout: 100000001
    timeout_continuation: close

- tag: AwaitCreate
  poInstance: LenderApp

  # The lender should have about 76 ADA now, since 2 ADA was sent to the script
  # address when creating the contract, 2 ADA was sent the borrower along with
  # the role token, and up to 1 ADA in fees might have been paid.

- tag: CheckFunds
  poOwner: Lender
  poValue:
    lovelace: 76000000
  poMaximumFees: 1000000
  poInstances:
  - LenderApp

  # The borrower should now have their original 10 ADA plus the 2 ADA that
  # arrived with the role token.

- tag: CheckFunds
  poOwner: Borrower
  poValue:
    lovelace: 12000000
  poMaximumFees: 0
  poInstances:
  - LenderApp

  # The borrower needs to follow this contract in order to interact with it.

- tag: Follow
  poInstance: BorrowerApp
  poOtherInstance: LenderApp

  # The lender deposits the 50 ADA loan amount.

- tag: CallApplyInputs
  poInstance: LenderApp
  poOwner: Lender
  poInputs:
  - tag: ClientInput
    contents:
      tag: IDeposit
      contents:
      - role_token: Lender
      - role_token: Lender
      - currency_symbol: ''
        token_name: ''
      - 50000000
  poSlots:
  - getSlot: 1000000
  - getSlot: 100000000

- tag: AwaitApplyInputs
  poInstance: LenderApp

  # The lender will have 50 ADA less for the loan and may have paid up to
  # another 2 ADA in fees, leaving a balance of 26 ADA minus up to 3 ADA in
  # fees.

- tag: CheckFunds
  poOwner: Lender
  poValue:
    lovelace: 26000000
  poMaximumFees: 3000000
  poInstances:
  - LenderApp

  # The borrower's funds are unchanged.

- tag: CheckFunds
  poOwner: Borrower
  poValue:
    lovelace: 12000000
  poMaximumFees: 0
  poInstances:
  - BorrowerApp

  # Now the borrower redeems the 50 ADA from the payout script.

- tag: CallRedeem
  poInstance: BorrowerApp
  poOwner: Borrower

- tag: AwaitRedeem
  poInstance: BorrowerApp

  # The lender's funds are unchanged.

- tag: CheckFunds
  poOwner: Lender
  poValue:
    lovelace: 26000000
  poMaximumFees: 3000000
  poInstances:
  - LenderApp

  # The borrower has an additional 50 ADA but may have paid up to 1 ADA in fees
  # for the redemption, leaving a balance of 62 ADA minus up to 1 ADA in fees.

- tag: CheckFunds
  poOwner: Borrower
  poValue:
    lovelace: 62000000
  poMaximumFees: 1000000
  poInstances:
  - BorrowerApp

  # The borrower pays back the 53 ADA of loan plus interest.

- tag: CallApplyInputs
  poInstance: BorrowerApp
  poOwner: Lender
  poInputs:
  - tag: ClientInput
    contents:
      tag: IDeposit
      contents:
      - role_token: Borrower
      - role_token: Borrower
      - currency_symbol: ''
        token_name: ''
      - 53000000
  poSlots:
  - getSlot: 1000000
  - getSlot: 100000000

- tag: AwaitApplyInputs
  poInstance: BorrowerApp

  # Because the contract is closed when the borrower makes their deposit, the
  # lender receives back their 2 ADA that they contributed to create the
  # contract.

- tag: CheckFunds
  poOwner: Lender
  poValue:
    lovelace: 28000000
  poMaximumFees: 3000000
  poInstances:
  - LenderApp

  # The borrower has 53 ADA less and may have paid another 2 ADA in fees,
  # leaving a balance of 9 ADA minus up to 3 ADA in fees.

- tag: CheckFunds
  poOwner: Borrower
  poValue:
    lovelace: 9000000
  poMaximumFees: 3000000
  poInstances:
  - BorrowerApp

  # The lender redeems the 53 ADA.

- tag: CallRedeem
  poInstance: LenderApp
  poOwner: Lender

- tag: AwaitRedeem
  poInstance: LenderApp

  # The lender now has an additional 53 ADA but may have paid another 1 ADA in
  # fees, leaving a balance of 81 ADA minus up to 4 ADA in fees.

- tag: CheckFunds
  poOwner: Lender
  poValue:
    lovelace: 81000000
  poMaximumFees: 4000000
  poInstances:
  - LenderApp

  # The borrower's funds are unchanged.

- tag: CheckFunds
  poOwner: Borrower
  poValue:
    lovelace: 9000000
  poMaximumFees: 3000000
  poInstances:
  - BorrowerApp

  # Stop the Marlowe application.

- tag: Stop
  poInstance: LenderApp

- tag: Stop
  poInstance: BorrowerApp
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
                           test-zcb.yaml
```

```console
***** Test "Zero-Coupon Bond" *****
[CreateWallet] Created wallet 881f366927a1cd3847dd12244a2620e9592d61c0 for role "Lender".
[FundWallet] Funded wallet 881f366927a1cd3847dd12244a2620e9592d61c0 for role "Lender" with valueFromList [(AdaAssetId,80000000)].
[ActivateApp] Activated MarloweApp instance c2b9e4c7-fd8d-43ce-a9b5-f1dc6a0fa726 for role "Lender".
[CreateWallet] Created wallet 8000731e727a26edf6d411e90d1f3a27b6dca172 for role "Borrower".
[FundWallet] Funded wallet 8000731e727a26edf6d411e90d1f3a27b6dca172 for role "Borrower" with valueFromList [(AdaAssetId,10000000)].
[ActivateApp] Activated MarloweApp instance f66a6862-9526-4b98-a9b0-c75b96659dc4 for role "Borrower".
[PrintWallet] Wallet for role "Lender" contains valueFromList [(AdaAssetId,80000000)].
[PrintWallet] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,10000000)].
[CallCreate] Endpoint "create" called on instance c2b9e4c7-fd8d-43ce-a9b5-f1dc6a0fa726 for owners [("Lender",ca485e3ef87f7515bae5cf49a6a55224043d30bd28c04bb06a841c6e),("Borrower",95f1162148b1d5edda006afcfe9b36313034a995f24a6abb17fb23a7)].
[AwaitCreate] Creation confirmed with MarloweParams {rolePayoutValidatorHash = 0955850f7d05e040572ef7c580ee99963ed891bc7e1ebd8f2d60372a, rolesCurrency = ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef, slotConfig = (1000,POSIXTime {getPOSIXTime = 1638215277000})}.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,75515174),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,12000000),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Borrower",1)].
[Follow] Instance f66a6862-9526-4b98-a9b0-c75b96659dc4 now follows instance c2b9e4c7-fd8d-43ce-a9b5-f1dc6a0fa726.
[CallApplyInputs] Endpoint "apply-inputs" called on c2b9e4c7-fd8d-43ce-a9b5-f1dc6a0fa726 for inputs [ClientInput (IDeposit "Lender" "Lender" (Token "" "") 50000000)] and slots Just (Slot {getSlot = 1000000},Slot {getSlot = 100000000}).
[AwaitApplyInputs] Input application confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,23998539),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,12000000),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Borrower",1)].
[CallRedeem] Endpoint "redeem" called on f66a6862-9526-4b98-a9b0-c75b96659dc4 for role "Borrower".
[AwaitRedeem] Redemption confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,23998539),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,61554180),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Borrower",1)].
[CallApplyInputs] Endpoint "apply-inputs" called on f66a6862-9526-4b98-a9b0-c75b96659dc4 for inputs [ClientInput (IDeposit "Borrower" "Borrower" (Token "" "") 53000000)] and slots Just (Slot {getSlot = 1000000},Slot {getSlot = 100000000}).
[AwaitApplyInputs] Input application confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,25998539),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,7201370),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Borrower",1)].
[CallRedeem] Endpoint "redeem" called on c2b9e4c7-fd8d-43ce-a9b5-f1dc6a0fa726 for role "Lender".
[AwaitRedeem] Redemption confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,78466536),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,7201370),(AssetId "ea5915a653254c5feea89dac251a7264ce5f6874299439ea1cebcaef" "Borrower",1)].
[Stop] Instance c2b9e4c7-fd8d-43ce-a9b5-f1dc6a0fa726 stopped.
[Stop] Instance f66a6862-9526-4b98-a9b0-c75b96659dc4 stopped.
***** SUCCEEDED *****
```
