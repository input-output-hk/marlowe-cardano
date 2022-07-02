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
*   Escrow
    *   [input file](test-escrow.yaml)
*   Escrow with collateral
    *   [input file](test-escrow-with-collateral.yaml)
*   Zero-coupon bond
    *   [input file](test-zcb.yaml)
*   Zero coupon bond, with an attempt at an overdue payment
    *   [input file](test-zero-coupon-bond-too-late.yaml)
*   Zero coupon bond, with timeout immediately
    *   [input file](test-zero-coupon-bond-immediate-timeout.yaml)
*   Zero coupon bond, with timeout after a delay
    *   [input file](test-zero-coupon-bond-delayed-timeout.yaml)
*   Coupon bond guaranteed
    *   [input file](test-coupon-bond-guaranteed.yaml)
*   Contract for differences
    *   [input file](test-contract-for-differences.yaml)
*   Contract for differences, with oracle
    *   [input file](test-contract-for-differences-with-oracle.yaml)
*   Swap of ADA for ADA
    *   [input file](test-swap-of-ada-for-ada.yaml)
*   Swap of ADA and dollar tokens
    *   [input file](test-swap-of-ada-and-dollar-tokens.yaml)


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
          timeout: 1961123625000
          timeout_continuation: close
    timeout: 1929587625000
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
  - input_from_party:
      role_token: Lender
    that_deposits: 50000000
    of_token:
      currency_symbol: ''
      token_name: ''
    into_account:
      role_token: Lender
  poTimes:
  - 1645590825000
  - 1898051625000

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
  - input_from_party:
      role_token: Borrower
    that_deposits: 53000000
    of_token:
      currency_symbol: ''
      token_name: ''
    into_account:
      role_token: Borrower
  poTimes:
  - 1645590825000
  - 1898051625000

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
                           --passphrase fixme-allow-pass-per-wallet                                         \
                           test-zcb.yaml
```

```console
***** Test "Zero-Coupon Bond" *****
[CreateWallet] Created wallet 98b83ddea22624e16479d177b0947f8462216681 for role "Lender".
[FundWallet] Funded wallet 98b83ddea22624e16479d177b0947f8462216681 for role "Lender" with valueFromList [(AdaAssetId,80000000)].
[ActivateApp] Activated MarloweApp instance 9b7060a3-466c-489a-8283-77832c1505f5 for role "Lender".
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewObservableState Null.
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewActiveEndpoints [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}].
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewYieldedExportTxs [].
[CreateWallet] Created wallet 222ee18eebb5e5d6bab95576abe3271f819909a6 for role "Borrower".
[FundWallet] Funded wallet 222ee18eebb5e5d6bab95576abe3271f819909a6 for role "Borrower" with valueFromList [(AdaAssetId,10000000)].
[ActivateApp] Activated MarloweApp instance d5b512a8-983e-45f9-a059-b16f4c968ca3 for role "Borrower".
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewObservableState Null.
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewActiveEndpoints [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}].
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewYieldedExportTxs [].
[PrintWallet] Wallet for role "Lender" contains valueFromList [(AdaAssetId,80000000)].
[PrintWallet] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,10000000)].
[CallCreate] Endpoint "create" called on instance 9b7060a3-466c-489a-8283-77832c1505f5 for owners [("Lender",AddressInEra (ShelleyAddressInEra ShelleyBasedEraShelley) (ShelleyAddress Testnet (KeyHashObj (KeyHash "96293f6e71868695aee1a1cc5c6f0abaeead3980be21ad6c768af452")) (StakeRefBase (KeyHashObj (KeyHash "c7abeec3f9b955c9c915260c67bfccbedd377d79a0f720a25dfc66d6"))))),("Borrower",AddressInEra (ShelleyAddressInEra ShelleyBasedEraShelley) (ShelleyAddress Testnet (KeyHashObj (KeyHash "a75822380bc8980697cc8b2f32adffa6724fa492e1fbcfb150db0056")) (StakeRefBase (KeyHashObj (KeyHash "51d103189b206767ccf93230431712d9eb735e9441c41aad1dc1ddb0")))))].
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewActiveEndpoints [].
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewObservableState (Object (fromList [("contents",Array [String "bfffa39f-39ac-4397-b34f-11b28127de97",Object (fromList [("contents",Object (fromList [("rolePayoutValidatorHash",String "d30d795bc00fa0eca1e12c1a5fcd546cd078ce4ec0d8f86f44d67585"),("rolesCurrency",Object (fromList [("unCurrencySymbol",String "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc")]))])),("tag",String "CreateResponse")])]),("tag",String "EndpointSuccess")])).
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewActiveEndpoints [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}].
[AwaitCreate] Creation confirmed with MarloweParams {rolePayoutValidatorHash = d30d795bc00fa0eca1e12c1a5fcd546cd078ce4ec0d8f86f44d67585, rolesCurrency = d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc}.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,75509500),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,12000000),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Borrower",1)].
[Follow] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 now follows instance 9b7060a3-466c-489a-8283-77832c1505f5.
[CallApplyInputs] Endpoint "apply-inputs" called on 9b7060a3-466c-489a-8283-77832c1505f5 for inputs [ClientInput (IDeposit "Lender" "Lender" (Token "" "") 50000000)] and times Just (POSIXTime {getPOSIXTime = 1645590825000},POSIXTime {getPOSIXTime = 1898051625000}).
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewActiveEndpoints [].
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewObservableState (Object (fromList [("contents",Array [String "e1209d59-9111-4034-8ca3-7fe7f9a012f0",Object (fromList [("tag",String "ApplyInputsResponse")])]),("tag",String "EndpointSuccess")])).
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewActiveEndpoints [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}].
[AwaitApplyInputs] Input application confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,24017591),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,12000000),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Borrower",1)].
[CallRedeem] Endpoint "redeem" called on d5b512a8-983e-45f9-a059-b16f4c968ca3 for role "Borrower".
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewActiveEndpoints [].
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewObservableState (Object (fromList [("contents",Array [String "bcd1dc16-8b1d-4d46-b18d-c22880e96e02",Object (fromList [("tag",String "RedeemResponse")])]),("tag",String "EndpointSuccess")])).
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewActiveEndpoints [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}].
[AwaitRedeem] Redemption confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,24017591),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,61545077),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Borrower",1)].
[CallApplyInputs] Endpoint "apply-inputs" called on d5b512a8-983e-45f9-a059-b16f4c968ca3 for inputs [ClientInput (IDeposit "Borrower" "Borrower" (Token "" "") 53000000)] and times Just (POSIXTime {getPOSIXTime = 1645590825000},POSIXTime {getPOSIXTime = 1898051625000}).
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewActiveEndpoints [].
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewObservableState (Object (fromList [("contents",Array [String "52b40a57-67b2-4d26-976f-187e8471069f",Object (fromList [("tag",String "ApplyInputsResponse")])]),("tag",String "EndpointSuccess")])).
[runContract] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 received NewActiveEndpoints [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}].
[AwaitApplyInputs] Input application confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,26017591),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,7217521),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Borrower",1)].
[CallRedeem] Endpoint "redeem" called on 9b7060a3-466c-489a-8283-77832c1505f5 for role "Lender".
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewActiveEndpoints [].
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewObservableState (Object (fromList [("contents",Array [String "8217c313-14f0-49e3-b469-47860db3d27e",Object (fromList [("tag",String "RedeemResponse")])]),("tag",String "EndpointSuccess")])).
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received NewActiveEndpoints [ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "close"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "redeem"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "auto"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs-nonmerkleized"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "apply-inputs"}, aeMetadata = Nothing},ActiveEndpoint {aeDescription = EndpointDescription {getEndpointDescription = "create"}, aeMetadata = Nothing}].
[AwaitRedeem] Redemption confirmed.
[CheckFunds] Wallet for role "Lender" contains valueFromList [(AdaAssetId,78483015),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Lender",1)].
[CheckFunds] Wallet for role "Borrower" contains valueFromList [(AdaAssetId,7217521),(AssetId "d03cfc3587b906967b62ffc8f58656d16fd7089031871095efa328cc" "Borrower",1)].
[runContract] Instance 9b7060a3-466c-489a-8283-77832c1505f5 received ContractFinished Nothing.
[Stop] Instance 9b7060a3-466c-489a-8283-77832c1505f5 stopped.
[Stop] Instance d5b512a8-983e-45f9-a059-b16f4c968ca3 stopped.
***** SUCCEEDED *****
```
