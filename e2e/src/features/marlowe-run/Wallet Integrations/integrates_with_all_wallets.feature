Feature: Integrates with All Wallets

  As a user, I would like to use a light wallet to create transactions on Marlowe Run,
  so that I can use the wallet of my preference

  Scenario Outline: Creating Valid Transactions with all integrated wallets: 
    Given the "Simple Escrow" contract is loaded on Marlowe Run

    When I connect with <Integrated Wallet> wallet
    And I run the contract

    Then it should be recorded to the blockchain

  Examples:
      | Integrated Wallet |
      | Nami              |
      | Yoroi             |
      | Flint             |

  @dev
  Scenario: Creating a new wallet on the homepage
    Given I am on the "home" page of the "marlowe run" application

    When I click the "Generate testnet wallet" button
    And I fill in the "Wallet nickname" input with "main"
    And I click the "Create wallet" button

    Then I should see the "mnemonic phrase"
    And I copy the "mnemonic phrase" text
    And I click the "Ok" button

    And I fill in the "mnemonic phrase textbox" input with "mnemonic phrase" from the clipboard
    And I click the "Ok" button

    Then I should see a button with "Choose a template" text


    # Then I should see "Create testnet wallet" text