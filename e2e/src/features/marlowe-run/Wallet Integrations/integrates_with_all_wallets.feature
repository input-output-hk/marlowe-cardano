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