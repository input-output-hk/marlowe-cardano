Feature: View transaction history by Contract

  As a user, I would like to see a list of my transactions for contract with links
  to cardano scan, so that I can view details of my transactions recorded on the blockchain

  Scenario: Viewing past contracts page for list of transactions

    Given I have participated in "Simple Swap" contract
    And I have participated in a "loan" contract

    When I visit "past contracts" page

    Then I should see "Simple Swap"
    And I should see transactions under "Simple Swap"
    And each "Simple Swap" transaction should have a link to "cardano scan"
    And I should see "loan"
    And I should see transactions under "loan"
    And each "loan" transaction should have a link to "cardano scan"