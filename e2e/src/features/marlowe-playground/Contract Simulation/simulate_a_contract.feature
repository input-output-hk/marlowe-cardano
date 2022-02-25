Feature: Simulate a contract

  As a user, I would like to simulate a contract on marlowe playground,
  so that I can confirm that my contract will behave as expected

  Scenario: Successfully simulating a Javascript Escrow Contract

    Given I am on the "playground editor" page
    And the "playground editor" contains "Javascript Escrow Contract Code"

    When I click the "Compile" button
    And I click the "Send to Simulator" button

    Then I should see "SIMULATION HAS NOT STARTED YET"

    When I enter "5" in "Constant for Price" field
    And I click "Start simulation" button

    Then the contract should simulate Successfully