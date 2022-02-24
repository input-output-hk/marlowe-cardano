Feature: Integration with Marlowe Run

  As a user, I would like to export a contract from playground to marlowe run, so that
  I can test my contract against a real blockchain

  Scenario: Export an existing Marlowe Contract Template to Marlowe Run
    Given I have opened the "Javascript Escrow" example contract template
    Then I should see a button with text "Export to Marlowe Run"

    When I click "Export to Marlowe Run" button
    Then I should see "Escrow" contract in "Marlowe Run"

  Scenario: Exporting a newly created Marlowe Contract Template to Marlowe Run
    Given I am in "Marlowe Playground"

    When I write a(n) "Javascript Escrow" contract template
    And I click "Export to Marlowe Run" button

    Then I should see "Escrow" contract in "Marlowe Run"

  Scenario: Attempting to export an invalid contract template to Marlowe Run
    Given I am in "Marlowe Playground"

    When I write a(n) "invalid" contract template

    Then the "Export to Marlowe Run" button should be "disabled"