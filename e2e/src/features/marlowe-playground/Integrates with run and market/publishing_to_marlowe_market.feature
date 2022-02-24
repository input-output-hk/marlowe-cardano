Feature: Integrates with Marlowe Market

  As a user, I would like to publish a contract from playground directly to marlowe market,
  so that I can sell or share my contract with the community

  Scenario: Publish an existing Marlowe Contract Template to Marlowe Market
    Given I have opened the "Javascript Escrow" example contract template
    Then I should see a button with text "Publish to Marlowe Market"

    When I click "Publish to Marlowe Market" button

    Then I should see "Escrow" contract in "Marlowe Market"

  Scenario: Publishing a newly created Marlowe Contract Template to Marlowe Market
    Given I am in "Marlowe Playground"

    When I write a(n) "Javascript Escrow" contract template
    And I click "Publish to Marlowe Market" button

    Then I should see "Escrow" contract in "Marlowe Market"

  Scenario: Attempting to publish an invalid contract template to Marlowe Market
    Given I am in "Marlowe Playground"

    When I write a(n) "invalid" contract template

    Then the "Publish to Marlowe Market" button should be "disabled"