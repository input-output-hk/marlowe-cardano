Feature: Creating a new contract with Marlowe
  As a user, I would like to create a contract template using Marlowe,
  so that I can write contracts in my preferred language

  Scenario: Creating a new Marlowe contract template
    Given I am on the "home page"

    When I click "Start in Marlowe" button

    Then I should be on the "Marlowe Editor" page
    And I should see "New Project" as the contract header
    And the "playground editor" should contain "empty Marlowe contract code"

    When I enter "Marlowe Escrow Contract Code" into the "playground editor"

    Then the "Send To Simulator" button should be "enabled"

  Scenario: Using an existing Marlowe contract template
    Given I am on the "home page"

    When I click "Open an example" button
    And I click "Marlowe" under "Escrow"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Marlowe Escrow Contract Code"

    Then the "Send To Simulator" button should be "enabled"