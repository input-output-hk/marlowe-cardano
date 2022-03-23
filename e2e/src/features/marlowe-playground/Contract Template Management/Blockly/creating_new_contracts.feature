Feature: Creating a new contract with Blockly
  As a user, I would like to create a contract template using Blockly,
  so that I can write contracts in my preferred language

  Scenario: Creating a new Blockly contract template
    Given I am on the "home page"

    When I click the "Start in Blockly" button

    Then I am directed to the "marlowe-playground-blockly" page
    And the "editor header" should contain "New Project" text
    # And the "playground editor" should contain "empty  Blockly contract code"

    # When I enter "Blockly Escrow Contract Code" into the "playground editor"

    Then the "Send To Simulator" button should be "enabled"

  Scenario: Using an existing Blockly contract template
    Given I am on the "home page"

    When I click "Open an example" button
    And I click "Blockly" under "Escrow"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Blockly Escrow Contract Code"

    Then the "Send To Simulator" button should be "enabled"