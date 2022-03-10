Feature: Creating a new contract with Javascript
  As a user, I would like to create a contract template using javascript,
  so that I can write contracts in my preferred language

  @dev
  Scenario: Creating a new Javascript contract template
    Given I am on the "home" page

    When I click the "Start in Javascript" button

    Then I am directed to the "marlowe-playground-js" page
    And the "editor header" should contain "New Project" text
    # And the "playground editor" should contain "empty javascript contract code" text

    # When I enter "Javascript Escrow Contract Code" into the "playground editor"
    # And I click "Compile" button

    # Then the "Send To Simulator" button should be "enabled"

  Scenario: Using an existing Javascript contract template
    Given I am on the "home page"

    When I click "Open an example" button
    And I click "Javascript" under "Escrow"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Javascript Escrow Contract Code"

    When I click "Compile" button

    Then the "Send To Simulator" button should be "enabled"