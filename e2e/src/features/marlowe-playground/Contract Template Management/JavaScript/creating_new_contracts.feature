Feature: Creating a new contract with Javascript
  As a user, I would like to create a contract template using javascript,
  so that I can write contracts in my preferred language

  @dev
  @smoke
  Scenario: Creating a new Javascript contract template
    Given I am on the "home" page of the "marlowe-playground" application

    When I click the "Start in Javascript" link

    Then I am directed to the "marlowe-playground-js" page
    And the "editor header" should contain "New Project" text
    # And the "playground editor" should contain "empty javascript contract code" text

    # When I fill in the "playground editor" input with "javascript-escrow" contract code
    # And I click the "Compile" button

    # Then the "Send To Simulator" button should be "enabled"

  @dev
  Scenario: Using an existing Javascript contract template
    Given I am on the "home" page of the "marlowe-playground" application

    When I click the "Open an example" button
    And I click the "Escrow Javascript" button

    Then I am directed to the "marlowe-playground-js" page
    And the "editor header" should contain "Purchase" text
    # And the "playground editor" should contain "javascript-escrow" contract code

    # When I click "Compile" button

    # Then the "Send To Simulator" button should be "enabled"