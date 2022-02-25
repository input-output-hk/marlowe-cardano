Feature: Editing existing contracts

  As a user, I would like to edit an existing contract template,
  so that I can apply updates to or customize a contract

  Scenario: Editing an existing Javascript contract template
    Given I am on the "home page"

    When I click "Open an example" button
    And I click "Javascript" under "Escrow"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Javascript Escrow Contract Code"

    When I enter "Javascript Swap Contract Code" in "playground editor"
    And I click the "compile" button

    Then the "Send To Simulator" button should be "enabled"

  Scenario: Editing a saved Javascript contract template
    Given I am on the "home page"

    When I click "Open esiting project" button
    And I login with "github"
    And I select "Escrow" from "github"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Javascript Escrow Contract Code"

    When I enter "Javascript Swap Contract Code" in "playground editor"
    And I click the "compile" button

    Then the "Send To Simulator" button should be "enabled"