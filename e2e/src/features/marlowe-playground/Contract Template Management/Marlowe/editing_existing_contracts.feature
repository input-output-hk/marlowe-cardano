Feature: Editing existing contracts with Marlowe

  As a user, I would like to edit an existing contract template,
  so that I can apply updates to or customize a contract

  Scenario: Editing an existing Marlowe contract template
    Given I am on the "home page"

    When I click "Open an example" button
    And I click "Marlowe" under "Escrow"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Marlowe Escrow Contract Code"

    When I enter "Marlowe Swap Contract Code" in "playground editor"

    Then the "Send To Simulator" button should be "enabled"

  Scenario: Editing a saved Marlowe contract template
    Given I am on the "home page"

    When I click "Open exsiting project" button
    And I login with "github"
    And I select "Escrow" from "github"

    Then I should be on the "Marlowe Editor" page
    And I should see "Purchase" as the contract header
    And the "playground editor" should contain "Marlowe Escrow Contract Code"

    When I enter "Marlowe Swap Contract Code" in "playground editor"

    Then the "Send To Simulator" button should be "enabled"