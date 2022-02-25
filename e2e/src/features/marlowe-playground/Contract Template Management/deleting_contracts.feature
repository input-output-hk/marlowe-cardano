Feature: Deleting Contracts

  As a user, I would like to delete an existing contract template, so that
  I discard unneeded contracts

  Scenario: Deleting a saved contract

    Given I have "Blockly Escrow Contract Code" saved in Gist

    When I visit "marlowe plagyround home page"
    And I click "Open exsiting project" button
    And I login with "github"

    Then I should see "Blockly Escrow Contract Code" saved in Gist

    When I click the "delete" button

    Then I should see message: "Your contract was deleted"
    And I should not see "Blockly Escrow Contract Code" saved in Gist