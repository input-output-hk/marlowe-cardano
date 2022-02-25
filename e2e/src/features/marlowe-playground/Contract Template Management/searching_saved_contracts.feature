Feature: Searching saved contract

  As a user, I would like to search a saved contract from my github Gist
  account, so that I can more easily retrieve a contract at a later time

  Scenario: Searching for Javascript Escrow contract
    Given I have "Blockly Escrow Contract Code" saved in Gist
    And I have "Javascript Escrow Contract Code" saved in Gist
    And I have "Haskell Escrow Contract Code" saved in Gist


    When I visit "marlowe plagyround home page"
    And I click "Open exsiting project" button
    And I login with "github"

    Then I should see a "search" field

    When I enter "Javascript" into the "search" field

    Then the "available contracts" list should contain "Javascript Escrow Contract Code"
    And the "available contracts" list should not contain "Blockly Escrow Contract Code"
    And the "available contracts" list should not contain "Haskell Escrow Contract Code"


