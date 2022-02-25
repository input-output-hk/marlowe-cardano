Feature: Integration with Marlowe Playground

  As a user, I would like to export a contract directly to playground, so that I can
  customize the contract code to my liking

  Scenario: Export a contract to marlowe playground
    Given I have opened the "Javascript Escrow" in "Marlowe Run" app
    Then I should see a button with text "Export to Marlowe Playground"

    When I click "Export to Marlowe Playground" button
    Then I should see "Javascript Escrow Contract Code" contract in "playground editor"