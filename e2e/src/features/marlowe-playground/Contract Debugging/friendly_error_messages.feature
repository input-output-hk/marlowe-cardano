Feature: Friendly error messages

  As a contract writer, when I'm writing a contract with errors,
  I would like to see a human readable error message,
  So that I know what errors to fix

  Scenario: Writing a contract without a close contract

    Given I am on the "playground editor" page
    And the "playground editor" contains "Javascript Escrow Contract Code"

    When I delete "const buyer: Party = Role(\"Buyer\");" from line "17"
    And I click the "Compile" button

    Then the compilation should error
    And I should see the error message: "Cannot find name 'buyer'."