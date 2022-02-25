Feature: Notifications of Contract Actions
  As a user, I would like to be notified if I have an action to make on a contract, 
  so that I can continue the contract

  Scenario Outline: User is assigned as "borrower" to a loan contract
    Given I have a <Integrated Wallet> wallet
    And I was assigned as "borrower" to a "loan" contract
    And I am required to "accept" the contract

    When I connect my <Integrated Wallet> wallet to Marlowe Run

    Then I should see a new notification
    And the notification requires me to "accept" the contract

  Examples:
      | Integrated Wallet |
      | Nami              |
      | Yoroi             |
      | Flint             |
      | Daedalus          |