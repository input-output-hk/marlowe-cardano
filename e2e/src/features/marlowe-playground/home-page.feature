Feature: As a user I should be able to see the Marlowe Playground home page

    As a user I should be able to navigate to the marlowe playground home page and
    see all the starting point options available

    @dev
    @smoke
    @regression
    Scenario: As a user I expect to be able to see the available languages

      Given I am on the "home" page
      Then I should see "Start in Javascript" text
      And I should see "Start in Haskell" text
      And I should see "Start in Marlowe" text
      And I should see "Start in Blockly" text
      And I should see "Start in Haskell" text
      And I should see a button with "Open existing project" text
      And I should see a button with "Open an example" text

      When I click "Open existing project" button
      Then I should see "Login with github" text