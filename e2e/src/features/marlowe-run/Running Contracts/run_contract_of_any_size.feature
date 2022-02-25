Feature: Can run a contract of any size on the blockchain

  As a user, I would like to run a contract of any size (Merkleization),
  so that I am not size limited on the contracts I want to run

  Scenario: Running a smallest valid contract
    Given the "Smallest Valid" contract is loaded on Marlowe Run

    When I run the contract

    Then it should be recorded to the blockchain

  Scenario: Running the largest valid contract
    Given the "Largest Valid" contract is loaded on Marlowe Run

    When I run the contract

    Then it should be recorded to the blockchain

  Scenario: Running a contract that is too large
    Given the "Too Large Valid" contract is loaded on Marlowe Run

    When I run the contract

    Then I should see the error message: "Contract too large"

