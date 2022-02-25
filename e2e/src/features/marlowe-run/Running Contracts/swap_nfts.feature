Feature: Ability to swap NFT

  As a user, I would like to use a Marlowe Swap Contract to swap
  NFTs and/or other tokens, so that I can make a profit

  Scenario: Swap lovelace for an nft
    Given the "NFT Swap" contract is loaded on Marlowe Run

    When I run the contract
    And I enter "5000000 lovelace" to swap for "NFT"
    And I click "Submit"

    Then my wallet balance should "decrease" by "5000000 lovelace"
    And my wallet balance should "increase" by "NFT"

  Scenario: Swap lovelace for multiple nfts
    Given the "NFT Swap" contract is loaded on Marlowe Run

    When I run the contract
    And I enter "5000000 lovelace" to swap for "Multiple NFTs"
    And I click "Submit"

    Then my wallet balance should "decrease" by "5000000 lovelace"
    And my wallet balance should "increase" by "Multiple NFTs"

  Scenario: Swap nfts for lovelace
    Given the "NFT Swap" contract is loaded on Marlowe Run

    When I run the contract
    And I enter "NFT" to swap for "5000000 lovelace"
    And I click "Submit"

    Then my wallet balance should "decrease" by "NFT"
    And my wallet balance should "increase" by "5000000 lovelace"

  Scenario: Swap multiple nfts for lovelace
    Given the "NFT Swap" contract is loaded on Marlowe Run

    When I run the contract
    And I enter "Multiple NFTs" to swap for "5000000 lovelace"
    And I click "Submit"

    Then my wallet balance should "decrease" by "Multiple NFTs"
    And my wallet balance should "increase" by "5000000 lovelace"