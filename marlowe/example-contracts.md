# Marlowe Contracts

- [Simple Examples](#simple-examples)
- [ACTUS Financial Contracts](#actus-financial-contracts)
- [Examples in Marlowe Playground](#examples-in-marlowe-playground)
- [Generating Marlowe from Haskell](#generating-marlowe-from-haskell)
- [Marlowe Runtime](#marlowe-runtime)
- [Cookbook of Miscellaneous Contracts](#cookbook-of-miscellaneous-contracts)
- [Solutions to Hackathon Challenges](#solutions-to-hackathon-challenges)


## Simple Examples


### [Test of a Simple Contract](../marlowe-cli/examples/simple/ReadMe.md)

[This simple contract](../marlowe-contracts/src/Marlowe/Contracts/Trivial.hs) takes as deposit, waits for a notification, makes a payment, waits for another notification, and then closes the contract.


### [Test of a Zero-Coupon Bond](../marlowe-cli/examples/zcb/ReadMe.md)

[This zero-coupon bond](../marlowe-cli/src/Language/Marlowe/CLI/Examples/ZeroCouponBond.hs) has one party borrow and another pay back with interest. It uses role tokens.


### [Example Escrow Contract](../marlowe-cli/examples/escrow/ReadMe.md)

This escrow contract is for the purchase of an item at a price. The buyer may complain that there is a problem, asking for a refund. If the seller disputes that complaint, then a mediator decides. There are four potential outcomes:

-   ["Everything is alright"](../marlowe-cli/examples/escrow/everything-is-alright.md): no problem.
-   ["Confirm problem"](../marlowe-cli/examples/escrow/confirm-problem.md): seller agrees that there is a problem.
-   ["Dismiss claim"](../marlowe-cli/examples/escrow/dismiss-claim.md): mediator decides in favor of seller.
-   ["Confirm claim"](../marlowe-cli/examples/escrow/confirm-claim.md): mediator decides in favor of buyer.


### [Test of a Contract for Differences](../marlowe-cli/examples/cfd/ReadMe.md)

In this example execution of a contract for differences, the party and counterparty each make a deposit to cover margin, an oracle reports prices, and the price change is settled among the parties.


### [Test of a Swap Contract](../marlowe-cli/examples/swap/ReadMe.md)

[This swap contract](../marlowe-cli/src/Language/Marlowe/CLI/Examples/Swap.hs) swaps native tokens between two parties.


### [Test of a Covered Call Contract](../marlowe-cli/examples/coveredCall/ReadMe.md)

[This option contract](../marlowe-contracts/src/Marlowe/Contracts/Options.hs) transfers a token if the counter-party exercises the option.


## ACTUS Financial Contracts

[ACTUS (Algorithmic Contract Types Unified Standards)](https://www.actusfrf.org/) covers a large number of common financial contract. Marlowe implements the suite of ACTUS contracts in a Haskell package, [marlowe-actus](../marlowe-actus/README.md) that will generate the Marlowe code for an ACTUS contract.

![Taxonomy of ACTUS contracts](https://static.wixstatic.com/media/3b2673_b08695f290df4db08b733adb39e9451d~mv2_d_2136_1200_s_2.png/v1/fill/w_961,h_538,al_c,q_90,usm_0.66_1.00_0.01,enc_auto/Screenshot%202019-01-24%20at%2015_24_12.png)


### [Test of an ACTUS Contract (Zero-Coupon Bond)](../marlowe-cli/examples/actus/ReadMe.md)

This zero-coupon bond has one party borrow and another pay back with interest. It uses role tokens.


## Examples in Marlowe Playground

The following contract examples are available in [Marlowe Playground](https://marlowe-playground-staging.plutus.aws.iohkdev.io/) in Haskell, TypeScript, Marlowe, and Blockly formats. They can be exported as JSON from the Playground and run using the  `marlowe-cli` or `marlowe` tools.


### Escrow

Regulates a money exchange between a "Buyer" and a "Seller". If there is a disagreement, an "Mediator" will decide whether the money is refunded or paid to the "Seller".


### Escrow With Collateral

Regulates a money exchange between a "Buyer" and a "Seller" using a collateral from both parties to incentivize collaboration. If there is a disagreement the collateral is burned.


### Zero Coupon Bond

A simple loan. The investor pays the issuer the discounted price at the start, and is repaid the full (notional) price at the end.


### Coupon Bond Guaranteed

Debt agreement between an "Lender" and an "Borrower". "Lender" will advance the "Principal" amount at the beginning of the contract, and the "Borrower" will pay back "Interest installment" every 30 slots and the "Principal" amount by the end of 3 installments. The debt is backed by a collateral provided by the "Guarantor" which will be refunded as long as the "Borrower" pays back on time.


### Swap

Takes Ada from one party and dollar tokens from another party, and it swaps them atomically.


### Contract For Differences

"Party" and "Counterparty" deposit 100 Ada and after 60 slots is redistributed depending on the change in a given trade price reported by "Oracle". If the price increases, the difference goes to "Counterparty"; if it decreases, the difference goes to "Party", up to a maximum of 100 Ada.


### Contract For Differences with Oracle

"Party" and "Counterparty" deposit 100 Ada and after 60 slots these assets are redistributed depending on the change in price of 100 Ada worth of dollars between the start and the end of the contract. If the price increases, the difference goes to "Counterparty"; if it decreases, the difference goes to "Party", up to a maximum of 100 Ada.


## Generating Marlowe from Haskell

The [marlowe-contracts](../marlowe-contracts/src/Marlowe/Contracts) Haskell package contains the generators for the following types of Marlowe contracts:
- [Escrows](../marlowe-contracts/src/Marlowe/Contracts/Escrow.hs)
- [Forwards](../marlowe-contracts/src/Marlowe/Contracts/Forward.hs)
- [Futures](../marlowe-contracts/src/Marlowe/Contracts/Futures.hs)
- [Options](../marlowe-contracts/src/Marlowe/Contracts/Options.hs)
- [Structured Products](../marlowe-contracts/src/Marlowe/Contracts/StructuredProducts.hs)
- [Swaps](../marlowe-contracts/src/Marlowe/Contracts/Swap.hs)
- [Zero-Coupon Bonds](../marlowe-contracts/src/Marlowe/Contracts/ZeroCouponBond.hs)


## Marlowe Runtime

Marlowe Runtime is the new backend for running Marlowe contracts. Here are examples of running contracts with Marlowe Runtime:
- [ACTUS PAM (principal at maturity) contract](../marlowe-runtime/examples/deposit.ipynb)
- [Token Bidding](../marlowe-runtime/examples/token-bid.ipynb)


## Cookbook of Miscellaneous Contracts


### [Using Djed in Marlowe Contracts](../marlowe-cli/cookbook/payment-using-djed.ipynb)

In this example, Christopher Marlowe sells the Swan Theatre to Francis Beaumont for 2250 `DjedUSD`. The Swan Theatre is represented as an NFT.


### [Simple Marlowe Contract for a Token Airdrop](../marlowe-cli/cookbook/token-drop.ipynb)

This contract demonstrates the use of Cardano address in a Marlowe contract, where formerly public-key hashes were used to identify non-role parties in a contract. The contract itself is a simple airdrop of tokens to three recipients, where the airdrop must occur after a specified time (via a `Notify` containing a `TimeIntervalStart` condition).


### [Revenue-Based Loan in Marlowe](../marlowe-cli/cookbook/revenue-based-loan.ipynb)

This revenue-based loan involves variable payments that are computed as a fixed percentage of the borrower's revenue during each period of the loan. The payments continue until principal plus interest is paid off. There is no penalty for missing a payment. An oracle reports the borrower's revenue each period, prior to the borrower depositing that amount. The contract was both simulated and run on the Cardano blockchain: that involved 8 transactions.

[This video](https://vimeo.com/726500312/f90fd85ed7) shows [the contract](../marlowe-cli/cookbook/revenue-based-loan.hs) being run in Marlowe Playground.


### [English Auction](../marlowe-cli/cookbook/english-auction.ipynb)

This English Auction with five bidders and three rounds of bidding demonstrates the use of merkleization for Marlowe contracts. Because bidders may bid in any order and may bid more than once, unless they are disqualified by an illegal bid or timeout, the contract involves a combinatorial explosion of possibilities. Without merkleization, the contract JSON file is 990MB in size and contains 940k `Case` statements, but after merkleization the JSON file is 9.8MB and it contains just 1150 merkleized `Case` statements. The contract was both simulated and run on the Cardano blockchain: that involved 18 transactions.


### [Collective Loan in Marlowe](../marlowe-cli/cookbook/collective-loan.ipynb)

This collective-loan contract lets lenders deposit currency such as `DjedUSD` in exchange for liquidity tokens, and then lets borrowers draw funds from the contract as loans. After the borrowers pay back their borrowings with interest to the collective loan, the lenders then redeem their liquidity tokens for `DjedUSD` that includes profit from the lending. A central administrator manages approvals. The Marlowe contract for this collective loan is scalable to an arbitrarily large number of lenders or borrowers, so long as the trusted central administrator is relied upon to disperse role tokens on a just-in-time basis. (Less centralized versions may be feasible, but they would be more rigid in terms of sequence of participation of the lenders and borrowers.) The contract was both simulated and run on the Cardano blockchain: that involved 3 lenders, 5 borrowers, and ~60 transactions.

See also [this video](https://vimeo.com/735889853/10601168e4) that walks through this example in Marlowe Playground and on the Marlowe testnet.


### [Collective Loan in Marlowe with 30 Lenders and 70 Borrowers](../marlowe-cli/cookbook/collective-loan-30-70.ipynb)

This collective-loan contract lets lenders deposit currency such as `DjedUSD` in exchange for liquidity tokens, and then lets borrowers draw funds from the contract as loans. After the borrowers pay back their borrowings with interest to the collective loan, the lenders then redeem their liquidity tokens for `DjedUSD` that includes profit from the lending. A central administrator manages approvals. The Marlowe contract for this collective loan is scalable to an arbitrarily large number of lenders or borrowers, so long as the trusted central administrator is relied upon to disperse role tokens on a just-in-time basis. (Less centralized versions may be feasible, but they would be more rigid in terms of sequence of participation of the lenders and borrowers.) The contract was both simulated and run on the Cardano blockchain: that involved 30 lenders, 70 borrowers, and ~530 transactions.


### [Stabilized Collective Loan in Marlowe: Failure Due to Protocol Limits](../marlowe-cli/cookbook/stabilized-collective-loan-failure.ipynb)

This collective loan includes multiple investors and multiple borrowers, where the pool is supervised by an administrator and where a stabilization fund guarantees returns to investors, subject to a cap on losses.

Although this contract is simulatable without difficulty, **_this contract fails to execute_** on both the Preview testnet (due to its exceed the transaction-size limit) and the Pioneers testnet (due its execution memory and steps, even though this network has generously larger resources limits). (See Step 5 below for more details.) See [stabilized-collective-loan-success.ipynb](https://github.com/input-output-hk/marlowe-cardano/blob/2a0db652f01c6c08b3d90b6052c4b84785811d05/marlowe-cli/cookbook/stabilized-collective-loan-success.ipynb) for an example of how including extra `Notify`s works around resource limits.


### [Stabilized Collective Loan in Marlowe: Working around Protocol Limits](../marlowe-cli/cookbook/stabilized-collective-loan-success.ipynb)

This collective loan includes multiple investors and multiple borrowers, where the pool is supervised by an administrator and where a stabilization fund guarantees returns to investors, subject to a cap on losses. Although this contract is simulatable without difficulty, it would fail on a network like `preview` or `mainnet` due to its exceed the execution-memory limit if it didn't include extra `Notify` terms to split steps into small transactions. Also note that this contract is a proof of principle and that the mathematics underlying it requires review and adjustment. See [stabilized-collective-loan-failure.ipynb](https://github.com/input-output-hk/marlowe-cardano/blob/2a0db652f01c6c08b3d90b6052c4b84785811d05/marlowe-cli/cookbook/stabilized-collective-loan-failure.ipynb) for an example of how not including these extra `Notify`s causes transactions to fail.


### [Marlowe Contract for Guessing Game](../marlowe-cli/cookbook/guessing-game.ipynb)

This Marlowe contract demonstrates how to store private data on the public blockchain in a Marlowe contract. It uses the fact that it is difficult to factor the product of two prime numbers.


### [Test of a Zero-Coupon Bond](../marlowe-cli/cookbook/zcb.ipynb)

We introduce new variants of `marlowe-cli run execute` and `marlowe-cli run withdraw` that automatically handle the UTxOs management, coin selection, and balancing, so that one doesn't need to specify `--tx-in` and `--tx-out` for Marlowe transactions.


### [Test of a Swap Contract](../marlowe-cli/cookbook/swap.ipynb)

We introduce new variants of `marlowe-cli run execute` and `marlowe-cli run withdraw` that automatically handle the UTxOs management, coin selection, and balancing, so that one doesn't need to specify `--tx-in` and `--tx-out` for Marlowe transactions.


### [Simple Marlowe Transactions in the Babbage Era](../marlowe-cli/cookbook/simple-babbage.ipynb)

This notebook demonstrates submitting Marlowe transactions in the Babbage Era using the low-level functions of `marlowe-cli` to create the Marlowe's Plutus script and the datums and redeemers needed when running Marlowe transactions. It uses `cardano-cli` to build, sign, and submit the transactions.


### [Running Marlowe with a Reference Script](../marlowe-cli/cookbook/reference-script.ipynb)

This example shows how to use the `marlowe-cli` and `cardano-cli` tools to submit Marlowe transactions that involve reference scripts. The `marlowe-cli` tool is used to generate the script, datum, and redeemers, while the `cardano-cli` tool is used to submit transactions and query the blockchain. Using a reference script significantly reduces the cost of a Marlowe transaction.


### [First Marlowe Using Plutus V2 on Mainnet](../marlowe-cli/cookbook/marlowe-1st-plutusv2.ipynb)

The first Plutus V2 script ever run on `mainnet` was this simple token-drop Marlowe contract.


### [Marlowe "Swap" Contract Run on Mainnet Using a Reference Script](../marlowe-cli/cookbook/marlowe-1st-reference-script.ipynb)

This transcript documents the first-ever use of a reference script on `mainnet`. The scenario is that John Fletcher trades 300 `Globe` tokens with Thomas Kyd for 500 `Swan` tokens.


### [Coupon-Bond Guaranteed on Mainnet with Reference Scripts](../marlowe-cli/cookbook/coupon-bond-guaranteed.ipynb)

This example demonstrates the use of `marlowe-cli` with Plutus V2 reference scripts for the two Marlowe validators on `mainnet`. It is a three-party contract for a guaranteed loan, where a _guarantor_ deposits principal plus interest before a _lender_ makes the loan to the _borrower_. As the borrower pays principal and/or interest, the guarantor receives the relevant part of their guarantee back while the lender receives the repayment. The total fee (Plutus and non-Plutus) paid in this example is approximately 10 Ada.


### [A Geo-Located Smart Contract Using Cardano Beam and Marlowe](../marlowe-cli/cookbook/beamer.ipynb)

We present a novel synergy between the Cardano Beam Dapp and the Marlowe Dapp, where Cardano Beam is used to geolocate the role tokens needed for authorization of Marlowe transactions. This enables use cases such as geocaching, scavenger hunts, or token drops that are linked to Marlowe contracts: this melds the geolocation capabilities of Cardano Beam with the contract-logic capabilities of Marlowe.

A video that shows this example being run is available [here](https://youtu.be/DmkYen0eaV0).


## Solutions to Hackathon Challenges


### English auctions (difficult)

In live terms, English auctions are where bids are announced by the bidders and winners pay what they bid to receive the object. The common operational method of the format is that it is an ascending bid auction in which bids are open for all to see. The winner is the highest bidder and the price is the highest bid.

Solution: [Haskell](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/app/EnglishAuction.hs), [Marlowe](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/marlowe/EnglishAuction.marlowe), [core JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/core-json/EnglishAuction.json) [extended JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/extended-json/EnglishAuction.json), [TypeScript](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/typescript/EnglishAuction.ts).


### Dutch auctions (difficult)

Dutch auctions are the reverse of English auctions whereby the price begins high and is systematically lowered until a buyer accepts the price.

Solution: [Haskell](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/app/DutchAuction.hs), [Marlowe](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/marlowe/DutchAuction.marlowe), [core JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/core-json/DutchAuction.json), [extended JSON](solutions/extended-json/DutchAuction.json), [TypeScript](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/typescript/DutchAuction.ts).


### First-price open-bid (easy)

First-price open-bid auctions are when a single bid is made by all bidding parties and the single highest bidder wins, and pays what they bid. This contract is not realistic because it can easily be gamed by waiting to bid last.

Solution: [Haskell](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/app/FirstPriceBid.hs), [Marlowe](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/marlowe/FirstPriceBid.marlowe), [core JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/core-json/FirstPriceBid.json), [extended JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/extended-json/FirstPriceBid.json), [TypeScript](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/typescript/FirstPrice.ts).

A very difficult variant of this in Marlowe is the first-price *sealed-bid* auction, where participants commit their bids before any of them reveal their bids.


### Second-price open-bid (intermediate)

Second-price open-bid auctions are when a single bid is made by all bidding parties and the single highest bidder wins, and pays what the second-highest bidder bid. This contract is not realistic because it can easily be gamed through collusion.

Solution: [Haskell](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/app/SecondPriceBid.hs), [Marlowe](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/marlowe/SecondPriceBid.marlowe), [core JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/core-json/SecondPriceBid.json), [extended JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/extended-json/SecondPriceBid.json), [TypeScript](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/typescript/SecondPrice.ts).

A very difficult variant of this in Marlowe is the Vickrey second-price *sealed-bid* auction, where participants commit their bids before any of them reveal their bids.


### Reverse auction (intermediate)

Reverse auctions are where the roles of buyer and seller are reversed. Multiple sellers compete to obtain the buyer's business and prices typically decrease over time as new offers are made.

Solution: [Haskell](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/app/ReverseAuction.hs), [Marlowe](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/marlowe/ReverseAuction.marlowe), [core JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/core-json/ReverseAuction.json), [extended JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/extended-json/ReverseAuction.json), [TypeScript](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/typescript/ReverseAuction.ts).


### Bidding fee auction (difficult)

A bidding fee auction (also known as a penny auction) requires customers to pay the auction owner for bids, which they can increment an auction price one unit of currency at a time.  On English auctions for example, the price goes up in 1 pence (0.01 GBP) increments.

Solution: [Haskell](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/app/BiddingFee.hs), [Marlowe](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/marlowe/BiddingFee.marlowe), [core JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/core-json/BiddingFee.json), [extended JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/extended-json/BiddingFee.json), [TypeScript](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/typescript/BiddingFee.ts).


### Stable coin (easy)

The Marlowe analog of a stable coin is a contract that pays the party the value in a base currency (say, USD) of the ADA that they initially deposit in a contract. A counterparty deposits collateral, according to a specified *reserve ratio* (i.e., the ratio of the total ADA in the contract to the ADA deposited by the party), to cover price changes in ADA. When the party withdraws their funds, they receive the ADA corresponding to the base-currency value initially deposited, and the counterparty receives the remaining ADA.

Solution: [Haskell](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/app/StableCoin.hs), [Marlowe](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/marlowe/StableCoin.marlowe), [core JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/core-json/StableCoin.json), [extended JSON](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/extended-json/StableCoin.json), [TypeScript](https://github.com/input-output-hk/marlowe-hackathons/tree/main/austin-2022/solutions/typescript/StableCoin.ts).
