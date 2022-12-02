# Cookbook of Miscellaneous Contracts


## [Using Djed in Marlowe Contracts](./payment-using-djed.ipynb)

In this example, Christopher Marlowe sells the Swan Theatre to Francis Beaumont for 2250 `DjedUSD`. The Swan Theatre is represented as an NFT.


## [Simple Marlowe Contract for a Token Airdrop](./token-drop.ipynb)

This contract demonstrates the use of Cardano address in a Marlowe contract, where formerly public-key hashes were used to identify non-role parties in a contract. The contract itself is a simple airdrop of tokens to three recipients, where the airdrop must occur after a specified time (via a `Notify` containing a `TimeIntervalStart` condition).


## [Revenue-Based Loan in Marlowe](./revenue-based-loan.ipynb)

This revenue-based loan involves variable payments that are computed as a fixed percentage of the borrower's revenue during each period of the loan. The payments continue until principal plus interest is paid off. There is no penalty for missing a payment. An oracle reports the borrower's revenue each period, prior to the borrower depositing that amount. The contract was both simulated and run on the Cardano blockchain: that involved 8 transactions.

[This video](https://vimeo.com/726500312/f90fd85ed7) shows [the contract](./revenue-based-loan.hs) being run in Marlowe Playground.


## [English Auction](./english-auction.ipynb)

This English Auction with five bidders and three rounds of bidding demonstrates the use of merkleization for Marlowe contracts. Because bidders may bid in any order and may bid more than once, unless they are disqualified by an illegal bid or timeout, the contract involves a combinatorial explosion of possibilities. Without merkleization, the contract JSON file is 990MB in size and contains 940k `Case` statements, but after merkleization the JSON file is 9.8MB and it contains just 1150 merkleized `Case` statements. The contract was both simulated and run on the Cardano blockchain: that involved 18 transactions.


## [Collective Loan in Marlowe](./collective-loan.ipynb)

This collective-loan contract lets lenders deposit currency such as `DjedUSD` in exchange for liquidity tokens, and then lets borrowers draw funds from the contract as loans. After the borrowers pay back their borrowings with interest to the collective loan, the lenders then redeem their liquidity tokens for `DjedUSD` that includes profit from the lending. A central administrator manages approvals. The Marlowe contract for this collective loan is scalable to an arbitrarily large number of lenders or borrowers, so long as the trusted central administrator is relied upon to disperse role tokens on a just-in-time basis. (Less centralized versions may be feasible, but they would be more rigid in terms of sequence of participation of the lenders and borrowers.) The contract was both simulated and run on the Cardano blockchain: that involved 3 lenders, 5 borrowers, and ~60 transactions.

See also [this video](https://vimeo.com/735889853/10601168e4) that walks through this example in Marlowe Playground and on the Marlowe testnet.


## [Collective Loan in Marlowe with 30 Lenders and 70 Borrowers](./collective-loan-30-70.ipynb)

This collective-loan contract lets lenders deposit currency such as `DjedUSD` in exchange for liquidity tokens, and then lets borrowers draw funds from the contract as loans. After the borrowers pay back their borrowings with interest to the collective loan, the lenders then redeem their liquidity tokens for `DjedUSD` that includes profit from the lending. A central administrator manages approvals. The Marlowe contract for this collective loan is scalable to an arbitrarily large number of lenders or borrowers, so long as the trusted central administrator is relied upon to disperse role tokens on a just-in-time basis. (Less centralized versions may be feasible, but they would be more rigid in terms of sequence of participation of the lenders and borrowers.) The contract was both simulated and run on the Cardano blockchain: that involved 30 lenders, 70 borrowers, and ~530 transactions.


## [Stabilized Collective Loan in Marlowe: Failure Due to Protocol Limits](./stabilized-collective-loan-failure.ipynb)

This collective loan includes multiple investors and multiple borrowers, where the pool is supervised by an administrator and where a stabilization fund guarantees returns to investors, subject to a cap on losses.

Although this contract is simulatable without difficulty, **_this contract fails to execute_** on both the Preview testnet (due to its exceed the transaction-size limit) and the Pioneers testnet (due its execution memory and steps, even though this network has generously larger resources limits). (See Step 5 below for more details.) See [stabilized-collective-loan-success.ipynb](https://github.com/input-output-hk/marlowe-cardano/blob/2a0db652f01c6c08b3d90b6052c4b84785811d05/marlowe-cli/cookbook/stabilized-collective-loan-success.ipynb) for an example of how including extra `Notify`s works around resource limits.


## [Stabilized Collective Loan in Marlowe: Working around Protocol Limits](./stabilized-collective-loan-success.ipynb)

This collective loan includes multiple investors and multiple borrowers, where the pool is supervised by an administrator and where a stabilization fund guarantees returns to investors, subject to a cap on losses. Although this contract is simulatable without difficulty, it would fail on a network like `preview` or `mainnet` due to its exceed the execution-memory limit if it didn't include extra `Notify` terms to split steps into small transactions. Also note that this contract is a proof of principle and that the mathematics underlying it requires review and adjustment. See [stabilized-collective-loan-failure.ipynb](https://github.com/input-output-hk/marlowe-cardano/blob/2a0db652f01c6c08b3d90b6052c4b84785811d05/marlowe-cli/cookbook/stabilized-collective-loan-failure.ipynb) for an example of how not including these extra `Notify`s causes transactions to fail.


## [Marlowe Contract for Guessing Game](./guessing-game.ipynb)

This Marlowe contract demonstrates how to store private data on the public blockchain in a Marlowe contract. It uses the fact that it is difficult to factor the product of two prime numbers.


## [Test of a Zero-Coupon Bond](./zcb.ipynb)

We introduce new variants of `marlowe-cli run execute` and `marlowe-cli run withdraw` that automatically handle the UTxOs management, coin selection, and balancing, so that one doesn't need to specify `--tx-in` and `--tx-out` for Marlowe transactions.


## [Test of a Swap Contract](./swap.ipynb)

We introduce new variants of `marlowe-cli run execute` and `marlowe-cli run withdraw` that automatically handle the UTxOs management, coin selection, and balancing, so that one doesn't need to specify `--tx-in` and `--tx-out` for Marlowe transactions.


## [Simple Marlowe Transactions in the Babbage Era](./simple-babbage.ipynb)

This notebook demonstrates submitting Marlowe transactions in the Babbage Era using the low-level functions of `marlowe-cli` to create the Marlowe's Plutus script and the datums and redeemers needed when running Marlowe transactions. It uses `cardano-cli` to build, sign, and submit the transactions.


## [Running Marlowe with a Reference Script](./reference-script.ipynb)

This example shows how to use the `marlowe-cli` and `cardano-cli` tools to submit Marlowe transactions that involve reference scripts. The `marlowe-cli` tool is used to generate the script, datum, and redeemers, while the `cardano-cli` tool is used to submit transactions and query the blockchain. Using a reference script significantly reduces the cost of a Marlowe transaction.


## [First Marlowe Using Plutus V2 on Mainnet](./marlowe-1st-plutusv2.ipynb)

The first Plutus V2 script ever run on `mainnet` was this simple token-drop Marlowe contract.


## [Marlowe "Swap" Contract Run on Mainnet Using a Reference Script](./marlowe-1st-reference-script.ipynb)

This transcript documents the first-ever use of a reference script on `mainnet`. The scenario is that John Fletcher trades 300 `Globe` tokens with Thomas Kyd for 500 `Swan` tokens.


## [Coupon-Bond Guaranteed on Mainnet with Reference Scripts](./coupon-bond-guaranteed.ipynb)

This example demonstrates the use of `marlowe-cli` with Plutus V2 reference scripts for the two Marlowe validators on `mainnet`. It is a three-party contract for a guaranteed loan, where a _guarantor_ deposits principal plus interest before a _lender_ makes the loan to the _borrower_. As the borrower pays principal and/or interest, the guarantor receives the relevant part of their guarantee back while the lender receives the repayment. The total fee (Plutus and non-Plutus) paid in this example is approximately 10 Ada.


## [A Geo-Located Smart Contract Using Cardano Beam and Marlowe](./beamer.ipynb)

We present a novel synergy between the Cardano Beam Dapp and the Marlowe Dapp, where Cardano Beam is used to geolocate the role tokens needed for authorization of Marlowe transactions. This enables use cases such as geocaching, scavenger hunts, or token drops that are linked to Marlowe contracts: this melds the geolocation capabilities of Cardano Beam with the contract-logic capabilities of Marlowe.

A video that shows this example being run is available [here](https://youtu.be/DmkYen0eaV0).

