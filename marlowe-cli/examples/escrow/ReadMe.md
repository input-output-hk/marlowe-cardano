# Example Escrow Contract

This escrow contract is for the purchase of an item at a price. The buyer may complain that there is a problem, asking for a refund. If the seller disputes that complaint, then a mediator decides.
There are four potential outcomes:

*   ["Everything is alright"](everything-is-alright.md): no problem.
*   ["Confirm problem"](confirm-problem.md): seller agrees that there is a problem.
*   ["Dismiss claim"](dismiss-claim.md): mediator decides in favor of seller.
*   ["Confirm claim"](confirm-claim.md): mediator decides in favor of buyer.

The following flow chart illustrates the actions and decisions involved.

![Flow chart for escrow contract.](flow-chart.svg)


## Marlowe Contract

The [Marlowe escrow contract](../../src/Language/Marlowe/CLI/Examples/Escrow.hs) embodies the logic for this contract:

<table>
<tr>
<td>
<pre>
When
  [
    Case (Deposit seller buyer ada 256)
      ( When
        [
          Case (Choice (ChoiceId "Everything is alright" (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")) [Bound 0 0])
            Close
        , Case (Choice (ChoiceId "Report problem" (PK "6d96cdffd43fd91d22b19e7013192b023cf39180773f9ad909abea79")) [Bound 1 1])
            ( Pay seller (Account buyer) ada 256 )
            ( When
              [
                Case (Choice (ChoiceId "Confirm problem" (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")) [Bound 1 1])
                  Close
              , Case (Choice (ChoiceId "Dispute problem" (PK "d735da025e42bdfeb92e325fc530da3ac732a47726c6cee666a6ea5a")) [Bound 0 0])
                ( When
                  [
                    Case (Choice (ChoiceId "Dismiss claim" (PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274")) [Bound 0 0])
                      ( Pay buyer (Account seller) ada 256 )
                      Close
                  , Case (Choice (ChoiceId "Confirm claim" (PK "2e0e34cb70301903309c3c5b81000280f31a807de62153302d4aa274")) [Bound 1 1])
                      Close
                  ]
                  2159803
                  Close
                )
              ]
              2073403
            ) Close
        ]
        1987003
        Close
      )
  ]
  1900603
  Close
</pre>
</td>
<td>
<img alt="A Marlowe Contract for Escrow" src="escrow-0.png"/>
</td>
</tr>
</table>


## Transactions

The four paths through the flow chart variously involve three, four, or five transactions:

|         | ["Everything is alright"](everything-is-alright.md) | ["Confirm problem"](confirm-problem.md)     | ["Dismiss claim"](dismiss-claim.md)         | ["Confirm claim"](confirm-claim.md)         |
|---------|---------------------------------------------        |---------------------------------------------|---------------------------------------------|---------------------------------------------|
| 1.      | Mediator provides minimum ADA.                      | Mediator provides minimum ADA.              | Mediator provides minimum ADA.              | Mediator provides minimum ADA.              |
| 2.      | Buyer deposits funds into seller's account.         | Buyer deposits funds into seller's account. | Buyer deposits funds into seller's account. | Buyer deposits funds into seller's account. |
| 3.      | Buyer reports that everything is alright.           | Buyer reports that there is a problem.      | Buyer reports that there is a problem.      | Buyer reports that there is a problem.      |
| 4.      | - - -                                               | Seller confirms that there is a problem.    | Seller disputes that there is a problem.    | Seller disputes that there is a problem.    |
| 5.      | - - -                                               | - - -                                       | Mediator dismisses the buyer's claim.       | Mediator confirms the buyer's claim.        |
| Outcome | Seller receives purchase price.                     | Buyer receives refund.                      | Seller receives purchase price.             | Buyer receives refund.                      |
|         | Mediator receives their minimum ADA back.           | Mediator receives their minimum ADA back.   | Mediator receives their minimum ADA back.   | Mediator receives their minimum ADA back.   |
