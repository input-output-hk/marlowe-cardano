## Start a new marlowe contract
#### UC-CONTRACT-1

A user can create a new instance of a Marlowe contract through the pre-defined templates.
Once the terms of the contract has been defined, each participant that has a Role in the
contract will receive a Role token to move the contract forward.

## Receive a role token for a marlowe contract
#### UC-CONTRACT-2

When a user receives a role token it becomes a participant of the Marlowe Contract and can apply an input at the appropiate time to move it forward.
TODO: We should differentiate a little more the flows of starting a new contract and receiving a new token by providing a modal or something letting the user know that this is a contract they didn't start.

## Apply an input to a contract
#### UC-CONTRACT-3

At each contract step, a participant with the correct Role token can apply an input (make a deposit, take a choice) to advance the contract.

## Redeem payments
#### UC-CONTRACT-4

Redeem is the way a participant can get the money out of the contract and back to their account.
At the moment the payments are auto-redeemed, but eventually this should be a manual action, as redeeming incurs in fees.
