.. _using-marlowe:

Using Marlowe from the ghci command line
========================================

This tutorial shows you how to use Marlowe from within Haskell, and in
particular shows how to exercise a contract using the semantics given
earlier.

Marlowe in Haskell
------------------

This tutorial works in the version of Marlowe which can be found in the
``main`` branch of the `marlowe-cardano repository <https://github.com/input-output-hk/marlowe-cardano>`_.
We can run ghci by using the `nix-shell` available in `marlowe-dependency-docs repository <https://github.com/input-output-hk/marlowe-dependency-docs>`_:

.. code:: bash

   git clone "https://github.com/input-output-hk/marlowe-dependency-docs.git"
   cd marlowe-dependency-docs
   nix-shell
   ghci

A standalone version and formalisation of the semantics can also be found in
the `marlowe repository <https://github.com/input-output-hk/marlowe>`_, but
some details may be slightly differently, like the use of slots instead of POSIX time.

Stepping through contracts
--------------------------

As we saw earlier the semantics of a single transaction are defined by
the function

.. code:: haskell

   computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput

where the types are defined like this:

.. code:: haskell

   data TransactionInput = TransactionInput
       { txInterval :: TimeInterval
       , txInputs   :: [Input] }
          
   data TransactionOutput =
      TransactionOutput
         { txOutWarnings :: [TransactionWarning]
         , txOutPayments :: [Payment]
         , txOutState    :: State
         , txOutContract :: Contract }
      | Error TransactionError

and ``States`` are defined like this, with a helper function to define
an initially empty state:

.. code:: haskell

   data State = State { accounts    :: Accounts
                      , choices     :: Map ChoiceId ChosenNum
                      , boundValues :: Map ValueId Integer
                      , minTime     :: POSIXTime }

   emptyState :: POSIXTime -> State
   emptyState sn = State { accounts = Map.empty
                         , choices  = Map.empty
                         , boundValues = Map.empty
                         , minTime = sn }

We can use the facilities of ``ghci`` to step through a contract one
transaction at a time, and, here, we will do that with the embedded
escrow contract contained in
`Escrow.hs <https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-contracts/src/Marlowe/Contracts/Escrow.hs>`_.

To load the contract into the interpreter we first import some libraries
and create utility functions:

.. code:: haskell

   Prelude> :set -XOverloadedStrings
   Prelude> import qualified Marlowe.Contracts.Escrow as Escrow
   Prelude Escrow> import qualified Language.Marlowe.Extended as EM
   Prelude Escrow EM> import Language.Marlowe as M
   Prelude Escrow EM M> import qualified Data.Time.Clock.POSIX as P
   Prelude Escrow EM M P> :set prompt "> "
   > let toPOSIX = POSIXTime . floor . P.utcTimeToPOSIXSeconds . read
   > let toEMPOSIX = EM.POSIXTime . floor . P.utcTimeToPOSIXSeconds . read

The example is written in Extended Marlowe, so we first need to convert it
to core Marlowe in order to execute it:

.. code:: haskell

   > let Just contract = EM.toCore $ Escrow.escrow (EM.Constant 450) "bob" "alice" "carol" (toEMPOSIX "2023-02-01 00:00:00.000000 UTC") (toEMPOSIX "2023-03-01 00:00:00.000000 UTC") (toEMPOSIX "2023-04-01 00:00:00.000000 UTC") (toEMPOSIX "2023-05-01 00:00:00.000000 UTC") :: Maybe Contract

Now we can single step it using the facility to make local bindings:

.. code:: haskell

   > let (TransactionOutput txWarn1 txPay1 state1 con1) = computeTransaction (TransactionInput (toPOSIX "2023-01-01 00:00:00.000000 UTC", toPOSIX "2023-01-31 23:59:59.000000 UTC") [NormalInput (IDeposit "bob" "alice" ada 450)]) (emptyState 0) contract

In doing this we have pattern matched the output of an application of
``computeTransaction``, which takes three inputs: the second is an
initial state (at slot number 0) and the third is the initial escrow
contract. The first is a ``TransactionInput`` which contains a
``TimeInterval`` – here ``((toPOSIX "2023-01-01 00:00:00.000000 UTC"), (toPOSIX "2023-01-31 23:59:59.000000 UTC"))`` – and
a deposit of 450 Lovelace from ``"alice"`` into ``bob``'s' account namely
``NormalInput (IDeposit "bob" "alice" ada 450)``.

.. note::

   If you want to try this for yourself in ghci, you can copy and paste
   from the code examples: they are in horizontally scrolling windows.

The output is matched with
``TransactionOutput txWarn1 txPay1 state1 con1`` so that we can examine
the various components separately:

.. code:: haskell

   > txWarn1
   []
   >  txPay1
   []
   > state1
   State {accounts = Map {unMap = [(("bob",Token "" ""),450)]}, choices = Map {unMap = []}, boundValues = Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1672531200}}
   > con1
   When [Case (Choice (ChoiceId "Everything is alright" "alice") [Bound 0 0]) Close,Case (Choice (ChoiceId "Report problem" "alice") [Bound 1 1]) (Pay "bob" (Account "alice") (Token "" "") (Constant 450) (When [Case (Choice (ChoiceId "Confirm problem" "bob") [Bound 1 1]) Close,Case (Choice (ChoiceId "Dispute problem" "bob") [Bound 0 0]) (When [Case (Choice (ChoiceId "Dismiss claim" "carol") [Bound 0 0]) (Pay "alice" (Account "bob") (Token "" "") (Constant 450) Close),Case (Choice (ChoiceId "Confirm claim" "carol") [Bound 1 1]) Close] (POSIXTime {getPOSIXTime = 1682899200}) Close)] (POSIXTime {getPOSIXTime = 1680307200}) Close))] (POSIXTime {getPOSIXTime = 1677628800}) Close

This shows that the transaction generates no warnings or payments, but
updates the state to show the balance in the account ``"bob"``, and
updates the contract, ready to receive a choice from Alice.

In the next state the contract is waiting for input, and if Alice
agrees that "Everything is alright", then a payment to Bob is generated.
This is verified through this interaction in GHCI:

.. code:: haskell

   > let (TransactionOutput txWarn2 txPay2 state2 con2) = computeTransaction (TransactionInput (toPOSIX "2023-02-01 00:00:00.000000 UTC", toPOSIX "2023-02-28 23:59:59.000000 UTC") [NormalInput (IChoice (ChoiceId "Everything is alright" "alice") 0)]) state1 con1
   > txPay2
   [Payment "bob" (Party "bob") (Value (Map [(,Map [("",450)])]))]
   > con2
   Close
   > state2
   State {accounts = Map {unMap = []}, choices = Map {unMap = [(ChoiceId "Everything is alright" "alice",0)]}, boundValues = Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1675209600}}

An alternative way of doing this is to add these definitions to a
working file, e.g. ``Build.hs``, where these definitions will be
preserved. Indeed, it would be very sensible to include some of the
definitions used above in such a file.

Alternative routes through the contract
---------------------------------------

An alternative execution of the contract is given by

-  First step: Alice deposits money as in the earlier example.

-  Second step: Alice reports a problem and Bob disagrees.
   This can be done like this:

.. code:: haskell

   > let (TransactionOutput txWarn2 txPay2 state2 con2) = computeTransaction (TransactionInput (toPOSIX "2023-02-01 00:00:00.000000 UTC", toPOSIX "2023-02-28 23:59:59.000000 UTC") [NormalInput (IChoice (ChoiceId "Report problem" "alice") 1), NormalInput (IChoice (ChoiceId "Dispute problem" "bob") 0)]) state1 con1
   > con2
   When [Case (Choice (ChoiceId "Dismiss claim" "carol") [Bound 0 0]) (Pay "alice" (Account "bob") (Token "" "") (Constant 450) Close),Case (Choice (ChoiceId "Confirm claim" "carol") [Bound 1 1]) Close] (POSIXTime {getPOSIXTime = 1682899200}) Close
   > state2
   State {accounts = Map {unMap = [(("alice",Token "" ""),450)]}, choices = Map {unMap = [(ChoiceId "Report problem" "alice",1),(ChoiceId "Dispute problem" "bob",0)]}, boundValues = Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1675209600}}

This shows that we're now in a contract where the choice is up to Carol,
and that there is still the 450 Lovelace in the ``"alice"`` account.

Note that we have two inputs in the same transaction, Marlowe
supports this as long as the transaction is signed by all relevant
parties, and the time interval is before the timeout of the earliest
``When``.

-  Third step: Carol makes a choice. If she chooses "Dismiss claim",
   payment to Bob is made. If she chooses "Confirm claim", Alice is refunded.
   Let's do that now:

.. code:: haskell

   > let (TransactionOutput txWarn3 txPay3 state3 con3) = computeTransaction (TransactionInput (toPOSIX "2023-04-01 00:00:00.000000 UTC", toPOSIX "2023-04-30 23:59:59.000000 UTC") [NormalInput (IChoice (ChoiceId "Confirm claim" "carol") 1)]) state2 con2
   > txPay3
   [Payment "alice" (Party "alice") (Value (Map [(,Map [("",450)])]))]
   > con3
   Close
   > state3
   State {accounts = Map {unMap = []}, choices = Map {unMap = [(ChoiceId "Report problem" "alice",1),(ChoiceId "Dispute problem" "bob",0),(ChoiceId "Confirm claim" "carol",1)]}, boundValues = Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1680307200}}

So now the contract is ready to ``Close``, and so to refund any
remaining money, but it is clear from ``state3`` that there are no
accounts containing non-zero balances, and so the contract is
terminated.

Why is single stepping useful? It is the equivalent of debugging, and we
are able to see the internal state of the contract at each stage, the
contract continuation, i.e. what remains to be executed, and the actions
produced at each step.

   **Exercise**

   Explore some other ways of engaging with the contract - What happens
   when Bob confirms there is a problem? - What happens if Bob and Alice disagree,
   but Carol sides with Bob?
