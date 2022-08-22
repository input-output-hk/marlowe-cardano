
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}


module Spec.Marlowe.Semantics.Golden (
  tests
, pangram
) where


import Data.List (intercalate)
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.FindInputs (getAllInputs)
import Plutus.V1.Ledger.Api (POSIXTime (..))
import Spec.Marlowe.Semantics.Golden.Pangram
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


tests :: TestTree
tests =
  testGroup "Golden"
    [
      testGroup "Pangram"
        [
          testCase "Valid inputs"   testPangramValid
        , testCase "Invalid inputs" testPangramInvalid
--      , testCase "Generate valid inputs" printPangram  -- Uncomment this to generate golden test cases.
        ]
    ]


deriving instance Eq Payment

deriving instance Eq TransactionOutput


testPangramValid :: IO ()
testPangramValid =
  sequence_
    [
      assertBool (show (t, i))  $ playTrace t pangram i == o
    |
      (t, i, o) <- pangramValids
    ]


testPangramInvalid :: IO ()
testPangramInvalid =
  sequence_
    [
      assertBool (show (t, i))  $ playTrace t pangram i /= o
    |
      (t, i, o) <- pangramInvalids
    ]


printPangram :: IO ()
printPangram =
  do
    Right is <- getAllInputs pangram
    sequence_
      [
        case playTrace t pangram i of
          Error{} -> pure ()
          o       -> putStrLn $ intercalate "\t" [show t, show i, show o]
      |
        (t, i) <- is
      ]


pangram :: Contract
pangram =
  let
    start = 1
    delta = 5
    timeout i = POSIXTime $ start + delta * i
    party1 = PK "03e718291f87b2b004caac168d55e81da688e4501ce560ae613fa7e7"
    party2 = PK "1b4a1ddd07976d3eee561fdce46d70f2f4c03985195906c08f547249"
    party3 = Role "Cy"
    party4 = Role "Noe"
    party5 = Role "Sten"
    token1 = Token "" ""
    token2 = Token "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8" "PIN"
    token3 = Token "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741" "TALE"
    choice1 = ChoiceId "be" party1
    choice2 = ChoiceId "be" party3
    choice3 = ChoiceId "dry" party3
    choice4 = ChoiceId "grab" party4
    value1  = ChoiceValue choice1
    value2  = ChoiceValue choice2
    value3  = ChoiceValue choice3
    value4  = ChoiceValue choice4
    value5  = UseValue (ValueId "x"  )
    value6  = UseValue (ValueId "id" )
    value7  = UseValue (ValueId "lab")
    value8  = AvailableMoney party1 token1
    value9  = AvailableMoney party2 token2
    value10 = AvailableMoney party3 token3
    value11 = AvailableMoney party4 token1
    value12 = AvailableMoney party5 token1
    value13 = Cond observation3 (value1 `AddValue` value5) (value8 `SubValue` TimeIntervalStart)
    value14 = value7 `MulValue` (value10 `DivValue` Constant 2)
    value15 = NegValue value9
    value16 = Cond observation5 (TimeIntervalEnd `DivValue` value7) (value6 `MulValue` value5)
    value17 = Cond observation4 value3 value7
    observation1 = ValueGE value1 (Constant 4) `OrObs` FalseObs
    observation2 = ValueGT value2 (Constant 6) `AndObs` NotObs (ChoseSomething choice2)
    observation3 = ValueLE value3 (Constant 9) `OrObs` ChoseSomething choice3
    observation4 = ValueLT value4 (Constant 1) `AndObs` observation2
    observation5 = ValueEQ value1 value2 `OrObs` NotObs observation1
    deposit1 = Case (Deposit party1 party2 token1 value5  ) . Let "deposit1" value5
    deposit2 = Case (Deposit party1 party1 token1 value6  ) . Let "deposit2" value6
    deposit3 = Case (Deposit party3 party3 token2 value7  ) . Let "deposit3" value7
    deposit4 = Case (Deposit party4 party3 token1 value1  ) . Let "deposit4" value1
    deposit5 = Case (Deposit party3 party5 token1 value2  ) . Let "deposit5" value2
    choose1  = Case (Choice choice1 [Bound 1 4, Bound 3 6]) . Let "choose1"  value1
    choose2  = Case (Choice choice1 [Bound 5 8]           ) . Let "choose2"  value2
    choose3  = Case (Choice choice2 [Bound 1 8]           ) . Let "choose3"  value3
    choose4  = Case (Choice choice3 []                    ) . Let "choose4"  value4
    notify1  = Case (Notify observation1                  ) . Let "notify1"  (Cond observation1 (Constant 0) (Constant 1))
    notify2  = Case (Notify observation2                  ) . Let "notify2"  (Cond observation2 (Constant 0) (Constant 1))
    pay1 = Pay party1 (Party   party3) token2 value13
    pay2 = Pay party2 (Party   party1) token2 value14
    pay3 = Pay party3 (Account party1) token2 value15
    pay4 = Pay party4 (Account party4) token2 value16
    pay5 = Pay party5 (Party   party2) token2 value17
    if1 c = If observation5 (let1 c) (let2 c)
    let1 = Let "x" value11
    let2 = Let "lab" (value10 `AddValue` Constant 10 `AddValue` value12)
    assert1 = Assert (observation3 `OrObs` observation2)
    when i as c = When (fmap ($ c) as) (timeout i) Close  -- FIXME: Ideally, this should timeout to `c` instead of to `Close`, but the combinations are too many.
    both i a1 a2 c =
      When [
        a1 $ When [ a2 c ] (timeout $ i + 1) c
      , a2 $ When [ a1 c ] (timeout $ i + 1) c
      ]
      (timeout i) c
  in
      When [] (timeout 1)
    $ both 2 deposit1 deposit2
    $ both 4 notify1  choose2
    $ when 6 [deposit3, deposit4]
    $ when 7 [choose1, choose2]
    $ let1
    $ when 8 [deposit5]
    $ let2
    $ pay1
    $ when 9 [choose3]
    $ pay2
    $ when 10 [choose1]
    $ assert1
    $ let1
    $ when 11 [choose4]
    $ pay3
    $ if1
    $ pay4
    $ let2
    $ when 12 [notify2]
    $ pay5
      Close
