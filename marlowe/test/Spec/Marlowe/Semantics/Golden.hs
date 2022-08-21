
{-# LANGUAGE OverloadedStrings #-}


module Spec.Marlowe.Semantics.Golden (
  pangram
) where


import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types
import Plutus.V1.Ledger.Api (POSIXTime (..))


pangram :: Integer -> Integer -> Contract
pangram start delta =
  let
    timeouts = [POSIXTime $ start + delta * i | i <- [0..]]
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
    observation1 = ValueGE value1 (Constant 4)
    observation2 = ValueGT value2 (Constant 6)
    observation3 = ValueLE value3 (Constant 9)
    observation4 = ValueLT value4 (Constant 1)
    observation5 = ValueEQ value1 value2
    deposit1 = (Deposit party1 party2 token1 value5  , Let "deposit1" value5                                       )
    deposit2 = (Deposit party1 party1 token1 value6  , Let "deposit2" value6                                       )
    deposit3 = (Deposit party3 party3 token2 value7  , Let "deposit3" value7                                       )
    deposit4 = (Deposit party4 party3 token1 value1  , Let "deposit4" value1                                       )
    deposit5 = (Deposit party3 party5 token1 value2  , Let "deposit5" value2                                       )
    choose1  = (Choice choice1 [Bound 1 4, Bound 3 6], Let "choose1"  value1                                       )
    choose2  = (Choice choice1 [Bound 5 8]           , Let "choose2"  value2                                       )
    choose3  = (Choice choice2 [Bound 1 8]           , Let "choose3"  value3                                       )
    choose4  = (Choice choice3 []                    , Let "choose4"  value4                                       )
    notify1  = (Notify observation1                  , Let "notify1"  (Cond observation1 (Constant 0) (Constant 1)))
    notify2  = (Notify observation2                  , Let "notify2"  (Cond observation2 (Constant 0) (Constant 1)))
    contract1  = Pay party2 (Party party3) token2 (Constant 10)
    contract2  = Let (ValueId "x"  ) TimeIntervalStart
    contract3  = Let (ValueId "id" ) TimeIntervalEnd
  in
    Close
