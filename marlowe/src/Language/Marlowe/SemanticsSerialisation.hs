module Language.Marlowe.SemanticsSerialisation (contractToByteString) where

import           Language.Marlowe.SemanticsTypes (Action (..), Bound (..), Case (..), ChoiceId (..), Contract (..),
                                                  Observation (..), Party (..), Payee (..), Token (..), Value (..),
                                                  ValueId (..))
import           Language.Marlowe.Serialisation  (intToByteString, listToByteString, packByteString,
                                                  positiveIntToByteString)
import           Ledger                          (PubKeyHash (..), Slot (..))
import           Ledger.Value                    (CurrencySymbol (..), TokenName (..))
import           PlutusTx.Builtins               (BuiltinByteString, appendByteString)
import           PlutusTx.Ratio                  (denominator, numerator)

partyToByteString :: Party -> BuiltinByteString
partyToByteString (PK (PubKeyHash x))  = positiveIntToByteString 0 `appendByteString` packByteString x
partyToByteString (Role (TokenName x)) = positiveIntToByteString 1 `appendByteString` packByteString x

choiceIdToByteString :: ChoiceId -> BuiltinByteString
choiceIdToByteString (ChoiceId cn co) =
  packByteString cn `appendByteString` partyToByteString co

valueIdToByteString :: ValueId -> BuiltinByteString
valueIdToByteString (ValueId n) = packByteString n

tokenToByteString :: Token -> BuiltinByteString
tokenToByteString (Token (CurrencySymbol cs) (TokenName tn)) = packByteString cs `appendByteString` packByteString tn

observationToByteString :: Observation -> BuiltinByteString
observationToByteString (NotObs subObs) = positiveIntToByteString 0 `appendByteString` observationToByteString subObs
observationToByteString (AndObs lhs rhs) = positiveIntToByteString 1 `appendByteString` observationToByteString lhs `appendByteString` observationToByteString rhs
observationToByteString (OrObs lhs rhs) = positiveIntToByteString 2 `appendByteString` observationToByteString lhs `appendByteString` observationToByteString rhs
observationToByteString (ChoseSomething choId) = positiveIntToByteString 3 `appendByteString` choiceIdToByteString choId
observationToByteString TrueObs = positiveIntToByteString 4
observationToByteString FalseObs = positiveIntToByteString 5
observationToByteString (ValueGE lhs rhs) = positiveIntToByteString 6 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs
observationToByteString (ValueGT lhs rhs) = positiveIntToByteString 7 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs
observationToByteString (ValueLT lhs rhs) = positiveIntToByteString 8 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs
observationToByteString (ValueLE lhs rhs) = positiveIntToByteString 9 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs
observationToByteString (ValueEQ lhs rhs) = positiveIntToByteString 10 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs

valueToByteString :: Value Observation -> BuiltinByteString
valueToByteString (AvailableMoney accId token) = positiveIntToByteString 0 `appendByteString` partyToByteString accId `appendByteString` tokenToByteString token
valueToByteString (Constant integer) = positiveIntToByteString 1 `appendByteString` intToByteString integer
valueToByteString (NegValue val) = positiveIntToByteString 2 `appendByteString` valueToByteString val
valueToByteString (AddValue lhs rhs) = positiveIntToByteString 3 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs
valueToByteString (SubValue lhs rhs) = positiveIntToByteString 4 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs
valueToByteString (MulValue lhs rhs) = positiveIntToByteString 5 `appendByteString` valueToByteString lhs `appendByteString` valueToByteString rhs
valueToByteString (Scale r rhs) = positiveIntToByteString 6 `appendByteString` intToByteString (numerator r) `appendByteString` intToByteString (denominator r) `appendByteString` valueToByteString rhs
valueToByteString (ChoiceValue choId) = positiveIntToByteString 7 `appendByteString` choiceIdToByteString choId
valueToByteString SlotIntervalStart = positiveIntToByteString 8
valueToByteString SlotIntervalEnd = positiveIntToByteString 9
valueToByteString (UseValue valId) = positiveIntToByteString 10 `appendByteString` valueIdToByteString valId
valueToByteString (Cond cond thn els) = positiveIntToByteString 11 `appendByteString` observationToByteString cond `appendByteString` valueToByteString thn `appendByteString` valueToByteString els
valueToByteString (DivValue _ _) = positiveIntToByteString 12

payeeToByteString :: Payee -> BuiltinByteString
payeeToByteString (Account accId) = positiveIntToByteString 0 `appendByteString` partyToByteString accId
payeeToByteString (Party party)   = positiveIntToByteString 1 `appendByteString` partyToByteString party

boundToByteString :: Bound -> BuiltinByteString
boundToByteString (Bound l u) = intToByteString l `appendByteString` intToByteString u

actionToByteString :: Action -> BuiltinByteString
actionToByteString (Deposit accId party token val) = positiveIntToByteString 0 `appendByteString` partyToByteString accId `appendByteString` partyToByteString party `appendByteString` tokenToByteString token `appendByteString` valueToByteString val
actionToByteString (Choice choId bounds) = positiveIntToByteString 1 `appendByteString` choiceIdToByteString choId `appendByteString` listToByteString boundToByteString bounds
actionToByteString (Notify obs) = positiveIntToByteString 2 `appendByteString` observationToByteString obs

caseToByteString :: Case Contract -> BuiltinByteString
caseToByteString (Case action cont) = positiveIntToByteString 0 `appendByteString` actionToByteString action `appendByteString` contractToByteString cont
--caseToByteString (MerkleizedCase action bs) = positiveIntToByteString 1 `appendByteString` actionToByteString action `appendByteString` packByteString bs

contractToByteString :: Contract -> BuiltinByteString
contractToByteString Close = positiveIntToByteString 0
contractToByteString (Pay accId payee token val cont) = positiveIntToByteString 1 `appendByteString` partyToByteString accId `appendByteString` payeeToByteString payee `appendByteString` tokenToByteString token `appendByteString` valueToByteString val `appendByteString` contractToByteString cont
contractToByteString (If obs cont1 cont2) = positiveIntToByteString 2 `appendByteString` observationToByteString obs `appendByteString` contractToByteString cont1 `appendByteString` contractToByteString cont2
contractToByteString (When caseList (Slot timeout) cont) = positiveIntToByteString 3 `appendByteString` listToByteString caseToByteString caseList `appendByteString` intToByteString timeout `appendByteString` contractToByteString cont
contractToByteString (Let valId val cont) = positiveIntToByteString 4 `appendByteString` valueIdToByteString valId `appendByteString` valueToByteString val `appendByteString` contractToByteString cont
contractToByteString (Assert obs cont) = positiveIntToByteString 5 `appendByteString` observationToByteString obs `appendByteString` contractToByteString cont
