module Language.Marlowe.SemanticsDeserialisation (byteStringToContract) where

import           Language.Marlowe.Deserialisation (byteStringToInt, byteStringToList, byteStringToPositiveInt,
                                                   getByteString)
import           Language.Marlowe.SemanticsTypes  (Action (..), Bound (..), Case (..), ChoiceId (..), Contract (..),
                                                   Observation (..), Party (..), Payee (..), Token (..),
                                                   Value (AddValue, AvailableMoney, ChoiceValue, Cond, Constant, DivValue, MulValue, NegValue, Scale, SlotIntervalEnd, SlotIntervalStart, SubValue, UseValue),
                                                   ValueId (..))
import           Ledger                           (PubKeyHash (..), Slot (..))
import           Ledger.Value                     (CurrencySymbol (..), TokenName (..))
import           PlutusTx.Builtins                (BuiltinByteString)
import           PlutusTx.Prelude                 ((%))

byteStringToParty :: BuiltinByteString -> Maybe (Party, BuiltinByteString)
byteStringToParty x = do (y, t1) <- byteStringToPositiveInt x
                         case y of
                           0 -> withByteString t1 (PK . PubKeyHash)
                           1 -> withByteString t1 (Role . TokenName)
                           _ -> Nothing
  where withByteString bs f = do (z, t) <- getByteString bs
                                 return (f z, t)

byteStringToChoiceId :: BuiltinByteString -> Maybe (ChoiceId, BuiltinByteString)
byteStringToChoiceId x = do (cn, t1) <- getByteString x
                            (co, t2) <- byteStringToParty t1
                            return (ChoiceId cn co, t2)

byteStringToValueId :: BuiltinByteString -> Maybe (ValueId, BuiltinByteString)
byteStringToValueId x = do (n, t) <- getByteString x
                           return (ValueId n, t)

byteStringToToken :: BuiltinByteString -> Maybe (Token, BuiltinByteString)
byteStringToToken x = do (cs, t1) <- getByteString x
                         (tn, t2) <- getByteString t1
                         return (Token (CurrencySymbol cs) (TokenName tn), t2)

byteStringToObservation :: BuiltinByteString -> Maybe (Observation, BuiltinByteString)
byteStringToObservation x = do (y, t1) <- byteStringToPositiveInt x
                               case y of
                                 0  -> withOneObs t1 NotObs
                                 1  -> withTwoObs t1 AndObs
                                 2  -> withTwoObs t1 OrObs
                                 3  -> withChoiceId t1 ChoseSomething
                                 4  -> Just (TrueObs, t1)
                                 5  -> Just (FalseObs, t1)
                                 6  -> withTwoValues t1 ValueGE
                                 7  -> withTwoValues t1 ValueGT
                                 8  -> withTwoValues t1 ValueLT
                                 9  -> withTwoValues t1 ValueLE
                                 10 -> withTwoValues t1 ValueEQ
                                 _  -> Nothing
  where withOneObs = withOne byteStringToObservation
        withTwoObs = withTwo byteStringToObservation
        withChoiceId = withOne byteStringToChoiceId
        withTwoValues = withTwo byteStringToValue
        withOne bsToOne bs f = do (z, t) <- bsToOne bs
                                  return (f z, t)
        withTwo bsToOne bs f = do (y, t1) <- bsToOne bs
                                  (z, t2) <- bsToOne t1
                                  return (f y z, t2)

byteStringToValue :: BuiltinByteString -> Maybe (Value Observation, BuiltinByteString)
byteStringToValue x = do (y, t1) <- byteStringToPositiveInt x
                         case y of
                           0 -> withAccTok t1 AvailableMoney
                           1 -> withInteger t1 Constant
                           2 -> withOneVal t1 NegValue
                           3 -> withTwoVal t1 AddValue
                           4 -> withTwoVal t1 SubValue
                           5 -> withTwoVal t1 MulValue
                           6 -> withTwoVal t1 DivValue
                           7 -> do (n, t2) <- byteStringToInt t1
                                   (d, t3) <- byteStringToInt t2
                                   (v, t4) <- byteStringToValue t3
                                   return (Scale (n % d) v, t4)
                           8 -> withChoiceId t1 ChoiceValue
                           9 -> Just (SlotIntervalStart, t1)
                           10 -> Just (SlotIntervalEnd, t1)
                           11 -> withValueId t1 UseValue
                           12 -> do (cond, t2) <- byteStringToObservation t1
                                    (thn, t3) <- byteStringToValue t2
                                    (els, t4) <- byteStringToValue t3
                                    return (Cond cond thn els, t4)
                           _ -> Nothing
  where withAccTok = withBoth byteStringToParty byteStringToToken
        withInteger = withOne byteStringToInt
        withOneVal = withOne byteStringToValue
        withTwoVal = withTwo byteStringToValue
        withChoiceId = withOne byteStringToChoiceId
        withValueId = withOne byteStringToValueId
        withOne bsToOne bs f = do (z, t) <- bsToOne bs
                                  return (f z, t)
        withTwo bsToOne = withBoth bsToOne bsToOne
        withBoth bsToFst bsToSnd bs f = do (y, t1) <- bsToFst bs
                                           (z, t2) <- bsToSnd t1
                                           return (f y z, t2)

byteStringToPayee :: BuiltinByteString -> Maybe (Payee, BuiltinByteString)
byteStringToPayee x = do (y, t1) <- byteStringToPositiveInt x
                         case y of
                           0 -> withParty t1 Account
                           1 -> withParty t1 Party
                           _ -> Nothing
  where withParty = withOne byteStringToParty
        withOne bsToOne bs f = do (z, t) <- bsToOne bs
                                  return (f z, t)

byteStringToBound :: BuiltinByteString -> Maybe (Bound, BuiltinByteString)
byteStringToBound x = do (l, bs1) <- byteStringToInt x
                         (u, bs2) <- byteStringToInt bs1
                         return (Bound l u, bs2)

byteStringToAction :: BuiltinByteString -> Maybe (Action, BuiltinByteString)
byteStringToAction x = do (y, t1) <- byteStringToPositiveInt x
                          case y of
                            0 -> do (accId, t2) <- byteStringToParty t1
                                    (party, t3) <- byteStringToParty t2
                                    (token, t4) <- byteStringToToken t3
                                    (val, t5) <- byteStringToValue t4
                                    return (Deposit accId party token val, t5)
                            1 -> do (choId, t2) <- byteStringToChoiceId t1
                                    (bounds, t3) <- byteStringToList byteStringToBound t2
                                    return (Choice choId bounds, t3)
                            2 -> do (obs, t2) <- byteStringToObservation t1
                                    return (Notify obs, t2)
                            _ -> Nothing

byteStringToCase :: BuiltinByteString -> Maybe (Case Contract, BuiltinByteString)
byteStringToCase x = do (y, t1) <- byteStringToPositiveInt x
                        case y of
                          0 -> do (action, t2) <- byteStringToAction t1
                                  (cont, t3) <- byteStringToContract t2
                                  return (Case action cont, t3)
                          -- 1 -> do (action, t2) <- byteStringToAction t1
                          --         (bs, t3) <- getByteString t2
                          --         return (MerkleizedCase action bs, t3)
                          _ -> Nothing

byteStringToContract :: BuiltinByteString -> Maybe (Contract, BuiltinByteString)
byteStringToContract x = do (y, t1) <- byteStringToPositiveInt x
                            case y of
                              0 -> Just (Close, t1)
                              1 -> do (accId, t2) <- byteStringToParty t1
                                      (payee, t3) <- byteStringToPayee t2
                                      (token, t4) <- byteStringToToken t3
                                      (val, t5) <- byteStringToValue t4
                                      (cont, t6) <- byteStringToContract t5
                                      return (Pay accId payee token val cont, t6)
                              2 -> do (obs, t2) <- byteStringToObservation t1
                                      (cont1, t3) <- byteStringToContract t2
                                      (cont2, t4) <- byteStringToContract t3
                                      return (If obs cont1 cont2, t4)
                              3 -> do (caseList, t2) <- byteStringToList byteStringToCase t1
                                      (timeout, t3) <- byteStringToInt t2
                                      (cont, t4) <- byteStringToContract t3
                                      return (When caseList (Slot timeout) cont, t4)
                              4 -> do (valId, t2) <- byteStringToValueId t1
                                      (val, t3) <- byteStringToValue t2
                                      (cont, t4) <- byteStringToContract t3
                                      return (Let valId val cont, t4)
                              5 -> do (obs, t2) <- byteStringToObservation t1
                                      (cont, t3) <- byteStringToContract t2
                                      return (Assert obs cont, t3)
                              _ -> Nothing
