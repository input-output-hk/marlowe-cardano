{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

{-| = Extended Marlowe: adds templating functionality to Marlowe language
Extended Marlowe is not executable, it is translated to core Marlowe before
execution, deployment, or analysis, through the process of instantiation.
The purpose of Extended Marlowe is to allow Marlowe contracts to be reusable
in different situations without cluttering the code that goes on-chain
(core Marlowe).
-}

module Language.Marlowe.Extended ( module Language.Marlowe.Extended
                                 , module Language.Marlowe.Pretty
                                 , ada, adaSymbol, adaToken
                                 , S.AccountId, S.Bound(..), S.ChoiceId(..)
                                 , S.ChoiceName, S.ChosenNum, S.Party(..)
                                 , S.SlotInterval, S.Token(..), S.ValueId(..)
                                 , ToCore (..)
                                 , (%)
                                 ) where

import Data.Ratio ((%))
import GHC.Generics
import Language.Marlowe.Pretty (Pretty (..), pretty)
import qualified Language.Marlowe.SemanticsTypes as S
import Language.Marlowe.Util (ada)
import qualified Ledger as L (Slot (..))
import Ledger.Ada (adaSymbol, adaToken)
import Text.PrettyPrint.Leijen (parens, text)


data Timeout = SlotParam String
             | Slot Integer
  deriving stock (Show,Generic)

instance Pretty Timeout where
    prettyFragment (Slot n)         = prettyFragment n
    prettyFragment sp@(SlotParam _) = parens $ text $ show sp

instance Num Timeout where
  (+) (Slot a) (Slot b) = Slot (a + b)
  (+) _ _               = error "Tried to add with templates"
  (-) (Slot a) (Slot b) = Slot (a - b)
  (-) _ _               = error "Tried to subtract with templates"
  (*) (Slot a) (Slot b) = Slot (a * b)
  (*) _ _               = error "Tried to multiplate with templates"
  abs (Slot a) = Slot (abs a)
  abs _        = error "Tried to calculate absolute value of template"
  signum (Slot a) = Slot (signum a)
  signum _        = error "Tried to calculate signum of template"
  fromInteger x = Slot x
  negate (Slot x) = Slot (-x)
  negate _        = error "Tried to negate a template"

instance Pretty Rational where
    prettyFragment r = text $ "(" ++ show r ++ ")"

data Value = AvailableMoney S.AccountId S.Token
           | Constant Integer
           | ConstantParam String
           | NegValue Value
           | AddValue Value Value
           | SubValue Value Value
           | MulValue Value Value
           | DivValue Value Value
           | ChoiceValue S.ChoiceId
           | SlotIntervalStart
           | SlotIntervalEnd
           | UseValue S.ValueId
           | Cond Observation Value Value
  deriving stock (Show,Generic)
  deriving anyclass (Pretty)

data Observation = AndObs Observation Observation
                 | OrObs Observation Observation
                 | NotObs Observation
                 | ChoseSomething S.ChoiceId
                 | ValueGE Value Value
                 | ValueGT Value Value
                 | ValueLT Value Value
                 | ValueLE Value Value
                 | ValueEQ Value Value
                 | TrueObs
                 | FalseObs
  deriving stock (Show,Generic)
  deriving anyclass (Pretty)

data Action = Deposit S.AccountId S.Party S.Token Value
            | Choice S.ChoiceId [S.Bound]
            | Notify Observation
  deriving stock (Show,Generic)
  deriving anyclass (Pretty)

data Payee = Account S.AccountId
           | Party S.Party
  deriving stock (Show,Generic)
  deriving anyclass (Pretty)

data Case = Case Action Contract
  deriving stock (Show,Generic)
  deriving anyclass (Pretty)

data Contract = Close
              | Pay S.AccountId Payee S.Token Value Contract
              | If Observation Contract Contract
              | When [Case] Timeout Contract
              | Let S.ValueId Value Contract
              | Assert Observation Contract
  deriving stock (Show,Generic)
  deriving anyclass (Pretty)

class ToCore a b where
  toCore :: a -> Maybe b

instance ToCore Contract S.Contract where
  toCore Close = Just S.Close
  toCore (Pay accId payee tok val cont) = pure (S.Pay accId) <*> toCore payee <*> pure tok <*> toCore val <*> toCore cont
  toCore (If obs cont1 cont2) = S.If <$> toCore obs <*> toCore cont1 <*> toCore cont2
  toCore (When cases tim cont) = S.When <$> traverse toCore cases <*> toCore tim <*> toCore cont
  toCore (Let varId val cont) = pure (S.Let varId) <*> toCore val <*> toCore cont
  toCore (Assert obs cont) = S.Assert <$> toCore obs <*> toCore cont

instance ToCore Value (S.Value S.Observation) where
  toCore (Constant c)               = Just $ S.Constant c
  toCore (ConstantParam _)          = Nothing
  toCore (AvailableMoney accId tok) = pure (S.AvailableMoney accId) <*> pure tok
  toCore (NegValue v)               = S.NegValue <$> toCore v
  toCore (AddValue lhs rhs)         = S.AddValue <$> toCore lhs <*> toCore rhs
  toCore (SubValue lhs rhs)         = S.SubValue <$> toCore lhs <*> toCore rhs
  toCore (MulValue lhs rhs)         = S.MulValue <$> toCore lhs <*> toCore rhs
  toCore (DivValue lhs rhs)         = S.DivValue <$> toCore lhs <*> toCore rhs
  toCore (ChoiceValue choId)        = Just $ S.ChoiceValue choId
  toCore SlotIntervalStart          = Just S.SlotIntervalStart
  toCore SlotIntervalEnd            = Just S.SlotIntervalEnd
  toCore (UseValue vId)             = Just $ S.UseValue vId
  toCore (Cond obs lhs rhs)         = S.Cond <$> toCore obs <*> toCore lhs <*> toCore rhs

instance ToCore Observation S.Observation where
  toCore (AndObs lhs rhs)       = S.AndObs <$> toCore lhs <*> toCore rhs
  toCore (OrObs lhs rhs)        = S.OrObs <$> toCore lhs <*> toCore rhs
  toCore (NotObs v)             = S.NotObs <$> toCore v
  toCore (ChoseSomething choId) = Just $ S.ChoseSomething choId
  toCore (ValueGE lhs rhs)      = S.ValueGE <$> toCore lhs <*> toCore rhs
  toCore (ValueGT lhs rhs)      = S.ValueGT <$> toCore lhs <*> toCore rhs
  toCore (ValueLT lhs rhs)      = S.ValueLT <$> toCore lhs <*> toCore rhs
  toCore (ValueLE lhs rhs)      = S.ValueLE <$> toCore lhs <*> toCore rhs
  toCore (ValueEQ lhs rhs)      = S.ValueEQ <$> toCore lhs <*> toCore rhs
  toCore TrueObs                = Just S.TrueObs
  toCore FalseObs               = Just S.FalseObs

instance ToCore Action S.Action where
  toCore (Deposit accId party tok val) = pure (S.Deposit accId) <*> pure party <*> pure tok <*> toCore val
  toCore (Choice choId bounds)         = Just $ S.Choice choId bounds
  toCore (Notify obs)                  = S.Notify <$> toCore obs

instance ToCore Timeout L.Slot where
  toCore (SlotParam _) = Nothing
  toCore (Slot x)      = Just (L.Slot x)

instance ToCore Payee S.Payee where
  toCore (Account accId)  = Just $ S.Account accId
  toCore (Party roleName) = Just $ S.Party roleName

instance ToCore Case (S.Case S.Contract) where
  toCore (Case act c) = S.Case <$> toCore act <*> toCore c

advanceTillWhenAndThen :: Contract -> (Contract -> Contract) -> Contract
advanceTillWhenAndThen Close f                      = f Close
advanceTillWhenAndThen w@When{} f                   = f w
advanceTillWhenAndThen (Pay accId p tok val cont) f = Pay accId p tok val (f cont)
advanceTillWhenAndThen (If obs cont1 cont2) f       = If obs (f cont1) (f cont2)
advanceTillWhenAndThen (Let vId val cont) f         = Let vId val (f cont)
advanceTillWhenAndThen (Assert obs cont) f          = Assert obs (f cont)

both :: Contract -> Contract -> Contract
both Close b = b
both a Close = a
both a@(When cases1 (Slot timeout1) cont1) b@(When cases2 (Slot timeout2) cont2)
  = When ([Case a1 (both c1 b) | Case a1 c1 <- cases1] ++
          [Case a2 (both a c2) | Case a2 c2 <- cases2])
         (Slot (min timeout1 timeout2))
         (both (if timeout1 > timeout2 then a else cont1)
               (if timeout2 > timeout1 then b else cont2))
both a@When{} b = advanceTillWhenAndThen b (both a)
both a b = advanceTillWhenAndThen a (`both` b)
