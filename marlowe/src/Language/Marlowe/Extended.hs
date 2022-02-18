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
                                 , S.TimeInterval, S.Token(..), S.ValueId(..)
                                 , ToCore (..)
                                 , (%)
                                 ) where

import Data.Ratio ((%))
import GHC.Generics
import Language.Marlowe.Pretty (Pretty (..), pretty)
import qualified Language.Marlowe.SemanticsTypes as S
import Language.Marlowe.Util (ada)
import qualified Ledger as L (POSIXTime (..))
import Ledger.Ada (adaSymbol, adaToken)
import Text.PrettyPrint.Leijen (parens, text)


data Timeout = SlotParam String
             | POSIXTime Integer
  deriving stock (Show,Generic)

instance Pretty Timeout where
    prettyFragment (POSIXTime n)    = prettyFragment n
    prettyFragment sp@(SlotParam _) = parens $ text $ show sp

instance Num Timeout where
  (+) (POSIXTime a) (POSIXTime b) = POSIXTime (a + b)
  (+) _ _                         = error "Tried to add with templates"
  (-) (POSIXTime a) (POSIXTime b) = POSIXTime (a - b)
  (-) _ _                         = error "Tried to subtract with templates"
  (*) (POSIXTime a) (POSIXTime b) = POSIXTime (a * b)
  (*) _ _                         = error "Tried to multiplate with templates"
  abs (POSIXTime a) = POSIXTime (abs a)
  abs _             = error "Tried to calculate absolute value of template"
  signum (POSIXTime a) = POSIXTime (signum a)
  signum _             = error "Tried to calculate signum of template"
  fromInteger x = POSIXTime x
  negate (POSIXTime x) = POSIXTime (-x)
  negate _             = error "Tried to negate a template"

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
  toCore (AvailableMoney accId tok) = Just $ S.AvailableMoney accId tok
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
  toCore (Deposit accId party tok val) = pure (S.Deposit accId party tok) <*> toCore val
  toCore (Choice choId bounds)         = Just $ S.Choice choId bounds
  toCore (Notify obs)                  = S.Notify <$> toCore obs

instance ToCore Timeout L.POSIXTime where
  toCore (SlotParam _) = Nothing
  toCore (POSIXTime x) = Just (L.POSIXTime x)

instance ToCore Payee S.Payee where
  toCore (Account accId)  = Just $ S.Account accId
  toCore (Party roleName) = Just $ S.Party roleName

instance ToCore Case (S.Case S.Contract) where
  toCore (Case act c) = S.Case <$> toCore act <*> toCore c
