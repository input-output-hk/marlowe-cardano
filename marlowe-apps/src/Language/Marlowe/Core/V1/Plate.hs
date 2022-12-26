

{-# LANGUAGE MultiParamTypeClasses #-}


module Language.Marlowe.Core.V1.Plate
  ( Extract(..)
  , MarlowePlate(..)
  ) where


import Data.Generics.Multiplate (Multiplate(..), foldFor, preorderFold)
import Language.Marlowe.Core.V1.Semantics.Types

import qualified Data.Functor.Constant as F (Constant(..))
import qualified Data.Set as S


-- | A mutltiplate for a Marlowe contract.
data MarlowePlate f =
  MarlowePlate
  {
    contractPlate :: Contract -> f Contract
  , casePlate :: Case Contract -> f (Case Contract)
  , actionPlate :: Action -> f Action
  , valuePlate :: Value Observation -> f (Value Observation)
  , observationPlate :: Observation -> f Observation
  }

instance Multiplate MarlowePlate where
  multiplate child =
    let
      buildContract Close = pure Close
      buildContract (Pay a p t v c) = Pay a p t <$> valuePlate child v <*> contractPlate child c
      buildContract (If o c c') = If <$> observationPlate child o <*> contractPlate child c <*> contractPlate child c'
      buildContract (When cs t c) = When <$> sequenceA (casePlate child <$> cs) <*> pure t <*> contractPlate child c
      buildContract (Let i v c) = Let i <$> valuePlate child v <*> contractPlate child c
      buildContract (Assert o c) = Assert <$> observationPlate child o <*> contractPlate child c
      buildCase (Case a c) = Case <$> actionPlate child a <*> contractPlate child c
      buildCase (MerkleizedCase a h) = MerkleizedCase <$> actionPlate child a <*> pure h
      buildAction (Deposit a p t v) = Deposit a p t <$> valuePlate child v
      buildAction (Notify o) = Notify <$> observationPlate child o
      buildAction x = pure x
      buildValue (NegValue x) = NegValue <$> valuePlate child x
      buildValue (AddValue x y) = AddValue <$> valuePlate child x <*> valuePlate child y
      buildValue (SubValue x y) = SubValue <$> valuePlate child x <*> valuePlate child y
      buildValue (MulValue x y) = MulValue <$> valuePlate child x <*> valuePlate child y
      buildValue (DivValue x y) = DivValue <$> valuePlate child x <*> valuePlate child y
      buildValue (Cond o x y) = Cond <$> observationPlate child o <*> valuePlate child x <*> valuePlate child y
      buildValue x = pure x
      buildObservation (AndObs x y) = AndObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (OrObs x y) = OrObs <$> observationPlate child x <*> observationPlate child y
      buildObservation (NotObs x) = NotObs <$> observationPlate child x
      buildObservation (ValueGE x y) = ValueGE <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueGT x y) = ValueGT <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueLT x y) = ValueLT <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueLE x y) = ValueLE <$> valuePlate child x <*> valuePlate child y
      buildObservation (ValueEQ x y) = ValueEQ <$> valuePlate child x <*> valuePlate child y
      buildObservation x = pure x
    in
      MarlowePlate
        buildContract
        buildCase
        buildAction
        buildValue
        buildObservation
  mkPlate build =
    MarlowePlate
      (build contractPlate)
      (build casePlate)
      (build actionPlate)
      (build valuePlate)
      (build observationPlate)


-- | Extract something using the Marlowe multiplate.
class Extract a where
  -- | Shallow extraction.
  extractor :: MarlowePlate (F.Constant (S.Set a))
  -- | Deep extraction.
  extractAll :: Ord a => Contract -> S.Set a
  extractAll = foldFor contractPlate $ preorderFold extractor

{-
instance Extract Token where
  extractor =
    let
      single = F.Constant . S.singleton
      contractPlate' (Pay _ _ t _ _) = single t
      contractPlate' x = pure x
      actionPlate' (Deposit _ _ t _) = single t
      actionPlate' x = pure x
      valuePlate' (AvailableMoney _ t) = single t
      valuePlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      }

instance Extract P.TokenName where
  extractor =
    let
      role (Role r) = S.singleton r
      role _ = S.empty
      contractPlate' (Pay a (Account p) _ _ _) = F.Constant $ role a <> role p
      contractPlate' (Pay a (Party p) _ _ _) = F.Constant $ role a <> role p
      contractPlate' x = pure x
      actionPlate' (Deposit a p _ _) = F.Constant $ role a <> role p
      actionPlate' (Choice (ChoiceId _ p) _) = F.Constant $ role p
      actionPlate' x = pure x
      valuePlate' (AvailableMoney a _) = F.Constant $ role a
      valuePlate' (ChoiceValue (ChoiceId _ p)) = F.Constant $ role p
      valuePlate' x = pure x
      observationPlate' (ChoseSomething (ChoiceId _ p)) = F.Constant $ role p
      observationPlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      , observationPlate = observationPlate'
      }



-- | Extract something from a Marlowe contract.
extractFromContract :: Extract a
                    => Ord a
                    => ContractInstance lang era  -- ^ The bundle of contract information.
                    -> S.Set a                    -- ^ The extract.
extractFromContract ContractInstance{..} =
  M.foldr (S.union . extractAll) (extractAll ciContract) ciContinuations
-}
