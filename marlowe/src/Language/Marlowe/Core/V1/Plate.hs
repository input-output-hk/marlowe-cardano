

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Core.V1.Plate
  ( Extract(..)
  , MarlowePlate(..)
  , StateExtract(..)
  , extractAddresses
  , extractAllWithContinuations
  , extractNetworkAddresses
  , extractNetworks
  , extractRoleNames
  , extractTokens
  ) where


import Data.Generics.Multiplate (Multiplate(..), foldFor, mChildren, preorderFold, purePlate)
import Data.Maybe (mapMaybe)
import Language.Marlowe.Core.V1.Merkle (Continuations)
import Language.Marlowe.Core.V1.Semantics.Types

import Language.Marlowe.Core.V1.Semantics.Types.Address (Network)

import qualified Data.Functor.Constant as F (Constant(..))
import qualified Data.Map.Strict as M (foldr)
import qualified Data.Set as S (Set, empty, fromList, map, singleton, toList, union)
import qualified Plutus.V2.Ledger.Api as Ledger (Address)
import qualified PlutusTx.AssocMap as AM (keys)
import PlutusTx.Prelude (BuiltinByteString)

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
  -- | Extraction.
  extractor :: MarlowePlate (F.Constant (S.Set a))
  -- | Shallow extraction.
  extract :: Ord a => Contract -> S.Set a
  extract = foldFor contractPlate $ mChildren extractor
  -- | Deep extraction.
  extractAll :: Ord a => Contract -> S.Set a
  extractAll = foldFor contractPlate $ preorderFold extractor

instance Extract Action where
  extractor =
    purePlate
    {
      casePlate = F.Constant . S.singleton . getAction
    }

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

instance Extract TokenName where
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

instance Extract ChoiceId where
  extractor =
    let
      actionPlate' (Choice c _) = F.Constant $ S.singleton c
      actionPlate' x = pure x
      valuePlate' (ChoiceValue c) = F.Constant $ S.singleton c
      valuePlate' x = pure x
      observationPlate' (ChoseSomething c) = F.Constant $ S.singleton c
      observationPlate' x = pure x
    in
      purePlate
      {
        actionPlate = actionPlate'
      , valuePlate = valuePlate'
      , observationPlate = observationPlate'
      }

instance Extract ValueId where
  extractor =
    let
      contractPlate' (Let v _ _) = F.Constant $ S.singleton v
      contractPlate' x = pure x
      valuePlate' (UseValue v) = F.Constant $ S.singleton v
      valuePlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , valuePlate = valuePlate'
      }

instance Extract (AccountId, Token) where
  extractor =
    let
      contractPlate' (Pay a (Party _) t _ _) = F.Constant $ S.singleton (a, t)
      contractPlate' (Pay a (Account a') t _ _) = F.Constant $ S.singleton (a, t) <> S.singleton (a', t)
      contractPlate' x = pure x
      actionPlate' (Deposit a _ t _) = F.Constant $ S.singleton (a, t)
      actionPlate' x = pure x
      valuePlate' (AvailableMoney a t) = F.Constant $ S.singleton (a, t)
      valuePlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      , actionPlate = actionPlate'
      , valuePlate = valuePlate'
      }

instance Extract (Case Contract) where
  extractor =
    let
      contractPlate' (When cs _ _) = F.Constant $ S.fromList cs
      contractPlate' x = pure x
    in
      purePlate
      {
        contractPlate = contractPlate'
      }

instance Extract BuiltinByteString where
  extractor =
    let
      casePlate' (MerkleizedCase _ hash) = F.Constant $ S.singleton hash
      casePlate' x = pure x
    in
      purePlate
      {
        casePlate = casePlate'
      }

instance Extract Party where
  extractor =
    let
      contractPlate' (Pay p (Party p') _ _ _) = F.Constant $ S.fromList [p, p']
      contractPlate' x = pure x
      actionPlate' (Deposit p p' _ _) = F.Constant $ S.fromList [p, p']
      actionPlate' (Choice (ChoiceId _ p) _) = F.Constant $ S.singleton p
      actionPlate' x = pure x
      valuePlate' (AvailableMoney p _) = F.Constant $ S.singleton p
      valuePlate' (ChoiceValue (ChoiceId _ p)) = F.Constant $ S.singleton p
      valuePlate' x = pure x
      observationPlate' (ChoseSomething (ChoiceId _ p)) = F.Constant $ S.singleton p
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
extractAllWithContinuations
  :: Extract a
  => Ord a
  => Contract       -- ^ The contract.
  -> Continuations  -- ^ The continuations of the contract.
  -> S.Set a        -- ^ The extract.
extractAllWithContinuations = M.foldr (S.union . extractAll) . extractAll


-- | Class for extracting information from a contract's state.
class StateExtract a where
  -- | Extract information from a contract's state.
  extractFromState
    :: State  -- ^ The state.
    -> S.Set a  -- ^ The information.

instance StateExtract Party where
  extractFromState State{..} =
    let
      fromChoice (ChoiceId _ p) = p
    in
      S.fromList
        $ (fst <$> AM.keys accounts)
        <> (fromChoice <$> AM.keys choices)

instance StateExtract Token where
  extractFromState State{..} =
    S.fromList
      $ snd <$> AM.keys accounts


-- | List all of the parties in a contract and its state.
extractParties
  :: Maybe State  -- ^ The contract's initial state.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The contract's merkleized continuations.
  -> S.Set Party  -- ^ The parties.
extractParties state contract continuations =
  maybe mempty extractFromState state
    <> extractAllWithContinuations contract continuations


-- | List all of the parties in a contract and its state.
extractRoleNames
  :: Maybe State  -- ^ The contract's initial state.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The contract's merkleized continuations.
  -> S.Set TokenName  -- ^ The tokens.
extractRoleNames =
  let
    role (Role name) = Just name
    role _ = Nothing
  in
    (((S.fromList . mapMaybe role . S.toList) .) .) . extractParties


-- | List all of the network addresses in a contract and its state.
extractNetworkAddresses
  :: Maybe State  -- ^ The contract's initial state.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The contract's merkleized continuations.
  -> S.Set (Network, Ledger.Address)  -- ^ The network addresses.
extractNetworkAddresses =
  let
    address (Address n a) = Just (n, a)
    address _ = Nothing
  in
    (((S.fromList . mapMaybe address . S.toList) .) .) . extractParties


-- | List all of the networks in a contract and its state.
extractNetworks
  :: Maybe State  -- ^ The contract's initial state.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The contract's merkleized continuations.
  -> S.Set Network  -- ^ The networks.
extractNetworks = ((S.map fst .) .) . extractNetworkAddresses


-- | List all of the addresses in a contract and its state.
extractAddresses
  :: Maybe State  -- ^ The contract's initial state.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The contract's merkleized continuations.
  -> S.Set Ledger.Address  -- ^ The addresses.
extractAddresses = ((S.map snd .) .) . extractNetworkAddresses


-- | List all of the tokens in a contract and its state.
extractTokens
  :: Maybe State  -- ^ The contract's initial state.
  -> Contract  -- ^ The contract.
  -> Continuations  -- ^ The contract's merkleized continuations.
  -> S.Set Token  -- ^ The tokens.
extractTokens state contract continuations =
  maybe mempty extractFromState state
    <> extractAllWithContinuations contract continuations
