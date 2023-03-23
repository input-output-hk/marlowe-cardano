{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.ContractFragment
  where

import Cardano.Api (SerialiseAsRawBytes(serialiseToRawBytes), hashScriptData)
import Data.Nat
import Data.Vec (Vec(..))
import qualified Data.Vec as Vec
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.Cardano.Api (toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import Plutus.V1.Ledger.Api (toBuiltin)

-- A contract fragment indexed by the number of holes it contains.
data ContractFragment (n :: N) where
  -- | A close fragment has no holes.
  CloseF :: ContractFragment 'Z
  -- | A pay fragment
  PayF :: AccountId -> Payee  -> Token -> Value Observation -> ContractFragment n -> ContractFragment n
  -- | An if fragment has as many holes as its two clauses combined.
  IfF :: Observation -> ContractFragment n -> ContractFragment m -> ContractFragment (n + m)
  -- | A when fragment has 1 hole per case, combined with the holes in the
  -- timeout clause.
  WhenF :: Vec n Action -> Timeout -> ContractFragment m -> ContractFragment (n + m)
  -- | A let fragment
  LetF :: ValueId -> Value Observation -> ContractFragment n -> ContractFragment n
  -- | An assert fragment
  AssertF :: Observation -> ContractFragment n -> ContractFragment n

-- | Count the number of holes in a contract fragment.
fragmentHoles :: ContractFragment n -> Nat n
fragmentHoles = \case
  CloseF -> Zero
  PayF _ _ _ _ fragment -> fragmentHoles fragment
  IfF _ tru fal -> fragmentHoles tru %+ fragmentHoles fal
  WhenF cases _ fallback -> Vec.length cases %+ fragmentHoles fallback
  LetF _ _ fragment -> fragmentHoles fragment
  AssertF _ fragment -> fragmentHoles fragment

-- | A contract fragment can be converted to a partial contract with no holes
-- filled.
toPartial :: ContractFragment n -> PartialContract n
toPartial = \case
  CloseF -> RootFilled Close
  PayF payor payee token value fragment -> PayP payor payee token value $ toPartial fragment
  IfF obs tru fal -> case fragmentHoles tru of
    Zero -> IfPR obs (convertContract $ toPartial tru) $ toPartial fal
    Succ{} -> IfPL obs (toPartial tru) fal
  WhenF cases timeout fallback -> case Vec.length cases of
    Zero -> WhenPFallback [] timeout $ toPartial fallback
    Succ{} -> WhenPCases [] cases timeout fallback
  LetF valueId value fragment -> LetP valueId value $ toPartial fragment
  AssertF obs fragment -> AssertP obs $ toPartial fragment

-- An incremental state of a contract fragment having its holes filled.
data PartialContract (n :: N) where
  -- | A single hole
  Root :: PartialContract ('S 'Z)
  -- | A single filled hole
  RootFilled :: Contract -> PartialContract 'Z
  -- | A pay partial contract
  PayP :: AccountId -> Payee  -> Token -> Value Observation -> PartialContract n -> PartialContract n
  -- | An if contract with at least one hole in its true clause
  IfPL :: Observation -> PartialContract ('S n) -> ContractFragment m -> PartialContract('S  (n + m))
  -- | An if contract with zero holes in its true clause
  IfPR :: Observation -> Contract -> PartialContract n -> PartialContract n
  -- | A when contract with at least one hole in its cases
  WhenPCases :: [Case Contract] -> Vec ('S n) Action -> Timeout -> ContractFragment m -> PartialContract ('S (n + m))
  -- | A when contract with zero holes in its cases
  WhenPFallback :: [Case Contract] -> Timeout -> PartialContract n -> PartialContract n
  -- | A let partial contract
  LetP :: ValueId -> Value Observation -> PartialContract n -> PartialContract n
  -- | An assert partial contract
  AssertP :: Observation -> PartialContract n -> PartialContract n

-- | Count the number of holes in a partial contract.
partialHoles :: PartialContract n -> Nat n
partialHoles = \case
  Root -> Succ Zero
  RootFilled _ -> Zero
  PayP _ _ _ _ fragment -> partialHoles fragment
  IfPL _ tru fal -> case partialHoles tru of
    Succ n -> Succ $ n %+ fragmentHoles fal
  IfPR _ _ fal -> partialHoles fal
  WhenPCases _ cases _ fallback -> case Vec.length cases of
    Succ n -> Succ (n %+ fragmentHoles fallback)
  WhenPFallback _ _ fallback -> partialHoles fallback
  LetP _ _ fragment -> partialHoles fragment
  AssertP _ fragment -> partialHoles fragment

-- | A partial contract with no holes can be converted to a Marlowe contract.
convertContract :: PartialContract 'Z -> Contract
convertContract = \case
  RootFilled contract -> contract
  PayP payor payee token value contract -> Pay payor payee token value $ convertContract contract
  IfPR obs tru fal -> If obs tru $ convertContract fal
  WhenPFallback cases timeout fallback -> When (reverse cases) timeout $ convertContract fallback
  LetP valueId value contract -> Let valueId value $ convertContract contract
  AssertP obs contract -> Assert obs $ convertContract contract

-- | Fills the next hole in a partial contract with a complete contract. Also
-- returns the hash of the contract.
fillNextHole :: Contract -> PartialContract ('S n) -> (DatumHash, PartialContract n)
fillNextHole contract = \case
  Root -> (DatumHash contractHash, RootFilled contract)
  PayP payor payee token value partialContract ->
    PayP payor payee token value <$> fillNextHole contract partialContract
  IfPL obs tru fal ->
    let
      tru' = snd $ fillNextHole contract tru
    in
      ( DatumHash contractHash
      , case partialHoles tru' of
          Zero -> IfPR obs (convertContract tru') $ toPartial fal
          Succ _ -> IfPL obs tru' fal
      )
  IfPR obs tru fal -> IfPR obs tru <$> fillNextHole contract fal
  WhenPCases cases (Cons action actions) timeout fallback ->
    ( DatumHash contractHash
    , case actions of
      Nil -> WhenPFallback (mkCase action : cases) timeout $ toPartial fallback
      Cons{} ->
        WhenPCases (mkCase action : cases) actions timeout fallback
    )
  WhenPFallback cases timeout fallback ->
    WhenPFallback cases timeout <$> fillNextHole contract fallback
  LetP valueId value partialContract ->
    LetP valueId value <$> fillNextHole contract partialContract
  AssertP obs partialContract ->
    AssertP obs <$> fillNextHole contract partialContract
  where
    mkCase :: Action -> Case Contract
    mkCase action = case contract of
      Close -> Case action Close
      _ -> MerkleizedCase action $ toBuiltin contractHash
    contractHash = serialiseToRawBytes $ hashScriptData $ toCardanoScriptData $ toDatum contract
