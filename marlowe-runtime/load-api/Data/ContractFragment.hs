{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.ContractFragment
  where

import Data.Binary (Binary(..), Get, Put, getWord8, putWord8)
import Data.Nat
import Data.Vec (SomeVec(..), Vec(..), getVec, putVec, (%++))
import qualified Data.Vec as Vec
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), fromDatum, toDatum)
import Plutus.V1.Ledger.Api (toBuiltin)
import Plutus.V2.Ledger.Api (FromData, POSIXTime(..))

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

data SomeContractFragment = forall n. SomeContractFragment (ContractFragment n)


putContractFragment :: ContractFragment n -> Put
putContractFragment = \case
  CloseF -> putWord8 0x00
  PayF payor payee token value fragment -> do
    putWord8 0x01
    put $ toDatum payor
    put $ toDatum payee
    put $ toDatum token
    put $ toDatum value
    putContractFragment fragment
  IfF obs tru fal -> do
    putWord8 0x02
    put $ toDatum obs
    putContractFragment tru
    putContractFragment fal
  WhenF cases timeout fallback -> do
    putWord8 0x03
    putVec $ toDatum <$> cases
    put $ getPOSIXTime timeout
    putContractFragment fallback
  LetF valueId value fragment -> do
    putWord8 0x04
    put $ toDatum valueId
    put $ toDatum value
    putContractFragment fragment
  AssertF obs fragment -> do
    putWord8 0x05
    put $ toDatum obs
    putContractFragment fragment

getDatum :: FromData a => Get a
getDatum = get >>= \datum -> case fromDatum datum of
  Nothing -> fail "Invalid datum"
  Just a -> pure a

getContractFragment :: Get SomeContractFragment
getContractFragment = do
  tag <- getWord8
  case tag of
    0x00 -> pure $ SomeContractFragment CloseF
    0x01 ->  do
      payor <- getDatum
      payee <- getDatum
      token <- getDatum
      value <- getDatum
      SomeContractFragment fragment <- getContractFragment
      pure $ SomeContractFragment $ PayF payor payee token value fragment
    0x02 -> do
      obs <- getDatum
      SomeContractFragment tru <- getContractFragment
      SomeContractFragment fal <- getContractFragment
      pure $ SomeContractFragment $ IfF obs tru fal
    0x03 -> do
      SomeVec cases <- getVec >>= \(SomeVec dats) -> case traverse fromDatum dats of
        Nothing -> fail "Invalid actions"
        Just cases -> pure $ SomeVec cases
      timeout <- POSIXTime <$> get
      SomeContractFragment fallback <- getContractFragment
      pure $ SomeContractFragment $ WhenF cases timeout fallback
    0x04 -> do
      valueId <- getDatum
      value <- getDatum
      SomeContractFragment fragment <- getContractFragment
      pure $ SomeContractFragment $ LetF valueId value fragment
    0x05 -> do
      obs <- getDatum
      SomeContractFragment fragment <- getContractFragment
      pure $ SomeContractFragment $ AssertF obs fragment
    _ -> fail $ "Invalid tag " <> show tag

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

-- | A contract fragment with the original contracts contained in its first
-- layer of when clauses.
data ConvertedFragment = forall n. ConvertedFragment (ContractFragment n) (Vec n Contract)

-- | Convert a contract into a fragment with a vector of sub-contracts that
-- were extracted. Fails if the contract is merkleized.
toFragment :: Contract -> Maybe ConvertedFragment
toFragment = \case
  Close -> pure $ ConvertedFragment CloseF Nil
  Pay payor payee token value contract -> do
    ConvertedFragment contract' contracts <- toFragment contract
    pure $ ConvertedFragment (PayF payor payee token value contract') contracts
  If obs tru fal -> do
    ConvertedFragment tru' contracts <- toFragment tru
    ConvertedFragment fal' contracts' <- toFragment fal
    pure $ ConvertedFragment (IfF obs tru' fal') $ contracts %++ contracts'
  When cases timeout fallback -> do
    CasesConverted actions contracts <- convertCases cases
    ConvertedFragment fallback' contracts' <- toFragment fallback
    pure $ ConvertedFragment (WhenF actions timeout fallback') $ contracts %++ contracts'
  Let valueId value contract -> do
    ConvertedFragment contract' contracts <- toFragment contract
    pure $ ConvertedFragment (LetF valueId value contract') contracts
  Assert obs contract -> do
    ConvertedFragment contract' contracts <- toFragment contract
    pure $ ConvertedFragment (AssertF obs contract') contracts

data CasesConverted = forall n. CasesConverted (Vec n Action) (Vec n Contract)

convertCases :: [Case Contract] -> Maybe CasesConverted
convertCases [] = pure $ CasesConverted Nil Nil
convertCases (Case action contract : cases) = do
  CasesConverted actions contracts <- convertCases cases
  pure $ CasesConverted (Cons action actions) (Cons contract contracts)
convertCases (MerkleizedCase{} : _) = Nothing

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

-- | Fills the next hole in a partial contract with a complete contract.
-- Accepts a precomputed contract hash (does not check it).
fillNextHole :: DatumHash -> Contract -> PartialContract ('S n) -> PartialContract n
fillNextHole hash contract = \case
  Root -> RootFilled contract
  PayP payor payee token value partialContract ->
    PayP payor payee token value $ fillNextHole hash contract partialContract
  IfPL obs tru fal ->
    let
      tru' = fillNextHole hash contract tru
    in
      case partialHoles tru' of
        Zero -> IfPR obs (convertContract tru') $ toPartial fal
        Succ _ -> IfPL obs tru' fal
  IfPR obs tru fal -> IfPR obs tru $ fillNextHole hash contract fal
  WhenPCases cases (Cons action actions) timeout fallback -> case actions of
    Nil -> WhenPFallback (mkCase action : cases) timeout $ toPartial fallback
    Cons{} ->
      WhenPCases (mkCase action : cases) actions timeout fallback
  WhenPFallback cases timeout fallback ->
    WhenPFallback cases timeout $ fillNextHole hash contract fallback
  LetP valueId value partialContract ->
    LetP valueId value $ fillNextHole hash contract partialContract
  AssertP obs partialContract ->
    AssertP obs $ fillNextHole hash contract partialContract
  where
    mkCase :: Action -> Case Contract
    mkCase action = case contract of
      Close -> Case action Close
      _ -> MerkleizedCase action $ toBuiltin $ unDatumHash hash