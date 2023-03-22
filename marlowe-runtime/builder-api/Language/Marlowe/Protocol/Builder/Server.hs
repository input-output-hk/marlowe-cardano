{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Builder.Server
  where

import Cardano.Api (SerialiseAsRawBytes(serialiseToRawBytes), hashScriptData)
import Data.Nat (type (+), (%+))
import Data.Vec (Vec(..))
import qualified Data.Vec as Vec
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Builder.Types
import Language.Marlowe.Runtime.Cardano.Api (toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (toDatum)
import Network.TypedProtocol
import Plutus.V1.Ledger.Api (toBuiltin)

marloweBuilderServerPeer
  :: Monad m
  => (Contract -> m ())
  -> Peer MarloweBuilder 'AsServer ('StBuilder '[ 'S 'Z ]) m ()
marloweBuilderServerPeer = peer' $ StateCons (Succ Zero) Root StateNil

peer'
  :: Monad m
  => PeerState ns
  -> (Contract -> m ())
  -> Peer MarloweBuilder 'AsServer ('StBuilder ns) m ()
peer' state save = case state of
  StateNil -> Done TokBuilderNobody ()
  StateCons Zero partialContract prevState -> Effect do
    let contract = convertContract partialContract
    save contract
    pure $ Yield (ServerAgency TokBuilderServer) (MsgBuilt contract) case prevState of
      StateNil -> peer' prevState save
      StateCons Zero _ _  -> peer' prevState save
      StateCons (Succ n) prevPartialContract prevState' -> peer'
        (StateCons n (fillPartialContract contract prevPartialContract) prevState')
        save
  StateCons Succ{} _ _ ->
    Await (ClientAgency TokBuilderClient) \(MsgPush template) -> case toPartial template of
      (n, partialContract') -> peer' (StateCons n partialContract' state) save

data PeerState (ns :: [N]) where
  StateNil :: PeerState '[]
  StateCons :: Nat n -> PartialContract n -> PeerState ns -> PeerState (n ': ns)

data PartialContract (n :: N) where
  Root :: PartialContract ('S 'Z)
  RootFilled :: Contract -> PartialContract 'Z
  PayP :: AccountId -> Payee  -> Token -> Value Observation -> PartialContract n -> PartialContract n
  IfPL :: Observation -> PartialContract ('S n) -> ContractTemplate m -> PartialContract('S  (n + m))
  IfPR :: Observation -> Contract -> PartialContract n -> PartialContract n
  WhenPCases :: [Case Contract] -> Vec ('S n) Action -> Timeout -> ContractTemplate m -> PartialContract ('S (n + m))
  WhenPFallback :: [Case Contract] -> Timeout -> PartialContract n -> PartialContract n
  LetP :: ValueId -> Value Observation -> PartialContract n -> PartialContract n
  AssertP :: Observation -> PartialContract n -> PartialContract n

convertContract :: PartialContract 'Z -> Contract
convertContract = \case
  RootFilled contract -> contract
  PayP payor payee token value contract -> Pay payor payee token value $ convertContract contract
  IfPR obs tru fal -> If obs tru $ convertContract fal
  WhenPFallback cases timeout fallback -> When (reverse cases) timeout $ convertContract fallback
  LetP valueId value contract -> Let valueId value $ convertContract contract
  AssertP obs contract -> Assert obs $ convertContract contract

fillPartialContract :: Contract -> PartialContract ('S n) -> PartialContract n
fillPartialContract contract = \case
  Root -> RootFilled contract
  PayP payor payee token value partialContract ->
    PayP payor payee token value $ fillPartialContract contract partialContract
  IfPL obs tru fal ->
    let
      tru' = fillPartialContract contract tru
    in
      case partialSize tru' of
        Zero -> IfPR obs (convertContract tru') $ snd $ toPartial fal
        Succ _ -> IfPL obs tru' fal
  IfPR obs tru fal -> IfPR obs tru $ fillPartialContract contract fal
  WhenPCases cases (Cons action actions) timeout fallback -> case actions of
    Nil -> WhenPFallback (mkCase action contract : cases) timeout $ snd $ toPartial fallback
    Cons{} ->
      WhenPCases (mkCase action contract : cases) actions timeout fallback
  WhenPFallback cases timeout fallback ->
    WhenPFallback cases timeout $ fillPartialContract contract fallback
  LetP valueId value partialContract ->
    LetP valueId value $ fillPartialContract contract partialContract
  AssertP obs partialContract ->
    AssertP obs $ fillPartialContract contract partialContract

mkCase :: Action -> Contract -> Case Contract
mkCase action = \case
  Close -> Case action Close
  contract -> MerkleizedCase action $ toBuiltin $ serialiseToRawBytes $ hashScriptData $ toCardanoScriptData $ toDatum contract

toPartial :: ContractTemplate n -> (Nat n, PartialContract n)
toPartial = \case
  CloseT -> (Zero, RootFilled Close)
  PayT payor payee token value template -> PayP payor payee token value <$> toPartial template
  IfT obs tru fal -> case (toPartial tru, toPartial fal) of
    ((Zero, tru'), (n, fal')) -> (n, IfPR obs (convertContract tru') fal')
    ((Succ n, tru'), (m, _)) -> (Succ (n %+ m), IfPL obs tru' fal)
  WhenT cases timeout fallback -> case (Vec.length cases, toPartial fallback) of
    (Zero, (n, fallback')) -> (n, WhenPFallback [] timeout fallback')
    (Succ n, (m, _)) -> (Succ (n %+ m), WhenPCases [] cases timeout fallback)
  LetT valueId value template -> LetP valueId value <$> toPartial template
  AssertT obs template -> AssertP obs <$> toPartial template

partialSize :: PartialContract n -> Nat n
partialSize = \case
  Root -> Succ Zero
  RootFilled _ -> Zero
  PayP _ _ _ _ template -> partialSize template
  IfPL _ tru fal -> case (partialSize tru, fst $ toPartial fal) of
    (Succ n, m) -> Succ $ n %+ m
  IfPR _ _ fal -> partialSize fal
  WhenPCases _ cases _ fallback -> case (Vec.length cases, toPartial fallback) of
    (Succ n, (m, _)) -> Succ (n %+ m)
  WhenPFallback _ _ fallback -> partialSize fallback
  LetP _ _ template -> partialSize template
  AssertP _ template -> partialSize template
