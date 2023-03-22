{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Builder.Client
  where

import Data.Vec (Vec(..), (%++))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Builder.Types
import Network.TypedProtocol

marloweBuilderClientPeer
  :: MonadFail m
  => Contract
  -> (Contract -> m ())
  -> Peer MarloweBuilder 'AsClient ('StBuilder '[ 'S 'Z ]) m ()
marloweBuilderClientPeer rootContract = peer' $ StateCons (Cons rootContract Nil) StateNil

peer'
  :: MonadFail m
  => PeerState ns
  -> (Contract -> m ())
  -> Peer MarloweBuilder 'AsClient ('StBuilder ns) m ()
peer' state handleContract = case state of
  StateNil -> Done TokBuilderNobody ()
  StateCons Nil prevState ->
    Await (ServerAgency TokBuilderServer) \(MsgBuilt contract) -> Effect do
      handleContract contract
      pure case prevState of
        StateNil -> peer' prevState handleContract
        StateCons Nil _ -> peer' prevState handleContract
        -- Important - do not force the first param to Cons, it will throw an
        -- exception.
        StateCons (Cons _ contracts) prevState' -> peer' (StateCons contracts prevState') handleContract
  StateCons (Cons contract contracts) prevState -> case convertContract contract of
    Nothing -> Effect $ fail "Merkleized contract detected"
    Just (Converted contract' contracts') ->
      let
        nextState = StateCons contracts'
          -- Set this to error instead of contract so contract can be garbage
          -- collected (we never need to see it again)
          $ StateCons (Cons (error "already converted this contract") contracts) prevState
      in
        Yield (ClientAgency TokBuilderClient) (MsgPush contract') $ peer' nextState handleContract

data PeerState (ns :: [N]) where
  StateNil :: PeerState '[]
  StateCons :: Vec n Contract -> PeerState ns -> PeerState (n ': ns)

data Converted = forall n. Converted (ContractTemplate n) (Vec n Contract)

convertContract :: Contract -> Maybe Converted
convertContract = \case
  Close -> pure $ Converted CloseT Nil
  Pay payor payee token value contract -> do
    Converted contract' contracts <- convertContract contract
    pure $ Converted (PayT payor payee token value contract') contracts
  If obs tru fal -> do
    Converted tru' contracts <- convertContract tru
    Converted fal' contracts' <- convertContract fal
    pure $ Converted (IfT obs tru' fal') $ contracts %++ contracts'
  When cases timeout fallback -> do
    CasesConverted actions contracts <- convertCases cases
    Converted fallback' contracts' <- convertContract fallback
    pure $ Converted (WhenT actions timeout fallback') $ contracts %++ contracts'
  Let valueId value contract -> do
    Converted contract' contracts <- convertContract contract
    pure $ Converted (LetT valueId value contract') contracts
  Assert obs contract -> do
    Converted contract' contracts <- convertContract contract
    pure $ Converted (AssertT obs contract') contracts

convertCases :: [Case Contract] -> Maybe CasesConverted
convertCases [] = pure $ CasesConverted Nil Nil
convertCases (Case action contract : cases) = do
  CasesConverted actions contracts <- convertCases cases
  pure $ CasesConverted (Cons action actions) (Cons contract contracts)
convertCases (MerkleizedCase{} : _) = Nothing

data CasesConverted = forall n. CasesConverted (Vec n Action) (Vec n Contract)
