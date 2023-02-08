{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The type of the handshake protocol.
--
-- The job protocol is used to establish a connection between two peers, and
-- gives them a chance to confirm that they speak the same protocol.

module Network.Protocol.Handshake.Types
  where

import Data.Aeson (ToJSON, Value(..), object, (.=))
import GHC.Show (showSpace)
import Network.Protocol.Codec.Spec (ArbitraryMessage(..), MessageEq(..), ShowProtocol(..))
import Network.Protocol.Driver (MessageToJSON(..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Test.QuickCheck (Arbitrary(arbitrary), oneof, shrink)

data Handshake h ps where
  StInit :: ps -> Handshake h ps
  StHandshake :: ps -> Handshake h ps
  StLift :: ps -> Handshake h ps
  StDone :: Handshake h ps

instance Protocol ps => Protocol (Handshake h ps) where
  data Message (Handshake h ps) st st' where
    MsgHandshake :: h -> Message (Handshake h ps)
      ('StInit st)
      ('StHandshake st)
    MsgAccept :: Message (Handshake h ps)
      ('StHandshake st)
      ('StLift st)
    MsgReject :: Message (Handshake h ps)
      ('StHandshake st)
      'StDone
    MsgLift :: Message ps st st' -> Message (Handshake h ps)
      ('StLift st)
      ('StLift st')

  data ClientHasAgency st where
    TokInit :: ClientHasAgency ('StInit st')
    TokLiftClient :: ClientHasAgency st' -> ClientHasAgency ('StLift st')

  data ServerHasAgency st where
    TokHandshake :: ServerHasAgency ('StHandshake st')
    TokLiftServer :: ServerHasAgency st' -> ServerHasAgency ('StLift st')

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone
    TokLiftNobody :: NobodyHasAgency st' -> NobodyHasAgency ('StLift st')

  exclusionLemma_ClientAndServerHaveAgency = \case
    TokInit -> \case
    TokLiftClient c_tok -> \case
      TokLiftServer s_tok -> exclusionLemma_ClientAndServerHaveAgency c_tok s_tok

  exclusionLemma_NobodyAndClientHaveAgency = \case
    TokDone -> \case
    TokLiftNobody n_tok -> \case
      TokLiftClient c_tok -> exclusionLemma_NobodyAndClientHaveAgency n_tok c_tok

  exclusionLemma_NobodyAndServerHaveAgency = \case
    TokDone -> \case
    TokLiftNobody n_tok -> \case
      TokLiftServer s_tok -> exclusionLemma_NobodyAndServerHaveAgency n_tok s_tok

instance (ArbitraryMessage ps, Arbitrary h) => ArbitraryMessage (Handshake h ps) where
  arbitraryMessage = oneof
    [ AnyMessageAndAgency (ClientAgency TokInit) . MsgHandshake <$> arbitrary
    , pure $ AnyMessageAndAgency (ServerAgency TokHandshake) MsgReject
    , pure $ AnyMessageAndAgency (ServerAgency TokHandshake) MsgAccept
    , do
        AnyMessageAndAgency tok msg <- arbitraryMessage @ps
        pure case tok of
          ClientAgency tok' -> AnyMessageAndAgency (ClientAgency $ TokLiftClient tok') (MsgLift msg)
          ServerAgency tok' -> AnyMessageAndAgency (ServerAgency $ TokLiftServer tok') (MsgLift msg)
    ]
  shrinkMessage = \case
    ClientAgency TokInit -> \case
      MsgHandshake h -> MsgHandshake <$> shrink h
    ClientAgency (TokLiftClient tok) -> \case
      MsgLift msg -> MsgLift <$> shrinkMessage (ClientAgency tok) msg
    ServerAgency TokHandshake -> const []
    ServerAgency (TokLiftServer tok) -> \case
      MsgLift msg -> MsgLift <$> shrinkMessage (ServerAgency tok) msg

instance (MessageEq ps, Eq h) => MessageEq (Handshake h ps) where
  messageEq (AnyMessageAndAgency tok1 msg1) (AnyMessageAndAgency tok2 msg2)= case (tok1, tok2) of
    (ClientAgency TokInit, ClientAgency TokInit) -> case (msg1, msg2) of
      (MsgHandshake h, MsgHandshake h') -> h == h'
    (ClientAgency TokInit, _) -> False
    (ClientAgency (TokLiftClient tok1'), ClientAgency (TokLiftClient tok2')) -> case (msg1, msg2) of
      (MsgLift msg1', MsgLift msg2') ->
        messageEq (AnyMessageAndAgency (ClientAgency tok1') msg1') (AnyMessageAndAgency (ClientAgency tok2') msg2')
    (ClientAgency (TokLiftClient _), _) -> False
    (ServerAgency TokHandshake, ServerAgency TokHandshake) -> case (msg1, msg2) of
      (MsgAccept, MsgAccept) -> True
      (MsgAccept, MsgReject) -> False
      (MsgReject, MsgAccept) -> False
      (MsgReject, MsgReject) -> True
    (ServerAgency TokHandshake, _) -> False
    (ServerAgency (TokLiftServer tok1'), ServerAgency (TokLiftServer tok2')) -> case (msg1, msg2) of
      (MsgLift msg1', MsgLift msg2') ->
        messageEq (AnyMessageAndAgency (ServerAgency tok1') msg1') (AnyMessageAndAgency (ServerAgency tok2') msg2')
    (ServerAgency (TokLiftServer _), _) -> False

instance (ShowProtocol ps, Show h) => ShowProtocol (Handshake h ps) where
  showsPrecMessage p tok = \case
    MsgHandshake h -> showParen (p >= 11)
      ( showString "MsgHandshake"
      . showSpace
      . showsPrec 11 h
      )
    MsgAccept -> showString "MsgAccept"
    MsgReject -> showString "MsgReject"
    MsgLift msg -> showParen (p >= 11)
      ( showString "MsgLift"
      . showSpace
      . case tok of
          ClientAgency (TokLiftClient tok') -> showsPrecMessage 11 (ClientAgency tok') msg
          ServerAgency (TokLiftServer tok') -> showsPrecMessage 11 (ServerAgency tok') msg
      )
  showsPrecServerHasAgency p = \case
    TokHandshake -> showString "TokHandshake"
    TokLiftServer tok -> showParen (p >= 11)
      ( showString "TokLiftServer"
      . showSpace
      . showsPrecServerHasAgency 11 tok
      )
  showsPrecClientHasAgency p = \case
    TokInit -> showString "TokInit"
    TokLiftClient tok -> showParen (p >= 11)
      ( showString "TokLiftClient"
      . showSpace
      . showsPrecClientHasAgency 11 tok
      )

instance (MessageToJSON ps, ToJSON h) => MessageToJSON (Handshake h ps) where
  messageToJSON = \case
    ClientAgency TokInit -> \case
      MsgHandshake h -> object [ "handshake" .= h ]
    ClientAgency (TokLiftClient tok) -> \case
      MsgLift msg -> object [ "lift" .= messageToJSON (ClientAgency tok) msg ]
    ServerAgency TokHandshake -> String . \case
      MsgAccept -> "accept"
      MsgReject -> "reject"
    ServerAgency (TokLiftServer tok) -> \case
      MsgLift msg -> object [ "lift" .= messageToJSON (ServerAgency tok) msg ]
