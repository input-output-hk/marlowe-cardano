{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The type of the handshake protocol.
--
-- The job protocol is used to establish a connection between two peers, and
-- gives them a chance to confirm that they speak the same protocol.

module Network.Protocol.Handshake.Types
  where

import Control.Monad (unless)
import Data.Aeson (Value(..), object, (.=))
import Data.Binary (get, getWord8, put, putWord8)
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Show (showSpace)
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Codec.Spec (ArbitraryMessage(..), MessageEq(..), ShowProtocol(..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Observe.Event.Network.Protocol (MessageToJSON(..))
import Test.QuickCheck (Arbitrary(arbitrary), oneof, shrink)

data Handshake ps where
  StInit :: ps -> Handshake ps
  StHandshake :: ps -> Handshake ps
  StLift :: ps -> Handshake ps
  StDone :: Handshake ps

class HasSignature (ps :: k) where
  signature :: Proxy ps -> Text

instance Protocol ps => Protocol (Handshake ps) where
  data Message (Handshake ps) st st' where
    MsgHandshake :: Text -> Message (Handshake ps)
      ('StInit st)
      ('StHandshake st)
    MsgAccept :: Message (Handshake ps)
      ('StHandshake st)
      ('StLift st)
    MsgReject :: Message (Handshake ps)
      ('StHandshake st)
      'StDone
    MsgLift :: Message ps st st' -> Message (Handshake ps)
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

instance ArbitraryMessage ps => ArbitraryMessage (Handshake ps) where
  arbitraryMessage = oneof
    [ AnyMessageAndAgency (ClientAgency TokInit) . MsgHandshake . T.pack <$> arbitrary
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
      MsgHandshake sig -> MsgHandshake . T.pack <$> shrink (T.unpack sig)
    ClientAgency (TokLiftClient tok) -> \case
      MsgLift msg -> MsgLift <$> shrinkMessage (ClientAgency tok) msg
    ServerAgency TokHandshake -> const []
    ServerAgency (TokLiftServer tok) -> \case
      MsgLift msg -> MsgLift <$> shrinkMessage (ServerAgency tok) msg

instance MessageEq ps => MessageEq (Handshake ps) where
  messageEq (AnyMessageAndAgency tok1 msg1) (AnyMessageAndAgency tok2 msg2)= case (tok1, tok2) of
    (ClientAgency TokInit, ClientAgency TokInit) -> case (msg1, msg2) of
      (MsgHandshake sig, MsgHandshake sig') -> sig == sig'
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

instance ShowProtocol ps => ShowProtocol (Handshake ps) where
  showsPrecMessage p tok = \case
    MsgHandshake sig -> showParen (p >= 11)
      ( showString "MsgHandshake"
      . showSpace
      . showsPrec 11 sig
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

instance BinaryMessage ps => BinaryMessage (Handshake ps) where
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgHandshake sig -> do
        putWord8 0x00
        put sig
    ServerAgency TokHandshake -> \case
      MsgAccept -> putWord8 0x01
      MsgReject -> putWord8 0x02
    ClientAgency (TokLiftClient tok) -> \case
      MsgLift msg -> putMessage (ClientAgency tok) msg
    ServerAgency (TokLiftServer tok) -> \case
      MsgLift msg -> putMessage (ServerAgency tok) msg

  getMessage = \case
    ClientAgency TokInit -> do
      tag <- getWord8
      unless (tag == 0x00) $ fail $ "Expected 0x00, got " <> show tag
      SomeMessage . MsgHandshake <$> get
    ServerAgency TokHandshake -> do
      tag <- getWord8
      case tag of
        0x01 -> pure $ SomeMessage MsgAccept
        0x02 -> pure $ SomeMessage MsgReject
        _ -> fail $ "Expected 0x01 or 0x02, got " <> show tag
    ClientAgency (TokLiftClient tok) -> do
      SomeMessage msg <- getMessage $ ClientAgency tok
      pure $ SomeMessage $ MsgLift msg
    ServerAgency (TokLiftServer tok) -> do
      SomeMessage msg <- getMessage $ ServerAgency tok
      pure $ SomeMessage $ MsgLift msg

instance MessageToJSON ps => MessageToJSON (Handshake ps) where
  messageToJSON = \case
    ClientAgency TokInit -> \case
      MsgHandshake sig -> object [ "handshake" .= sig ]
    ClientAgency (TokLiftClient tok) -> \case
      MsgLift msg -> object [ "lift" .= messageToJSON (ClientAgency tok) msg ]
    ServerAgency TokHandshake -> String . \case
      MsgAccept -> "accept"
      MsgReject -> "reject"
    ServerAgency (TokLiftServer tok) -> \case
      MsgLift msg -> object [ "lift" .= messageToJSON (ServerAgency tok) msg ]
