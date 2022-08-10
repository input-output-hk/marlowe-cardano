{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}

module Network.Protocol.Command.Types where

import Network.TypedProtocol

class IsCommand (cmd :: * -> * -> * -> *) where
  data CommandId cmd :: * -> * -> * -> *
  data TokCommand cmd :: * -> * -> * -> *
  tokFromCmd :: cmd status err result -> TokCommand cmd status err result
  tokFromId :: CommandId cmd status err result -> TokCommand cmd status err result

data Command (cmd :: * -> * -> * -> *) where
  StInit :: Command cmd
  StCmd :: status -> err -> result -> Command cmd
  StAwait :: status -> err -> result -> Command cmd
  StDone :: Command cmd

instance Protocol (Command cmd) where
  data Message (Command cmd) from to where
    MsgExec :: cmd status err result -> Message (Command cmd)
      'StInit
      ('StCmd status err result)
    MsgResume :: CommandId cmd status err result -> Message (Command cmd)
      'StInit
      ('StCmd status err result)
    MsgFail :: TokCommand cmd status err result -> err -> Message (Command cmd)
      ('StCmd status err result)
      'StDone
    MsgSucceed :: TokCommand cmd status err result -> result -> Message (Command cmd)
      ('StCmd status err result)
      'StDone
    MsgAwait :: status -> CommandId cmd status err result -> Message (Command cmd)
      ('StCmd status err result)
      ('StAwait status err result)
    MsgPoll :: Message (Command cmd)
      ('StAwait status err result)
      ('StCmd status err result)
    MsgDone :: Message (Command cmd)
      ('StAwait status err result)
      'StDone

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokAwait :: TokCommand cmd status err result -> ClientHasAgency ('StAwait status err result)

  data ServerHasAgency st where
    TokCmd :: TokCommand cmd status err result -> ServerHasAgency ('StCmd status err result)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit      = \case
  exclusionLemma_ClientAndServerHaveAgency (TokAwait _) = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case
