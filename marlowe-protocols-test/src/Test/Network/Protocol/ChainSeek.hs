{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Test.Network.Protocol.ChainSeek
  where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Type.Equality (type (:~:)(Refl))
import Network.Protocol.ChainSeek.Client
import Network.Protocol.ChainSeek.Types
import Test.Hspec (Expectation, expectationFailure)

data ChainSeekServerScript q point tip (m :: * -> *) a
  = RejectHandshake [SchemaVersion] a
  | ConfirmHandshake (ServerStIdleScript q point tip m a)

data ServerStIdleScript q point tip (m :: * -> *) a where
  Do :: m () -> ServerStIdleScript q point tip m a -> ServerStIdleScript q point tip m a
  Halt :: a -> ServerStIdleScript q point tip m a
  ExpectDone :: a -> ServerStIdleScript q point tip m a
  ExpectQuery :: q err result -> ServerStNextScript q point tip err result m a -> ServerStIdleScript q point tip m a

data ServerStNextScript q point tip err result m a where
  RejectQuery :: err -> tip -> ServerStIdleScript q point tip m a -> ServerStNextScript q point tip err result m a
  RollForward :: result -> point -> tip -> ServerStIdleScript q point tip m a -> ServerStNextScript q point tip err result m a
  RollBackward :: point -> tip -> ServerStIdleScript q point tip m a -> ServerStNextScript q point tip err result m a
  Wait :: ServerStPollScript q point tip err result m a -> ServerStNextScript q point tip err result m a

data ServerStPollScript q point tip err result m a where
  ExpectPoll :: ServerStNextScript q point tip err result m a -> ServerStPollScript q point tip err result m a
  ExpectCancel :: ServerStIdleScript q point tip m a -> ServerStPollScript q point tip err result m a

runClientWithScript
  :: forall q point tip m a b
   . (MonadIO m, Query q)
  => (forall err result. q err result -> String)
  -> (forall err result. q err result -> q err result -> Expectation)
  -> ChainSeekServerScript q point tip m a
  -> ChainSeekClient q point tip m b
  -> m (a, Maybe b)
runClientWithScript showQuery assertQueryEq script ChainSeekClient{..} = do
  SendMsgRequestHandshake _ ClientStHandshake{..} <- runChainSeekClient
  case script of
    RejectHandshake versions a -> (a,) . Just <$> recvMsgHandshakeRejected versions
    ConfirmHandshake script'   -> runIdle script' =<< recvMsgHandshakeConfirmed
  where
    runIdle
      :: ServerStIdleScript q point tip m a
      -> ClientStIdle q point tip m b
      -> m (a, Maybe b)
    runIdle = \case
      Do action script' -> \client -> action *> runIdle script' client
      Halt a -> \_ -> pure (a, Nothing)
      ExpectDone a -> \case
        SendMsgDone b -> pure (a, Just b)
        SendMsgQueryNext move _ -> liftIO do
          expectationFailure $ "Expected MsgDone, got MsgQueryNext (" <> showQuery move <> ")"
          undefined
      ExpectQuery move script' -> \case
        SendMsgDone _ -> liftIO do
          expectationFailure $ "Expected MsgQueryNext (" <> showQuery move <> "), got MsgDone"
          undefined
        SendMsgQueryNext move' next -> do
          let tag = tagFromQuery move
          let tag' = tagFromQuery move'
          case tagEq tag tag' of
            Just (Refl, Refl) -> do
              liftIO $ move' `assertQueryEq` move
              runNext next script'
            Nothing -> do
              liftIO $ expectationFailure $ "Expected " <> showQuery move <> ", got " <> showQuery move'
              undefined

    runNext
      :: ClientStNext q err result point tip m b
      -> ServerStNextScript q point tip err result m a
      -> m (a, Maybe b)
    runNext ClientStNext{..} = \case
      RejectQuery err tip script' -> runIdle script' =<< recvMsgQueryRejected err tip
      RollForward result point tip script' -> runIdle script' =<< recvMsgRollForward result point tip
      RollBackward point tip script' -> runIdle script' =<< recvMsgRollBackward point tip
      Wait script' -> runPoll script' =<< recvMsgWait

    runPoll
      :: ServerStPollScript q point tip err result m a
      -> ClientStPoll q err result point tip m b
      -> m (a, Maybe b)
    runPoll = \case
      ExpectPoll script' -> \case
        SendMsgPoll next -> runNext next script'
        SendMsgCancel _ -> liftIO do
          expectationFailure "Expected MsgPoll, got MsgCancel"
          undefined
      ExpectCancel script' -> \case
        SendMsgCancel idle -> runIdle script' idle
        SendMsgPoll _ -> liftIO do
          expectationFailure "Expected MsgCancel, got MsgPoll"
          undefined
