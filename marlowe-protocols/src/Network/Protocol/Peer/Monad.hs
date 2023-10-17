{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Protocol.Peer.Monad (
  (<*>),
  (>>),
  (>>=),
  PeerT,
  ClientT,
  ServerT,
  await',
  await,
  fail,
  fmap,
  ihoistPeerT,
  liftPeerT,
  localDriver,
  runPeerT',
  runPeerT,
  withEvent,
  withEventArgs,
  withEventFields,
  withInjectEvent,
  withInjectEventArgs,
  withInjectEventFields,
  yield',
  yield,
) where

import Control.Monad.Event.Class (Inject (..), MonadEvent, MonadInjectEvent)
import qualified Control.Monad.Event.Class as E
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.Functor ((<&>))
import Network.Protocol.Driver (hoistDriver)
import Network.Protocol.Peer.Trace (LiftProtocol (..), SomeSubMessage (..))
import Network.Protocol.Singleton
import Network.TypedProtocol hiding (FlipAgency, TheyHaveAgency)
import Observe.Event (Event, InjectSelector, NewEventArgs (..))
import Observe.Event.Backend (simpleNewEventArgs)
import UnliftIO (MonadIO (..))
import Prelude (Functor, Monad, MonadFail, String, const, (.))
import qualified Prelude as P

newtype PeerT (pr :: PeerRole) ps (i :: ps) (j :: ps) m a = PeerT
  { unPeerT :: forall dState. dState -> Driver ps dState m -> m (a, dState)
  }
  deriving (Functor)

type ClientT ps = PeerT 'AsClient ps
type ServerT ps = PeerT 'AsServer ps

liftPeerT
  :: forall ps ps' pr (i :: ps) (j :: ps) (stLift :: ps -> ps') m a
   . (Functor m)
  => LiftProtocol ps ps' stLift
  -> PeerT pr ps i j m a
  -> PeerT pr ps' (stLift i) (stLift j) m a
liftPeerT LiftProtocol{..} (PeerT m) = PeerT \dState Driver{..} ->
  m
    dState
    Driver
      { sendMessage = \tok msg -> case tok of
          ClientAgency tok' -> sendMessage (ClientAgency (liftClient tok')) (liftMessage msg)
          ServerAgency tok' -> sendMessage (ServerAgency (liftServer tok')) (liftMessage msg)
      , recvMessage = \tok dState' -> case tok of
          ServerAgency tok' ->
            recvMessage (ServerAgency (liftServer tok')) dState' <&> \case
              (SomeMessage msg, dState'') -> case unliftMessage msg of
                SomeSubMessage msg' -> (SomeMessage msg', dState'')
          ClientAgency tok' ->
            recvMessage (ClientAgency (liftClient tok')) dState' <&> \case
              (SomeMessage msg, dState'') -> case unliftMessage msg of
                SomeSubMessage msg' -> (SomeMessage msg', dState'')
      , ..
      }

localDriver
  :: (forall dState. Driver ps dState m -> Driver ps dState m)
  -> PeerT pr ps i j m a
  -> PeerT pr ps i j m a
localDriver f (PeerT m) = PeerT \dState -> m dState P.. f

ihoistPeerT
  :: (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> PeerT pr ps i j m a
  -> PeerT pr ps i j n a
ihoistPeerT f g (PeerT m) = PeerT \dState driver -> f P.$ m dState (hoistDriver g driver)

fmap :: (Functor m) => (a -> b) -> PeerT pr ps i j m a -> PeerT pr ps i j m b
fmap = P.fmap

(>>=) :: (Monad m) => PeerT pr ps i j m a -> (a -> PeerT pr ps j k m b) -> PeerT pr ps i k m b
PeerT f >>= k = PeerT \dState driver -> do
  (a, dState') <- f dState driver
  unPeerT (k a) dState' driver

(>>) :: (Monad m) => PeerT pr ps i j m a -> PeerT pr ps j k m b -> PeerT pr ps i k m b
ma >> mv = ma >>= const mv

fail :: (MonadFail m) => String -> PeerT pr ps i j m a
fail s = PeerT \_ _ -> P.fail s

(<*>) :: (Monad m) => PeerT pr ps i j m (a -> b) -> PeerT pr ps j k m a -> PeerT pr ps i k m b
PeerT mf <*> PeerT ma = PeerT \dState driver -> do
  (f, dState') <- mf dState driver
  (a, dState'') <- ma dState' driver
  P.pure (f a, dState'')

instance (Monad m) => P.Applicative (PeerT pr ps i i m) where
  pure a = PeerT \dState _ -> P.pure (a, dState)
  (<*>) = (<*>)

instance (Monad m) => P.Monad (PeerT pr ps i i m) where
  (>>=) = (>>=)

instance (MonadFail m) => P.MonadFail (PeerT pr ps i i m) where
  fail = fail

instance (MonadFix m) => MonadFix (PeerT pr ps i i m) where
  mfix f = PeerT \dState driver -> mfix \ ~(a, _) -> unPeerT (f a) dState driver

instance (MonadIO m) => MonadIO (PeerT pr ps i i m) where
  liftIO = lift P.. liftIO

instance MonadTrans (PeerT pr ps i i) where
  lift m = PeerT \dState _ -> (,dState) P.<$> m

instance (MonadReader r m) => MonadReader r (PeerT pr ps i i m) where
  ask = lift ask
  local = local'

instance (MonadState s m) => MonadState s (PeerT pr ps i i m) where
  state f = PeerT \dState _ -> (,dState) P.<$> state f

instance (MonadWriter w m) => MonadWriter w (PeerT pr ps i i m) where
  tell = lift P.. tell
  listen = listen'
  pass = pass'

local' :: (MonadReader r m) => (r -> r) -> PeerT pr ps i j m a -> PeerT pr ps i j m a
local' f = ihoistPeerT (local f) (local f)

listen' :: (MonadWriter w m) => PeerT pr ps i j m a -> PeerT pr ps i j m (a, w)
listen' (PeerT m) = PeerT \dState driver -> do
  ((a, dState'), w) <- listen P.$ m dState driver
  P.pure ((a, w), dState')

pass' :: (MonadWriter w m) => PeerT pr ps i j m (a, w -> w) -> PeerT pr ps i j m a
pass' (PeerT m) = PeerT \dState driver -> pass do
  ((a, f), dState') <- m dState driver
  P.pure ((a, dState'), f)

withEventFields
  :: forall r s t pr ps i j m f a
   . (MonadInjectEvent r s t m)
  => s f
  -> [f]
  -> (Event m r f -> PeerT pr ps i j m a)
  -> PeerT pr ps i j m a
withEventFields s fs = withEventArgs (simpleNewEventArgs s){newEventInitialFields = fs}

withEventArgs
  :: forall r s t pr ps i j m f a
   . (MonadInjectEvent r s t m)
  => NewEventArgs r s f
  -> (Event m r f -> PeerT pr ps i j m a)
  -> PeerT pr ps i j m a
withEventArgs = withInjectEventArgs inject

withInjectEventFields
  :: forall r s t pr ps i j m f a
   . (MonadEvent r t m)
  => InjectSelector s t
  -> s f
  -> [f]
  -> (Event m r f -> PeerT pr ps i j m a)
  -> PeerT pr ps i j m a
withInjectEventFields inj s fs = withInjectEventArgs inj (simpleNewEventArgs s){newEventInitialFields = fs}

withInjectEventArgs
  :: forall r s t pr ps i j m f a
   . (MonadEvent r t m)
  => InjectSelector s t
  -> NewEventArgs r s f
  -> (Event m r f -> PeerT pr ps i j m a)
  -> PeerT pr ps i j m a
withInjectEventArgs inj args f = PeerT \dState driver ->
  E.withInjectEventArgs inj args \ev -> unPeerT (f ev) dState driver

withEvent :: (MonadInjectEvent r s t m) => s f -> (Event m r f -> PeerT pr ps i j m a) -> PeerT pr ps i j m a
withEvent = withEventArgs . simpleNewEventArgs

withInjectEvent
  :: (MonadEvent r t m) => InjectSelector s t -> s f -> (Event m r f -> PeerT pr ps i j m a) -> PeerT pr ps i j m a
withInjectEvent inj = withInjectEventArgs inj . simpleNewEventArgs

yield' :: (Monad m) => WeHaveAgency pr i -> Message ps i j -> PeerT pr ps i j m ()
yield' tok msg = PeerT \dState driver -> do
  sendMessage driver tok msg
  P.pure ((), dState)

yield :: (Monad m, SingWeHaveAgency pr i) => Message ps i j -> PeerT pr ps i j m ()
yield = yield' singWeHaveAgency

await'
  :: (Monad m)
  => TheyHaveAgency pr i
  -> (forall j. Message ps i j -> PeerT pr ps j k m a)
  -> PeerT pr ps i k m a
await' tok k = PeerT \dState driver -> do
  (SomeMessage msg, dState') <- recvMessage driver tok dState
  unPeerT (k msg) dState' driver

await
  :: (Monad m, SingTheyHaveAgency pr i)
  => (forall j. Message ps i j -> PeerT pr ps j k m a)
  -> PeerT pr ps i k m a
await = await' singTheyHaveAgency

runPeerT' :: NobodyHasAgency j -> PeerT pr ps i j m a -> dState -> Driver ps dState m -> m (a, dState)
runPeerT' _ (PeerT m) = m

runPeerT :: (SingNobodyHasAgency j) => PeerT pr ps i j m a -> dState -> Driver ps dState m -> m (a, dState)
runPeerT = runPeerT' singNobodyHasAgency
