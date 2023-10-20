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
  ClientT,
  PeerT,
  ServerT,
  await',
  await,
  awaitExplicit,
  fail,
  fmap,
  ihoistPeerT,
  join,
  liftPeerT,
  pure,
  return,
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
  yieldExplicit,
) where

import Control.Monad.Event.Class (Inject (..), MonadEvent, MonadInjectEvent)
import qualified Control.Monad.Event.Class as E
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import qualified Control.Monad.Trans as T
import Control.Monad.Writer (MonadWriter (..))
import Data.Functor ((<&>))
import Data.Void (absurd)
import Network.Protocol.Peer.Trace (LiftProtocol (..), SomeSubMessage (..))
import Network.Protocol.Singleton
import Network.TypedProtocol hiding (Driver (..), FlipAgency, TheyHaveAgency)
import Observe.Event (Event, InjectSelector, NewEventArgs (..))
import Observe.Event.Backend (simpleNewEventArgs)
import UnliftIO (MonadIO (..))
import Prelude (Functor, Monad, MonadFail, String, const, (.))
import qualified Prelude as P

data Channel (pr :: PeerRole) ps (st :: ps) m = Channel
  { sendMessage :: forall st'. WeHaveAgency pr st -> Message ps st st' -> m (Channel pr ps st' m)
  , recvMessage :: TheyHaveAgency pr st -> m (SomeMessageAndChannel pr ps st m)
  }

data SomeMessageAndChannel (pr :: PeerRole) ps (st :: ps) m where
  SomeMessageAndChannel
    :: Message ps st st'
    -> Channel pr ps st' m
    -> SomeMessageAndChannel pr ps st m

newtype PeerT (pr :: PeerRole) ps (i :: ps) (j :: ps) m a = PeerT
  { unPeerT :: Channel pr ps i m -> m (a, Channel pr ps j m)
  }
  deriving (Functor)

type ClientT ps = PeerT 'AsClient ps
type ServerT ps = PeerT 'AsServer ps

lowerChannel
  :: forall ps ps' pr (st :: ps) (stLift :: ps -> ps') m
   . (Functor m)
  => LiftProtocol ps ps' stLift
  -> Channel pr ps' (stLift st) m
  -> Channel pr ps st m
lowerChannel l@LiftProtocol{..} Channel{..} = do
  Channel
    { sendMessage = \tok msg ->
        lowerChannel l P.<$> case tok of
          ClientAgency tok' -> sendMessage (ClientAgency (liftClient tok')) (liftMessage msg)
          ServerAgency tok' -> sendMessage (ServerAgency (liftServer tok')) (liftMessage msg)
    , recvMessage = \case
        ServerAgency tok' ->
          recvMessage (ServerAgency (liftServer tok')) <&> \case
            SomeMessageAndChannel msg channel' -> case unliftMessage msg of
              SomeSubMessage msg' -> SomeMessageAndChannel msg' (lowerChannel l channel')
        ClientAgency tok' ->
          recvMessage (ClientAgency (liftClient tok')) <&> \case
            SomeMessageAndChannel msg channel' -> case unliftMessage msg of
              SomeSubMessage msg' -> SomeMessageAndChannel msg' (lowerChannel l channel')
    , ..
    }

liftPeerT
  :: forall ps ps' pr (i :: ps) (j :: ps) (stLift :: ps -> ps') m a
   . (Functor m, Protocol ps')
  => NobodyHasAgency j
  -> LiftProtocol ps ps' stLift
  -> PeerT pr ps i j m a
  -> PeerT pr ps' (stLift i) (stLift j) m a
liftPeerT tok l (PeerT m) = PeerT \channel ->
  m (lowerChannel l channel) <&> \case
    (a, _) -> (a, absurdChannel (liftNobody l tok))

absurdChannel
  :: (Protocol ps')
  => NobodyHasAgency (stLift st)
  -> Channel pr ps' (stLift st) m'
absurdChannel tok =
  Channel
    { sendMessage = \case
        ClientAgency tok' -> absurd P.$ exclusionLemma_NobodyAndClientHaveAgency tok tok'
        ServerAgency tok' -> absurd P.$ exclusionLemma_NobodyAndServerHaveAgency tok tok'
    , recvMessage = \case
        ClientAgency tok' -> absurd P.$ exclusionLemma_NobodyAndClientHaveAgency tok tok'
        ServerAgency tok' -> absurd P.$ exclusionLemma_NobodyAndServerHaveAgency tok tok'
    }

hoistChannel
  :: forall ps pr st m n
   . (Functor m, Functor n)
  => (forall x. m x -> n x)
  -> Channel ps pr st m
  -> Channel ps pr st n
hoistChannel f = go
  where
    go :: Channel ps pr st' m -> Channel ps pr st' n
    go Channel{..} =
      Channel
        { sendMessage = \tok msg -> f P.$ go P.<$> sendMessage tok msg
        , recvMessage = \tok ->
            f (recvMessage tok) <&> \case
              SomeMessageAndChannel msg channel -> SomeMessageAndChannel msg P.$ go channel
        }

ihoistPeerT
  :: (Monad n, Monad m)
  => (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> PeerT pr ps i j m a
  -> PeerT pr ps i j n a
ihoistPeerT f g (PeerT m) = PeerT \channel -> do
  (a, channel') <- f P.$ m (hoistChannel g channel)
  P.pure (a, hoistChannel f channel')

fmap :: (Functor m) => (a -> b) -> PeerT pr ps i j m a -> PeerT pr ps i j m b
fmap = P.fmap

return :: (P.Applicative m) => a -> PeerT pr ps i i m a
return = pure

lift :: (Functor m) => m a -> PeerT pr ps i i m a
lift ma = PeerT \channel -> (,channel) P.<$> ma

pure :: (P.Applicative m) => a -> PeerT pr ps i i m a
pure a = PeerT \channel -> P.pure (a, channel)

(>>=) :: (Monad m) => PeerT pr ps i j m a -> (a -> PeerT pr ps j k m b) -> PeerT pr ps i k m b
PeerT f >>= k = PeerT \channel -> do
  (a, channel') <- f channel
  unPeerT (k a) channel'

join :: (Monad m) => PeerT pr ps i j m (PeerT pr ps j k m b) -> PeerT pr ps i k m b
join = (>>= P.id)

(>>) :: (Monad m) => PeerT pr ps i j m a -> PeerT pr ps j k m b -> PeerT pr ps i k m b
ma >> mv = ma >>= const mv

fail :: (MonadFail m) => String -> PeerT pr ps i j m a
fail s = PeerT \_ -> P.fail s

(<*>) :: (Monad m) => PeerT pr ps i j m (a -> b) -> PeerT pr ps j k m a -> PeerT pr ps i k m b
PeerT mf <*> PeerT ma = PeerT \channel -> do
  (f, channel') <- mf channel
  (a, channel'') <- ma channel'
  P.pure (f a, channel'')

instance (Monad m) => P.Applicative (PeerT pr ps i i m) where
  pure = pure
  (<*>) = (<*>)

instance (Monad m) => P.Monad (PeerT pr ps i i m) where
  (>>=) = (>>=)

instance (MonadFail m) => P.MonadFail (PeerT pr ps i i m) where
  fail = fail

instance (MonadFix m) => MonadFix (PeerT pr ps i i m) where
  mfix f = PeerT \channel -> mfix \ ~(a, _) -> unPeerT (f a) channel

instance (MonadIO m) => MonadIO (PeerT pr ps i i m) where
  liftIO = lift P.. liftIO

instance T.MonadTrans (PeerT pr ps i i) where
  lift m = PeerT \channel -> (,channel) P.<$> m

instance (MonadReader r m) => MonadReader r (PeerT pr ps i i m) where
  ask = lift ask
  local = local'

instance (MonadState s m) => MonadState s (PeerT pr ps i i m) where
  state f = PeerT \channel -> (,channel) P.<$> state f

instance (MonadWriter w m) => MonadWriter w (PeerT pr ps i i m) where
  tell = lift P.. tell
  listen = listen'
  pass = pass'

local' :: (MonadReader r m) => (r -> r) -> PeerT pr ps i j m a -> PeerT pr ps i j m a
local' f (PeerT m) = PeerT P.$ local f . m

listen' :: (MonadWriter w m) => PeerT pr ps i j m a -> PeerT pr ps i j m (a, w)
listen' (PeerT m) = PeerT \channel -> do
  ((a, channel'), w) <- listen P.$ m channel
  P.pure ((a, w), channel')

pass' :: (MonadWriter w m) => PeerT pr ps i j m (a, w -> w) -> PeerT pr ps i j m a
pass' (PeerT m) = PeerT \channel -> pass do
  ((a, f), channel') <- m channel
  P.pure ((a, channel'), f)

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
withInjectEventArgs inj args f = PeerT \channel ->
  E.withInjectEventArgs inj args \ev -> unPeerT (f ev) channel

withEvent :: (MonadInjectEvent r s t m) => s f -> (Event m r f -> PeerT pr ps i j m a) -> PeerT pr ps i j m a
withEvent = withEventArgs . simpleNewEventArgs

withInjectEvent
  :: (MonadEvent r t m) => InjectSelector s t -> s f -> (Event m r f -> PeerT pr ps i j m a) -> PeerT pr ps i j m a
withInjectEvent inj = withInjectEventArgs inj . simpleNewEventArgs

yieldExplicit :: (Monad m) => WeHaveAgency pr i -> Message ps i j -> PeerT pr ps i j m ()
yieldExplicit tok msg = PeerT \channel -> do
  channel' <- sendMessage channel tok msg
  P.pure ((), channel')

yield' :: (Monad m, OurRole pr) => OurAgency pr i -> Message ps i j -> PeerT pr ps i j m ()
yield' = yieldExplicit . ourAgency

yield :: (Monad m, SingWeHaveAgency pr i) => Message ps i j -> PeerT pr ps i j m ()
yield = yieldExplicit singWeHaveAgency

awaitExplicit
  :: (Monad m)
  => TheyHaveAgency pr i
  -> (forall j. Message ps i j -> PeerT pr ps j k m a)
  -> PeerT pr ps i k m a
awaitExplicit tok k = PeerT \channel -> do
  SomeMessageAndChannel msg channel' <- recvMessage channel tok
  unPeerT (k msg) channel'

await
  :: (Monad m, SingTheyHaveAgency pr i)
  => (forall j. Message ps i j -> PeerT pr ps j k m a)
  -> PeerT pr ps i k m a
await = awaitExplicit singTheyHaveAgency

await'
  :: (Monad m, TheirRole pr)
  => TheirAgency pr i
  -> (forall j. Message ps i j -> PeerT pr ps j k m a)
  -> PeerT pr ps i k m a
await' tok = awaitExplicit (theirAgency tok)

runPeerT' :: (Functor m) => NobodyHasAgency j -> PeerT pr ps i j m a -> Channel pr ps i m -> m a
runPeerT' _ (PeerT m) = P.fmap P.fst . m

runPeerT :: (Functor m, SingNobodyHasAgency j) => PeerT pr ps i j m a -> Channel pr ps i m -> m a
runPeerT = runPeerT' singNobodyHasAgency
