{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Concurrent.Component.Run where

import Colog
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Event.Class
import Control.Monad.Reader
import Control.Monad.With
import Data.Bifunctor (first)
import Data.GeneralAllocate
import Data.Maybe (isJust)
import Observe.Event (EventBackend)
import Observe.Event.Backend (hoistEventBackend, noopEventBackend)
import Observe.Event.Render.OpenTelemetry (RenderSelectorOTel, tracerEventBackend)
import OpenTelemetry.Trace
import OpenTelemetry.Trace.Core (getSpanContext, wrapSpanContext)
import System.Environment (lookupEnv)
import UnliftIO (BufferMode (..), MonadUnliftIO, bracket, hSetBuffering, newMVar, stderr, stdout, withMVar, withRunInIO)

newtype AppM r s a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r s) r s, LogAction IO Message) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadCatch, MonadMask, MonadFail)

runAppMTraced :: forall s a. InstrumentationLibrary -> RenderSelectorOTel s -> AppM Span s a -> IO a
runAppMTraced library render app = bracket
  initializeTracerProvider'
  snd
  \(backend, _) -> do
    hSetBuffering stderr LineBuffering
    hSetBuffering stdout LineBuffering
    logAction <- concurrentLogger
    runAppM backend logAction app
  where
    initializeTracerProvider' :: IO (EventBackend IO Span s, IO ())
    initializeTracerProvider' = do
      endpointConfigured <- isJust <$> lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
      if endpointConfigured
        then do
          provider <- initializeTracerProvider
          let tracer = makeTracer provider library tracerOptions
          pure
            ( tracerEventBackend tracer render
            , shutdownTracerProvider provider
            )
        else do
          provider <- createTracerProvider [] emptyTracerProviderOptions
          let tracer = makeTracer provider library tracerOptions
          dummyContext <- inSpan' tracer "dummy" defaultSpanArguments getSpanContext
          pure
            ( noopEventBackend $ wrapSpanContext dummyContext
            , shutdownTracerProvider provider
            )

runAppM :: EventBackend IO r s -> LogAction IO Message -> AppM r s a -> IO a
runAppM eventBackend logAction (AppM action) = do
  runReaderT action (hoistEventBackend liftIO eventBackend, logAction)

concurrentLogger :: (MonadUnliftIO m) => IO (LogAction m Message)
concurrentLogger = do
  lock <- newMVar ()
  let LogAction baseAction = cmap fmtMessage logTextStdout
  pure $ LogAction $ withMVar lock . const . baseAction

instance MonadReader (LogAction (AppM r s) Message) (AppM r s) where
  ask = AppM $ asks $ hoistLogAction liftIO . snd
  local f (AppM m) = withRunInIO \runInIO -> runInIO $ AppM $ withReaderT (fmap $ hoistLogAction runInIO . f . hoistLogAction liftIO) m

instance MonadWith (AppM r s) where
  type WithException (AppM r s) = WithException IO
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (AppM r s) (WithException IO) releaseReturn b a
    -> (a -> AppM r s b)
    -> AppM r s (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = AppM . ReaderT $ \r -> do
    let allocA' :: (forall x. IO x -> IO x) -> IO (GeneralAllocated IO (WithException IO) releaseReturn b a)
        allocA' restore = do
          let restore' :: forall x. AppM r s x -> AppM r s x
              restore' mx = AppM . ReaderT $ restore . (runReaderT . unAppM) mx
          GeneralAllocated a releaseA <- (runReaderT . unAppM) (allocA restore') r
          let releaseA' relTy = (runReaderT . unAppM) (releaseA relTy) r
          pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unAppM) r . go)

instance MonadEvent r s (AppM r s) where
  askBackend = askBackendReaderT AppM fst
  localBackend = localBackendReaderT AppM unAppM first
