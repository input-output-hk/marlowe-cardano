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
import OpenTelemetry.Exporter.Handle (defaultFormatter, makeHandleExporter)
import OpenTelemetry.Processor.Batch (batchProcessor, batchTimeoutConfig)
import OpenTelemetry.Trace
import OpenTelemetry.Trace.Core (getSpanContext, wrapSpanContext)
import OpenTelemetry.Trace.Sampler (alwaysOn)
import System.Environment (lookupEnv)
import System.IO (Handle)
import UnliftIO (BufferMode (..), MonadUnliftIO, bracket, hSetBuffering, newMVar, stderr, stdout, withMVar, withRunInIO)

newtype AppM r s a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r s) r s, LogAction IO Message) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadCatch, MonadMask, MonadFail)

data TracingConfig s
  = UseEmptyTracerProvider
      InstrumentationLibrary
  | UseDefaultTracerProvider
      InstrumentationLibrary
      (RenderSelectorOTel s)
  | UseHandleDebugTracerProvider
      Handle
      InstrumentationLibrary
      (RenderSelectorOTel s)

mkEventBackend :: TracingConfig s -> IO (EventBackend IO Span s, IO ())
mkEventBackend = \case
  UseEmptyTracerProvider library -> do
    provider <- createTracerProvider [] emptyTracerProviderOptions
    let tracer = makeTracer provider library tracerOptions
    dummyContext <- inSpan' tracer "dummy" defaultSpanArguments getSpanContext
    pure
      ( noopEventBackend $ wrapSpanContext dummyContext
      , shutdownTracerProvider provider
      )
  UseDefaultTracerProvider library render -> do
    provider <- initializeTracerProvider
    let tracer = makeTracer provider library tracerOptions
    pure
      ( tracerEventBackend tracer render
      , shutdownTracerProvider provider
      )
  UseHandleDebugTracerProvider handle library render -> do
    provider <- do
      (_, tracerOptions') <- getTracerProviderInitializationOptions
      stderrProc <- batchProcessor batchTimeoutConfig $ makeHandleExporter handle (pure . defaultFormatter)
      let processors' = [stderrProc]
      createTracerProvider processors' (tracerOptions'{tracerProviderOptionsSampler = alwaysOn})
    let tracer = makeTracer provider library tracerOptions
    pure
      ( tracerEventBackend tracer render
      , shutdownTracerProvider provider
      )

runAppMTraced
  :: forall s a
   . InstrumentationLibrary
  -> RenderSelectorOTel s
  -> AppM Span s a
  -> IO a
runAppMTraced library render app = do
  otelExporterEndpointConfigured <- isJust <$> lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  stderrDebugExporterConfigured <- isJust <$> lookupEnv "OTEL_EXPORTER_STDERR_DEBUG"
  logAction <- concurrentLogger
  tracingConfig <- case (otelExporterEndpointConfigured, stderrDebugExporterConfigured) of
    (True, True) -> do
      usingLoggerT logAction $
        logWarning
          "Both OTEL_EXPORTER_OTLP_ENDPOINT and OTEL_EXPORTER_STDERR_DEBUG are set. Ignoring OTEL_EXPORTER_STDERR_DEBUG."
      pure $ UseDefaultTracerProvider library render
    (True, False) -> pure $ UseDefaultTracerProvider library render
    (_, True) -> pure $ UseHandleDebugTracerProvider stderr library render
    _ -> pure $ UseEmptyTracerProvider library
  runAppMTraced' tracingConfig logAction app

runAppMTraced'
  :: forall s a
   . TracingConfig s
  -> LogAction IO Message
  -> AppM Span s a
  -> IO a
runAppMTraced' tracingConfig logAction app = bracket
  (mkEventBackend tracingConfig)
  snd
  \(backend, _) -> do
    hSetBuffering stderr LineBuffering
    hSetBuffering stdout LineBuffering
    runAppM backend logAction app

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
