{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Monad
  where

import Colog (LogAction, Message, cmap, fmtMessage, hoistLogAction, logError, logTextStdout)
import Control.Monad.Catch (Exception(displayException), MonadCatch, MonadMask, MonadThrow, SomeException(..))
import Control.Monad.Event.Class
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans.Reader (withReaderT)
import Control.Monad.With (MonadWith(..), WithException)
import Data.Bifunctor (first)
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (Version, showVersion)
import Observe.Event (EventBackend)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Render.OpenTelemetry (RenderSelectorOTel, tracerEventBackend)
import OpenTelemetry.Trace
  (InstrumentationLibrary(..), Span, initializeGlobalTracerProvider, makeTracer, shutdownTracerProvider, tracerOptions)
import UnliftIO (MonadIO, MonadUnliftIO, bracket, liftIO, throwIO, try, withRunInIO)

newtype RuntimeM r s a = RuntimeM
  { unRuntimeM :: ReaderT (EventBackend (RuntimeM r s) r s, LogAction IO Message) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail, MonadThrow, MonadCatch, MonadMask)

instance MonadReader (LogAction (RuntimeM r s) Message) (RuntimeM r s) where
  ask = RuntimeM $ asks $ hoistLogAction liftIO . snd
  local f (RuntimeM m) = withRunInIO \runInIO ->
    runInIO $ RuntimeM $ withReaderT (fmap $ hoistLogAction runInIO . f . hoistLogAction liftIO) m

instance MonadWith (RuntimeM r s) where
  type WithException (RuntimeM r s) = WithException IO
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (RuntimeM r s) (WithException IO) releaseReturn b a
    -> (a -> RuntimeM r s b)
    -> RuntimeM r s (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = RuntimeM . ReaderT $ \r -> do
    let
      allocA' :: (forall x. IO x -> IO x) -> IO (GeneralAllocated IO (WithException IO) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. RuntimeM r s x -> RuntimeM r s x
          restore' mx = RuntimeM . ReaderT $ restore . (runReaderT . unRuntimeM) mx
        GeneralAllocated a releaseA <- (runReaderT . unRuntimeM) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . unRuntimeM) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unRuntimeM) r . go)

instance MonadEvent r s (RuntimeM r s) where
  localBackend = localBackendReaderT RuntimeM unRuntimeM first
  askBackend = askBackendReaderT RuntimeM fst

runRuntimeM
  :: EventBackend IO r s
  -> LogAction IO Text
  -> RuntimeM r s a
  -> IO a
runRuntimeM eventBackend logAction action = runReaderT (unRuntimeM action') env
  where
    env =
      ( hoistEventBackend liftIO eventBackend
      , cmap fmtMessage $ hoistLogAction liftIO logAction
      )
    action' = try action >>= \case
      Left (SomeException ex) -> do
        logError "Application crashed with exception:"
        logError $ fromString $ displayException ex
        throwIO ex
      Right a -> pure a

runRuntimeMTraced :: Text -> Version -> RenderSelectorOTel s -> RuntimeM Span s a -> IO a
runRuntimeMTraced libraryName version renderSelector action = bracket
  initializeGlobalTracerProvider
  shutdownTracerProvider
  \provider -> do
    let
      tracer = makeTracer
        provider
        (InstrumentationLibrary libraryName $ T.pack $ showVersion version)
        tracerOptions
    runRuntimeM (tracerEventBackend tracer renderSelector) logTextStdout action
