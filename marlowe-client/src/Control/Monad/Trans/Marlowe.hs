{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Marlowe
  where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Allocate (MonadAllocate)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), mapReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.With (MonadWith(..))
import Control.Monad.Writer (MonadWriter)
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(..))
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient)
import Network.Protocol.Connection (SomeClientConnector)
import UnliftIO (MonadUnliftIO)

newtype MarloweT m a = MarloweT { unMarloweT :: ReaderT (SomeClientConnector MarloweRuntimeClient IO) m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadPlus
    , MonadError e
    , MonadState s
    , MonadWriter w
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadCont
    , MonadUnliftIO
    , MonadBase b
    , MonadTransControl
    , MonadBaseControl b
    , MonadResource
    , MonadAllocate
    )

instance MonadWith m => MonadWith (MarloweT m) where
  type WithException (MarloweT m) = WithException m
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (MarloweT m) (WithException m) releaseReturn b a
    -> (a -> MarloweT m b)
    -> MarloweT m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = MarloweT . ReaderT $ \r -> do
    let
      allocA' :: (forall x. m x -> m x) -> m (GeneralAllocated m (WithException m) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. MarloweT m x -> MarloweT m x
          restore' mx = MarloweT . ReaderT $ restore . (runReaderT . unMarloweT) mx
        GeneralAllocated a releaseA <- (runReaderT . unMarloweT) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . unMarloweT) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unMarloweT) r . go)

instance MonadTrans MarloweT where
  lift = MarloweT . lift

instance MonadReader r m => MonadReader r (MarloweT m) where
  ask = lift ask
  local = mapMarloweT . local

mapMarloweT :: (m a -> n b) -> MarloweT m a -> MarloweT n b
mapMarloweT f = MarloweT . mapReaderT f . unMarloweT

runMarloweT :: MarloweT m a -> SomeClientConnector MarloweRuntimeClient IO -> m a
runMarloweT = runReaderT . unMarloweT
