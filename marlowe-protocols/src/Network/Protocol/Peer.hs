{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer
  where

import Control.Monad.Event.Class (MonadEvent, withEvent)
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Observe.Event.Component (GetSelectorConfig, SelectorConfig(..), SomeJSON(..), singletonFieldConfigWith)
import Observe.Event.Explicit (InjectSelector, addField)
import Observe.Event.Network.Protocol (MessageToJSON(..))

data PeerSelector ps f where
  Send :: PeerSelector ps (AnyMessageAndAgency ps)
  Recv :: PeerSelector ps (AnyMessageAndAgency ps)

getPeerSelectorConfig :: MessageToJSON ps => Bool -> GetSelectorConfig (PeerSelector ps)
getPeerSelectorConfig defaultEnabled = \case
  Send -> SelectorConfig "send" defaultEnabled $ singletonFieldConfigWith
    (\(AnyMessageAndAgency tok msg) -> SomeJSON $ messageToJSON tok msg)
    "message"
    True
  Recv -> SelectorConfig "recv" defaultEnabled $ singletonFieldConfigWith
    (\(AnyMessageAndAgency tok msg) -> SomeJSON $ messageToJSON tok msg)
    "message"
    True

hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle

logPeer
  :: MonadEvent r s m
  => InjectSelector (PeerSelector ps) s
  -> Peer ps pr st m a
  -> Peer ps pr st m a
logPeer inject = \case
  Effect m -> Effect $ logPeer inject <$> m
  Done tok a -> Done tok a
  Yield tok msg peer -> Effect $ inject Send \s injField -> withEvent s \ev -> do
    addField ev $ injField $ AnyMessageAndAgency tok msg
    pure $ Yield tok msg $ logPeer inject peer
  Await tok handle -> Await tok \msg -> Effect $ inject Recv \s injField -> withEvent s \ev -> do
    addField ev $ injField $ AnyMessageAndAgency tok msg
    pure $ logPeer inject $ handle msg
