{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.Store
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (STM)
import Language.Marlowe.Runtime.Indexer.ChainSeekClient (ChainEvent)
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries)
import Observe.Event (EventBackend)

data StoreSelector f

data StoreDependencies r = StoreDependencies
  { databaseQueries :: DatabaseQueries IO
  , eventBackend :: EventBackend IO r StoreSelector
  , pullEvent :: STM ChainEvent
  }

store :: Component IO (StoreDependencies r) ()
store = component_ \_ -> pure ()
