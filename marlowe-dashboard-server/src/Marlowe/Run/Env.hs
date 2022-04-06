{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Marlowe.Run.Env where

import Cardano.Api (NetworkId)
import Colog (HasLog, LogAction, Message, Msg (..), WithLog, cmap, getLogAction, setLogAction, withLog)
import Control.Monad.Reader (MonadReader)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude
import Servant.Client (ClientEnv)

data Env m = Env
  { envClientEnv           :: ClientEnv
  , envChainIndexClientEnv :: ClientEnv
  , envNetworkId           :: NetworkId
  , envLogAction           :: !(LogAction m Message)
  }

instance HasLog (Env m) Message m where
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

type HasEnv m = (MonadReader (Env m) m, HasLog (Env m) Message m)

cmapText :: (Text -> Text) -> LogAction m Message -> LogAction m Message
cmapText f = cmap f'
  where
    f' m = m { msgText = f $ msgText m }

withNamespace :: WithLog env Message m => [Text] -> m a -> m a
withNamespace prefix =
  withLog $ cmapText $ T.intercalate ":" . (prefix <>) . pure
