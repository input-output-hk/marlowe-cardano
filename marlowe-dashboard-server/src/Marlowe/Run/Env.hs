{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Marlowe.Run.Env where

import Colog (HasLog (..), LogAction, Message, WithLog, cmap, fmtMessage, logDebug, logInfo, logTextStdout, logWarning,
              usingLoggerT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Servant.Client (ClientEnv)

data Env m = Env
  { envClientEnv :: ClientEnv
  , envLogAction :: !(LogAction m Message)
  }

instance HasLog (Env m) Message m where
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

type HasEnv m = (MonadReader (Env m) m, WithLog (Env m) Message m)
