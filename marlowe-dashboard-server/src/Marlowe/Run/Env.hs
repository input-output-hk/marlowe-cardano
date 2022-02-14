{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Marlowe.Run.Env where

import Colog (HasLog, LogAction, Message, getLogAction, setLogAction)
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

type HasEnv m = (MonadReader (Env m) m, HasLog (Env m) Message m)
