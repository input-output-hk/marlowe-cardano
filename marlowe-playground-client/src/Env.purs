module Env where

import Marlowe (class HasSPSettings, SPSettings_)

-- Application enviroment configuration
newtype Env
  = Env { ajaxSettings :: SPSettings_
        }

instance hasSPSettingsEnv :: HasSPSettings Env where
  spSettings (Env { ajaxSettings }) = ajaxSettings
