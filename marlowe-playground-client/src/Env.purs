module Env where

import Marlowe (SPParams_)

-- Application enviroment configuration
newtype type Env
  = { ajaxSettings :: SPSettings_
    }
