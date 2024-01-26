{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.App.Run (
  runClientWithConfig,
) where

import Language.Marlowe.Runtime.App.Types (Client, Config (..))
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)

runClientWithConfig
  :: Config
  -> Client a
  -> IO a
runClientWithConfig Config{..} = connectToMarloweRuntime runtimeHost runtimePort
