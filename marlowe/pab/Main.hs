{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
module Main(main) where

import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import MarloweContract (MarloweContract)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Run (runWith)
import System.IO (stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  runWith (Builtin.handleBuiltin @MarloweContract)
