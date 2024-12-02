{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.CommandSpec (spec) where

import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Runtime.Transaction.Gen ()
import Network.Protocol.Codec.Spec (checkPropCodec, codecGoldenTests)
import Network.Protocol.Job.Types
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "MarloweTxCommand" do
  prop "It has a lawful Job protocol codec" $ checkPropCodec @(Job MarloweTxCommand)
  codecGoldenTests @(Job MarloweTxCommand) "Job MarloweTxCommand"
