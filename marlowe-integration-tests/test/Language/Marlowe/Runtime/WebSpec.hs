{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Marlowe.Runtime.WebSpec
  where

import qualified Language.Marlowe.Runtime.Web.GetContract as GetContract
import qualified Language.Marlowe.Runtime.Web.GetContracts as GetContracts
import qualified Language.Marlowe.Runtime.Web.GetTransactions as GetTransaction
import qualified Language.Marlowe.Runtime.Web.GetTransactions as GetTransactions
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Marlowe runtime Web API" do
  GetContracts.spec
  GetContract.spec
  GetTransactions.spec
  GetTransaction.spec
