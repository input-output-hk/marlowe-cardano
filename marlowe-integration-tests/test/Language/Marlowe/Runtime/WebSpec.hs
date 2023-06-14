{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Marlowe.Runtime.WebSpec where


import qualified Language.Marlowe.Runtime.Web.Contracts.GetContracts as Contracts.Get

import qualified Language.Marlowe.Runtime.Web.Contracts.Contract.GetContract as Contracts.Contract.Get
import qualified Language.Marlowe.Runtime.Web.Contracts.Contract.PostContract as Contracts.Contract.Post
import qualified Language.Marlowe.Runtime.Web.Contracts.Contract.PutContract as Contracts.Contract.Put

import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.GetTransactions as Contracts.Transactions.Get

import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.GetTransaction as Contracts.Transactions.Transaction.Get
import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.PostTransaction as Contracts.Transactions.Transaction.Post
import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.PutTransaction as Contracts.Transactions.Transaction.Put


import qualified Language.Marlowe.Runtime.Web.Withdrawal.PostWithdrawal as Withdrawal.Post
import qualified Language.Marlowe.Runtime.Web.Withdrawal.PutWithdrawal as Withdrawal.Put


import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Marlowe runtime Web API" do
  Contracts.Get.spec
  Contracts.Contract.Get.spec
  Contracts.Contract.Post.spec
  Contracts.Contract.Put.spec
  Contracts.Transactions.Get.spec
  Contracts.Transactions.Transaction.Get.spec
  Contracts.Transactions.Transaction.Post.spec
  Contracts.Transactions.Transaction.Put.spec
  Withdrawal.Post.spec
  Withdrawal.Put.spec
