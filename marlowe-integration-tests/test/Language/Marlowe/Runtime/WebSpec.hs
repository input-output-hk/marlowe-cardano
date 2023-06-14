{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Marlowe.Runtime.WebSpec where


import qualified Language.Marlowe.Runtime.Web.Contracts.Get as Contracts.Get


import qualified Language.Marlowe.Runtime.Web.Contracts.Contract.Get as Contracts.Contract.Get
import qualified Language.Marlowe.Runtime.Web.Contracts.Contract.Post as Contracts.Contract.Post
import qualified Language.Marlowe.Runtime.Web.Contracts.Contract.Put as Contracts.Contract.Put

import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.Get as Contracts.Transactions.Get

import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.Get as Contracts.Transactions.Transaction.Get
import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.Post as Contracts.Transactions.Transaction.Post
import qualified Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.Put as Contracts.Transactions.Transaction.Put


import qualified Language.Marlowe.Runtime.Web.Withdrawal.Post as Withdrawal.Post
import qualified Language.Marlowe.Runtime.Web.Withdrawal.Put as Withdrawal.Put


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
