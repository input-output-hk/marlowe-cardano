{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Query
  where

import Data.Map (Map)
import Language.Marlowe.Runtime.ChainSync.Api (TransactionOutput, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion, PayoutDatum, TransactionScriptOutput)
import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Runtime.Transaction.Constraints

type LoadWalletContext = WalletAddresses -> IO WalletContext

type LoadMarloweScriptOutput =
  forall v. MarloweVersion v -> ContractId -> IO (Maybe (TransactionScriptOutput v, TransactionOutput))

type LoadPayoutScriptOutputs =
  forall v. MarloweVersion v -> PayoutDatum v -> IO (Map TxOutRef TransactionOutput)

loadWalletContext :: LoadWalletContext
loadWalletContext = error "not implemented"

loadMarloweScriptOutput :: LoadMarloweScriptOutput
loadMarloweScriptOutput = error "not implemented"

loadPayoutScriptOutputs :: LoadPayoutScriptOutputs
loadPayoutScriptOutputs = error "not implemented"
