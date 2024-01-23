{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.App.Build (
  buildApplication,
  buildCreation,
  buildWithdrawal,
) where

import Data.Bifunctor (bimap)
import Data.Time (UTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Void (Void)
import Language.Marlowe (POSIXTime (..))
import Language.Marlowe.Runtime.App.Types (Client, TxBodyInEraWithReferenceScripts (..))
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace (..), TokenName, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion (..), MarloweTransactionMetadata, MarloweVersion)
import Language.Marlowe.Runtime.Transaction.Api (
  ContractCreated (..),
  ContractCreatedInEra (..),
  Destination (ToAddress),
  InputsApplied (..),
  InputsAppliedInEra (..),
  MarloweTxCommand (ApplyInputs, Create, Withdraw),
  RoleTokensConfig (..),
  WalletAddresses (WalletAddresses),
  WithdrawTx (..),
  WithdrawTxInEra (..),
  mkMint,
 )
import Network.Protocol.Job.Client (liftCommand)

import qualified Data.List.NonEmpty as NE (fromList)
import qualified Data.Map.Strict as M (Map, null, toList)
import Data.Set (Set)
import qualified Data.Set as S (fromList)
import Language.Marlowe.Runtime.Client (runMarloweTxClient)

buildCreation
  :: MarloweVersion v
  -> Contract v
  -> M.Map TokenName Address
  -> Maybe Lovelace
  -> MarloweTransactionMetadata
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String (ContractId, TxBodyInEraWithReferenceScripts))
buildCreation version' contract roles minUtxo metadata' =
  let roles' =
        if M.null roles
          then RoleTokensNone
          else
            RoleTokensMint . mkMint . fmap (\(token, addr) -> (token, Nothing, ToAddress addr, 1)) . NE.fromList . M.toList $
              roles
   in build show (\(ContractCreated era ContractCreatedInEra{..}) -> (contractId, TxBodyInEraWithReferenceScripts era txBody)) $
        \w -> Create Nothing version' w Nothing roles' metadata' minUtxo mempty $ Left contract

buildApplication
  :: MarloweVersion v
  -> ContractId
  -> Inputs v
  -> Maybe POSIXTime
  -> Maybe POSIXTime
  -> MarloweTransactionMetadata
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String (ContractId, TxBodyInEraWithReferenceScripts))
buildApplication version' contractId' inputs lower upper metadata' =
  build
    show
    ( \(InputsApplied era InputsAppliedInEra{contractId, txBody}) -> (contractId, TxBodyInEraWithReferenceScripts era txBody)
    )
    $ \w -> ApplyInputs version' w contractId' metadata' (utcTime <$> lower) (utcTime <$> upper) inputs

buildWithdrawal
  :: MarloweVersion v
  -> Set TxOutRef
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String TxBodyInEraWithReferenceScripts)
buildWithdrawal version payouts =
  build show (\(WithdrawTx era WithdrawTxInEra{txBody}) -> TxBodyInEraWithReferenceScripts era txBody) $
    \w -> Withdraw version w payouts

build
  :: (err -> String)
  -> (result -> a)
  -> (WalletAddresses -> MarloweTxCommand Void err result)
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String a)
build showError getBody command addresses change collaterals =
  let command' = command $ WalletAddresses change (S.fromList addresses) (S.fromList collaterals)
   in fmap (bimap showError getBody)
        . runMarloweTxClient
        $ liftCommand command'

utcTime :: POSIXTime -> UTCTime
utcTime = posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000) . fromInteger . getPOSIXTime
