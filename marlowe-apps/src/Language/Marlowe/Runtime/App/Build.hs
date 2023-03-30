{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.Build
  ( buildApplication
  , buildCreation
  , buildWithdrawal
  ) where


import Data.Bifunctor (bimap, second)
import Data.Time (UTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Void (Void)
import Language.Marlowe (POSIXTime(..))
import Language.Marlowe.Runtime.App.Types (Client)
import Language.Marlowe.Runtime.ChainSync.Api (Address, Lovelace(..), TokenName, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(..), MarloweTransactionMetadata, MarloweVersion)
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError
  , ContractCreated(..)
  , CreateError
  , InputsApplied(..)
  , MarloweTxCommand(ApplyInputs, Create, Withdraw)
  , RoleTokensConfig(..)
  , WalletAddresses(WalletAddresses)
  , WithdrawError
  , mkMint
  )
import Network.Protocol.Job.Client (liftCommand)

import qualified Cardano.Api as C (BabbageEra, TxBody)
import qualified Data.List.NonEmpty as NE (fromList)
import qualified Data.Map.Strict as M (Map, null, toList)
import qualified Data.Set as S (fromList)
import Language.Marlowe.Runtime.Client (runMarloweTxClient)


buildCreation
  :: Show (CreateError v)
  => MarloweVersion v
  -> Contract v
  -> M.Map TokenName Address
  -> Lovelace
  -> MarloweTransactionMetadata
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String (ContractId, C.TxBody C.BabbageEra))
buildCreation version' contract roles minUtxo metadata' =
  let
    roles' =
      if M.null roles
        then RoleTokensNone
        else RoleTokensMint . mkMint . fmap (second (, Nothing)) . NE.fromList . M.toList $ roles
  in
    build show (\ContractCreated{..} -> (contractId, txBody))
      $ \w -> Create Nothing version' w roles' metadata' minUtxo contract


buildApplication
  :: Show (ApplyInputsError v)
  => MarloweVersion v
  -> ContractId
  -> Inputs v
  -> Maybe POSIXTime
  -> Maybe POSIXTime
  -> MarloweTransactionMetadata
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String (ContractId, C.TxBody C.BabbageEra))
buildApplication version' contractId' inputs lower upper metadata' =
  build show (\InputsApplied{contractId, txBody} -> (contractId, txBody))
    $ \w -> ApplyInputs version' w contractId' metadata' (utcTime <$> lower) (utcTime <$> upper) inputs


buildWithdrawal
  :: Show (WithdrawError v)
  => MarloweVersion v
  -> ContractId
  -> TokenName
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String (ContractId, C.TxBody C.BabbageEra))
buildWithdrawal version contractId' role =
  build show (contractId',)
    $ \w -> Withdraw version w contractId' role


build
  :: (err -> String)
  -> (result -> (ContractId, C.TxBody C.BabbageEra))
  -> (WalletAddresses -> MarloweTxCommand Void err result)
  -> [Address]
  -> Address
  -> [TxOutRef]
  -> Client (Either String (ContractId, C.TxBody C.BabbageEra))
build showError getBody command addresses change collaterals =
  let
    command' = command $ WalletAddresses change (S.fromList addresses) (S.fromList collaterals)
  in
    fmap (bimap showError getBody)
      . runMarloweTxClient
      $ liftCommand command'


utcTime :: POSIXTime -> UTCTime
utcTime = posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000) . fromInteger . getPOSIXTime
