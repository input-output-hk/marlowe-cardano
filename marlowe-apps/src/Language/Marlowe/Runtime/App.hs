{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.Runtime.App
  ( handle
  ) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, catch)
import Data.Bifunctor (second)
import Data.Either (fromRight)
import Language.Marlowe.Runtime.App.Build (buildApplication, buildCreation, buildWithdrawal)
import Language.Marlowe.Runtime.App.List (allContracts, allHeaders, getContract)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.App.Sign (sign)
import Language.Marlowe.Runtime.App.Submit (submit, waitForTx)
import Language.Marlowe.Runtime.App.Types (Config(timeoutSeconds), MarloweRequest(..), MarloweResponse(..), mkBody)
import Language.Marlowe.Runtime.Core.Api
  (MarloweVersion(MarloweV1), MarloweVersionTag(V1), decodeMarloweTransactionMetadataLenient)


handle
  :: Config
  -> MarloweRequest 'V1
  -> IO (Either String (MarloweResponse 'V1))
handle config request =
  do
    let
      run =
        case request of
          ListContracts{..} -> Right . Contracts <$> allContracts reqFilter
          ListHeaders{..} -> Right . Headers <$> allHeaders reqFilter
          Get{..} -> fmap (uncurry Info) <$> getContract reqContractId
          Create{..} -> second (uncurry mkBody) <$> buildCreation MarloweV1 reqContract reqRoles reqMinUtxo (decodeMarloweTransactionMetadataLenient reqMetadata) reqAddresses reqChange reqCollateral
          Apply{..} -> second (uncurry mkBody) <$> buildApplication MarloweV1 reqContractId reqInputs reqValidityLowerBound reqValidityUpperBound (decodeMarloweTransactionMetadataLenient reqMetadata) reqAddresses reqChange reqCollateral
          Withdraw{..} -> second (uncurry mkBody) <$> buildWithdrawal MarloweV1 reqContractId reqRole reqAddresses reqChange reqCollateral
          Sign{..} -> pure . Right . uncurry Tx $ sign reqTxBody reqPaymentKeys reqPaymentExtendedKeys
          Submit{..} -> second TxId <$> submit reqTx
          Wait{..} -> second TxInfo <$> waitForTx reqPollingSeconds reqTxId
    withTimeout (timeoutSeconds config)
      $ runClientWithConfig config run
      `catch` \(err :: SomeException) -> pure . Left $ show err


withTimeout
  :: Int
  -> IO (Either String a)
  -> IO (Either String a)
withTimeout timeout action =
  fromRight (Left "Operation timed out.")
    <$> threadDelay (timeout * 1_000_000) `race` action
