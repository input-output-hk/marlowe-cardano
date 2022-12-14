

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Language.Marlowe.Runtime.Client
  ( App
  , handle
  , handleWithEvents
  ) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, catch)
import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (second)
import Language.Marlowe.Runtime.Client.Build (buildApplication, buildCreation, buildWithdrawal)
import Language.Marlowe.Runtime.Client.List
  (allContracts, followContract, followedContracts, getContract, unfollowContract)
import Language.Marlowe.Runtime.Client.Run (runClientWithConfig)
import Language.Marlowe.Runtime.Client.Sign (sign)
import Language.Marlowe.Runtime.Client.Submit (submit, waitForTx)
import Language.Marlowe.Runtime.Client.Types (Config(timeoutSeconds), MarloweRequest(..), MarloweResponse(..), mkBody)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1), MarloweVersionTag(V1))
import Observe.Event (EventBackend, addField, withEvent)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Render.JSON.Handle (JSONRef)
import Observe.Event.Syntax ((≔))

import qualified Data.Text as T (Text)


type App = ExceptT String IO


handleWithEvents
  :: EventBackend App JSONRef DynamicEventSelector
  -> Config
  -> MarloweRequest 'V1
  -> App (MarloweResponse 'V1)
handleWithEvents eventBackend config request =
  withEvent eventBackend (DynamicEventSelector "Handle")
    $ \event ->
      do
        addField event $ ("request" :: T.Text) ≔ request
        response <- ExceptT $ handle config request
        addField event $ ("response" :: T.Text) ≔ response
        pure response


handle
  :: Config
  -> MarloweRequest 'V1
  -> IO (Either String (MarloweResponse 'V1))
handle config request =
  do
    let
      run =
        case request of
          List -> Right . Contracts <$> allContracts
          Followed -> Right . Contracts <$> followedContracts
          Follow{..} -> fmap FollowResult <$> followContract reqContractId
          Unfollow{..} -> fmap FollowResult <$> unfollowContract reqContractId
          Get{..} -> fmap (uncurry Info) <$> getContract reqContractId
          Create{..} -> second (uncurry mkBody) <$> buildCreation MarloweV1 reqContract reqRoles reqMinUtxo reqAddresses reqChange reqCollateral
          Apply{..} -> second (uncurry mkBody) <$> buildApplication MarloweV1 reqContractId reqInputs reqValidityLowerBound reqValidityUpperBound reqAddresses reqChange reqCollateral
          Withdraw{..} -> second (uncurry mkBody) <$> buildWithdrawal MarloweV1 reqContractId reqRole reqAddresses reqChange reqCollateral
          Sign{..} -> pure . Right . uncurry Tx $ sign reqTransactionBody reqPaymentKeys reqPaymentExtendedKeys
          Submit{..} -> second TxId <$> submit reqTransaction
          Wait{..} -> second TxId <$> waitForTx reqTransactionId
    withTimeout (timeoutSeconds config)
      $ runClientWithConfig config run
      `catch` \(err :: SomeException) -> pure . Left $ show err


withTimeout
  :: Int
  -> IO (Either String a)
  -> IO (Either String a)
withTimeout timeout action =
  either (const $ Left "Operation timed out.") id
    <$> threadDelay (timeout * 1_000_000) `race` action
