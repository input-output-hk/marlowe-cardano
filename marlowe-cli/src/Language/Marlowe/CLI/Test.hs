-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Testing Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Language.Marlowe.CLI.Test (
-- * Testing
  runTests
) where


import Cardano.Api (ConsensusModeParams (CardanoModeParams), EpochSlots (..), IsShelleyBasedEra,
                    Key (getVerificationKey, verificationKeyHash), LocalNodeConnectInfo (..), ScriptDataSupportedInEra)
import Control.Lens (Bifunctor (bimap))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Language.Marlowe.CLI.IO (decodeFileStrict, readSigningKey)
import Language.Marlowe.CLI.Test.Script (scriptTest)
import Language.Marlowe.CLI.Test.Types (MarloweTests (..), Wallet (Wallet))
import Language.Marlowe.CLI.Transaction (querySlotConfig)
import Language.Marlowe.CLI.Types (CliEnv (CliEnv), CliError (..))
import qualified Language.Marlowe.CLI.Types as T
import PlutusCore (defaultCostModelParams)


-- | Run tests of a Marlowe contract.
runTests :: IsShelleyBasedEra era
         => MonadError CliError m
         => MonadIO m
         => ScriptDataSupportedInEra era
         -> MarloweTests era FilePath  -- ^ The tests.
         -> m ()                   -- ^ Action for running the tests.
runTests era ScriptTests{..} =
  do
    costModel <-
      maybe
        (throwError $ CliError "Missing default cost model.")
        pure
        defaultCostModelParams
    let
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network
        , localNodeSocketPath      = socketPath
        }
    faucetSigningKey <- readSigningKey faucetSigningKeyFile
    let
      vkey = T.toPaymentVerificationKey . T.getVerificationKey $ faucetSigningKey
      faucet = Wallet faucetAddress faucetSigningKey mempty mempty vkey

    slotConfig <- runReaderT (querySlotConfig connection) $ CliEnv era
    tests' <- mapM decodeFileStrict tests
    mapM_ (scriptTest era costModel connection faucet slotConfig) tests'

