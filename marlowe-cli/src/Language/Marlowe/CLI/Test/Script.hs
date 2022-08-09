-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Testing Marlowe contracts without the PAB.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}


module Language.Marlowe.CLI.Test.Script where

import Cardano.Api (BabbageEra, CardanoMode, LocalNodeConnectInfo (..), NetworkId,
                    StakeAddressReference (NoStakeAddress))
import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.State.Strict (MonadState, execStateT, get)
import Language.Marlowe.CLI.Test.Types (ScriptOperation (..), ScriptTest (..), TransactionNickname)
import Language.Marlowe.CLI.Types (CliError (..), MarloweTransaction (MarloweTransaction))
import Plutus.V1.Ledger.Api (CostModelParams)

import Control.Monad.RWS.Class (MonadReader)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.State (modify)
import qualified Data.Aeson as JSON
import Data.Foldable.Extra (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M (lookup)
import Language.Marlowe.CLI.Command.Template (initialMarloweState)
import Language.Marlowe.CLI.Run (initializeTransactionImpl, prepareTransactionImpl)
import qualified Language.Marlowe.Client as Client

import Cardano.Api (CardanoMode, LocalNodeConnectInfo (..), NetworkId (..))
import Control.Monad.Except (MonadError, throwError)
import Language.Marlowe.CLI.Test.Types
import Language.Marlowe.CLI.Types (CliError (..))
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))

newtype ScriptState = ScriptState
  { transactions :: Map String (MarloweTransaction BabbageEra)
  }

data ScriptEnv = ScriptEnv
  { seNetworkId        :: NetworkId
  , seSlotConfig       :: SlotConfig
  , seConstModelParams :: CostModelParams
  }

interpret :: MonadError CliError m
          => MonadState ScriptState m
          => MonadReader ScriptEnv m
          => MonadIO m
          => ScriptOperation
          -> m ()
interpret Initialize {..} = do
  ScriptEnv {..} <- ask
  let
    roleCurrencyJson = JSON.object
      [ ( "unCurrencySymbol"
        , JSON.String soRoleCurrency
        )
      ]
  parsedRoleCurrency <- case JSON.fromJSON roleCurrencyJson of
    JSON.Error message          -> throwError $ CliError message
    JSON.Success currencySymbol -> pure currencySymbol
  let
    marloweParams = Client.marloweParams parsedRoleCurrency
    marloweState = initialMarloweState soOwner soMinAda

  transaction <- initializeTransactionImpl
    marloweParams
    seSlotConfig
    seConstModelParams
    seNetworkId
    NoStakeAddress
    soContract
    marloweState
    True
    True

  modify $ insertMarloweTransaction soTransaction transaction

interpret Prepare {..} = do
  marloweTransaction <- findMarloweTransaction soTransaction
  preparedMarloweTransaction <- prepareTransactionImpl
                                  marloweTransaction
                                  soInputs
                                  soMinimumTime
                                  soMaximumTime
                                  True
  modify $ insertMarloweTransaction soTransaction preparedMarloweTransaction

interpret (Fail message) = throwError $ CliError message

insertMarloweTransaction :: TransactionNickname -> MarloweTransaction BabbageEra -> ScriptState -> ScriptState
insertMarloweTransaction nickname transaction scriptState@ScriptState { transactions } =
  scriptState{ transactions = Map.insert nickname transaction transactions }

-- | Find Marlowe Transaction corresponding to a transaction nickname.
findMarloweTransaction :: MonadError CliError m
                => MonadState ScriptState m
                => TransactionNickname   -- ^ The nickname.
                -> m (MarloweTransaction BabbageEra) -- ^ Action returning the instance.
findMarloweTransaction nickname = do
  ScriptState { transactions } <- get
  case M.lookup nickname transactions of
    Nothing -> throwError $ CliError ("[findMarloweTransaction] Marlowe Transaction was not found for nickname " <> show nickname <> ".")
    Just marloweTransaction -> pure marloweTransaction

-- | Test a Marlowe contract.
scriptTest  :: MonadError CliError m
            => MonadIO m
            => CostModelParams
            -> NetworkId                         -- ^ The network magic.
            -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
            -> SlotConfig                        -- ^ The time and slot correspondence.
            -> ScriptTest                        -- ^ The tests to be run.
            -> m ()                              -- ^ Action for running the tests.
scriptTest costModel networkId connection slotConfig ScriptTest{..} =
  do
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Test " <> show stTestName <> " *****"

    let
      interpretLoop :: MonadError CliError m
                    => MonadState ScriptState m
                    => MonadReader ScriptEnv m
                    => MonadIO m
                    => m ()
      interpretLoop = for_ stScriptOperations \operation -> do
        interpret operation
    void $ catchError
      ( runReaderT (execStateT interpretLoop (ScriptState mempty)) (ScriptEnv networkId slotConfig costModel))
      $ \e -> do
        -- TODO: Clean up wallets and instances.
        liftIO (putStrLn $ show e)
        liftIO (putStrLn "***** FAILED *****")
        throwError (e :: CliError)
    liftIO $ putStrLn "***** PASSED *****"




