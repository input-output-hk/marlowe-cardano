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

import Cardano.Api (AsType (AsPaymentKey), BabbageEra, CardanoMode, Key (getVerificationKey), LocalNodeConnectInfo (..),
                    NetworkId (..), ScriptDataSupportedInEra, StakeAddressReference (NoStakeAddress),
                    generateSigningKey)
import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.State.Strict (MonadState, execStateT, get)
import Language.Marlowe.CLI.Command.Template (TemplateCommand (..), initialMarloweState, makeContract)
import Language.Marlowe.CLI.Types (CliError (..), MarloweTransaction)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId)
import Marlowe.Contracts (trivial)
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
import Language.Marlowe.CLI.Run (initializeTransactionImpl, prepareTransactionImpl)
import Language.Marlowe.CLI.Test.Types
import qualified Language.Marlowe.Client as Client
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))

data ScriptState = ScriptState
  { transactions :: Map String (MarloweTransaction BabbageEra)
  , wallets      :: Map AccountId Wallet
  }

data ScriptEnv era = ScriptEnv
  { seNetworkId        :: NetworkId
  , seSlotConfig       :: SlotConfig
  , seConstModelParams :: CostModelParams
  , seEra              :: ScriptDataSupportedInEra era
  }

interpret :: MonadError CliError m
          => MonadState (ScriptState era) m
          => MonadReader (ScriptEnv era) m
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
  testContract <- case soContract of
    InlineContract contract -> pure contract
    TemplateContract templateCommand ->
      case templateCommand of
        TemplateTrivial{..} -> makeContract $ trivial
                                  party
                                  depositLovelace
                                  withdrawalLovelace
                                  timeout
        template -> throwError $ CliError $ "Template not implemented: " <> show template
        -- TemplateEscrow{..} -> makeContract $
        --                         escrow
        --                           (Constant price)
        --                           seller
        --                           buyer
        --                           mediator
        --                           paymentDeadline
        --                           complaintDeadline
        --                           disputeDeadline
        -- TemplateSwap{..} -> makeContract $
        --                       swap
        --                         aParty
        --                         aToken
        --                         (Constant aAmount)
        --                         aTimeout
        --                         bParty
        --                         bToken
        --                         (Constant bAmount)
        --                         bTimeout
        --                         Close
        -- TemplateZeroCouponBond{..} -> makeContract $
        --                                 zeroCouponBond
        --                                   lender
        --                                   borrower
        --                                   lendingDeadline
        --                                   paybackDeadline
        --                                   (Constant principal)
        --                                   (Constant principal `AddValue` Constant interest)
        --                                   ada
        --                                   Close
        -- TemplateCoveredCall{..} -> makeContract $
        --                             coveredCall
        --                               issuer
        --                               counterparty
        --                               Nothing
        --                               currency
        --                               underlying
        --                               (Constant strike)
        --                               (Constant amount)
        --                               issueDate
        --                               maturityDate
        --                               settlementDate

  transaction <- flip runReaderT (CliEnv seEra) $ initializeTransactionImpl
    marloweParams
    seSlotConfig
    seConstModelParams
    seNetworkId
    NoStakeAddress
    testContract
    marloweState
    True
    True

  modify $ insertMarloweTransaction soTransaction transaction

interpret Prepare {..} = do
  marloweTransaction <- findMarloweTransaction soTransaction
  preparedMarloweTransaction <- withError (\(CliError originalMessage) -> CliError $ originalMessage <> " - from Prepare Impl") $ prepareTransactionImpl
                                  marloweTransaction
                                  soInputs
                                  soMinimumTime
                                  soMaximumTime
                                  True
  modify $ insertMarloweTransaction soTransaction preparedMarloweTransaction

interpret CreateWallet {..} = do
  skey <- liftIO $ generateSigningKey AsPaymentKey
  let vkey = getVerificationKey skey
  let wallet = Wallet vkey skey
  modify $ insertWallet soOwner wallet

interpret (Fail message) = throwError $ CliError message

interpret _ = throwError $ CliError "Not implemented"

withError :: MonadError e m => (e -> e) -> m a -> m a
withError modifyError action = catchError action \e -> do
                                throwError $ modifyError e

insertMarloweTransaction :: TransactionNickname -> MarloweTransaction era -> ScriptState era -> ScriptState era
insertMarloweTransaction nickname transaction scriptState@ScriptState { transactions } =
  scriptState{ transactions = Map.insert nickname transaction transactions }

insertWallet :: AccountId -> Wallet -> ScriptState -> ScriptState
insertWallet ownerName wallet scriptState@ScriptState { wallets } =
  scriptState{ wallets = Map.insert ownerName wallet wallets }


-- | Find Marlowe Transaction corresponding to a transaction nickname.
findMarloweTransaction :: MonadError CliError m
                => MonadState (ScriptState era) m
                => TransactionNickname   -- ^ The nickname.
                -> m (MarloweTransaction era) -- ^ Action returning the instance.
findMarloweTransaction nickname = do
  ScriptState { transactions } <- get
  case M.lookup nickname transactions of
    Nothing -> throwError $ CliError ("[findMarloweTransaction] Marlowe Transaction was not found for nickname " <> show nickname <> ".")
    Just marloweTransaction -> pure marloweTransaction

-- | Test a Marlowe contract.
scriptTest  :: forall era m
             . MonadError CliError m
            => MonadIO m
            => ScriptDataSupportedInEra era
            -> CostModelParams
            -> NetworkId                         -- ^ The network magic.
            -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
            -> SlotConfig                        -- ^ The time and slot correspondence.
            -> ScriptTest                        -- ^ The tests to be run.
            -> m ()                              -- ^ Action for running the tests.
scriptTest era costModel networkId _ slotConfig ScriptTest{..} =
  do
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Test " <> show stTestName <> " *****"

    let
      interpretLoop = for_ stScriptOperations \operation ->
        interpret operation
    void $ catchError
      (runReaderT (execStateT interpretLoop (ScriptState mempty mempty)) (ScriptEnv networkId slotConfig costModel era))
      $ \e -> do
        -- TODO: Clean up wallets and instances.
        liftIO (print e)
        liftIO (putStrLn "***** FAILED *****")
        throwError (e :: CliError)
    liftIO $ putStrLn "***** PASSED *****"
