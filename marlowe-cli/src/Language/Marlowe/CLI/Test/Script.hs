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

import Cardano.Api (AsType (AsPaymentKey), CardanoMode, ConsensusModeParams (CardanoModeParams),
                    EpochSlots (EpochSlots), IsShelleyBasedEra, Key (getVerificationKey, verificationKeyHash),
                    LocalNodeConnectInfo (..), NetworkId (..), PaymentCredential (PaymentCredentialByKey),
                    ScriptDataSupportedInEra, StakeAddressReference (NoStakeAddress), generateSigningKey,
                    makeShelleyAddressInEra)
import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.State.Strict (MonadState, execStateT, get)
import Language.Marlowe.CLI.Command.Template (TemplateCommand (..), initialMarloweState, makeContract)
import Language.Marlowe.CLI.Types (CliEnv (..), CliError (..), MarloweTransaction, toTimeout)
import Language.Marlowe.Extended.V1 as E
import Marlowe.Contracts (coveredCall, escrow, swap, trivial, zeroCouponBond)
import Plutus.V1.Ledger.Api (CostModelParams)

import Control.Monad.RWS.Class (MonadReader)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.State (modify)
import qualified Data.Aeson as JSON
import Data.Foldable.Extra (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Marlowe.CLI.IO (liftCliMaybe)
import Language.Marlowe.CLI.Run (initializeTransactionImpl, prepareTransactionImpl)
import Language.Marlowe.CLI.Test.Types
import Language.Marlowe.CLI.Transaction (buildFaucetImpl)
import qualified Language.Marlowe.Client as Client
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))

data ScriptState era = ScriptState
  { transactions :: Map String (Maybe (MarloweTransaction era), MarloweTransaction era)
  , wallets      :: Map WalletNickname (Wallet era)
  }

data ScriptEnv era = ScriptEnv
  { seConnection       :: LocalNodeConnectInfo CardanoMode
  , seSlotConfig       :: SlotConfig
  , seConstModelParams :: CostModelParams
  , seEra              :: ScriptDataSupportedInEra era
  , seFaucet           :: Faucet era
  }

askEra :: MonadReader (ScriptEnv era) m => m (ScriptDataSupportedInEra era)
askEra = asks seEra

interpret :: IsShelleyBasedEra era
          => MonadError CliError m
          => MonadState (ScriptState era) m
          => MonadReader (ScriptEnv era) m
          => MonadIO m
          => ScriptOperation
          -> m ()
interpret FundWallet {..} = do
  ScriptEnv {..} <- ask
  let
    Faucet faucetSigningKey faucetAddress = seFaucet
  Wallet _ _ destAddress _ <- findWallet soWalletNickname

  era <- askEra
  void $ withCliErrorMsg (mappend "- from FundWallet") $ flip runReaderT (CliEnv era) $ buildFaucetImpl
    seConnection
    soValue
    [destAddress]
    faucetAddress
    faucetSigningKey
    (Just 5)

interpret Initialize {..} = do
  ScriptEnv {..} <- ask
  let
    LocalNodeConnectInfo { localNodeNetworkId } = seConnection
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
        TemplateTrivial{..} -> do timeout' <- toTimeout timeout
                                  makeContract $ trivial
                                    party
                                    depositLovelace
                                    withdrawalLovelace
                                    timeout'
        TemplateEscrow{..} -> do paymentDeadline' <- toTimeout paymentDeadline
                                 complaintDeadline' <- toTimeout complaintDeadline
                                 disputeDeadline' <- toTimeout disputeDeadline
                                 mediationDeadline' <- toTimeout mediationDeadline
                                 makeContract $ escrow
                                    (E.Constant price)
                                    seller
                                    buyer
                                    mediator
                                    paymentDeadline'
                                    complaintDeadline'
                                    disputeDeadline'
                                    mediationDeadline'
        TemplateSwap{..} -> do  aTimeout' <- toTimeout aTimeout
                                bTimeout' <- toTimeout bTimeout
                                makeContract $ swap
                                    aParty
                                    aToken
                                    (Constant aAmount)
                                    aTimeout'
                                    bParty
                                    bToken
                                    (Constant bAmount)
                                    bTimeout'
                                    Close
        TemplateZeroCouponBond{..} -> do  lendingDeadline' <- toTimeout lendingDeadline
                                          paybackDeadline' <- toTimeout paybackDeadline
                                          makeContract $
                                            zeroCouponBond
                                              lender
                                              borrower
                                              lendingDeadline'
                                              paybackDeadline'
                                              (Constant principal)
                                              (Constant principal `AddValue` Constant interest)
                                              ada
                                              Close
        TemplateCoveredCall{..} -> do issueDate' <- toTimeout issueDate
                                      maturityDate' <- toTimeout maturityDate
                                      settlementDate' <- toTimeout settlementDate
                                      makeContract $ coveredCall
                                          issuer
                                          counterparty
                                          Nothing
                                          currency
                                          underlying
                                          (Constant strike)
                                          (Constant amount)
                                          issueDate'
                                          maturityDate'
                                          settlementDate'
        template -> throwError $ CliError $ "Template not implemented: " <> show template
  liftIO $ print testContract
  transaction <- flip runReaderT (CliEnv seEra) $ initializeTransactionImpl
    marloweParams
    seSlotConfig
    seConstModelParams
    localNodeNetworkId
    NoStakeAddress
    testContract
    marloweState
    False
    True

  modify $ insertMarloweTransaction soTransaction transaction

interpret Prepare {..} = do
  (_, marloweTransaction) <- findMarloweTransaction soTransaction

  preparedMarloweTransaction <- withCliErrorMsg (mappend " - from Prepare Impl") $ prepareTransactionImpl
                                  marloweTransaction
                                  soInputs
                                  soMinimumTime
                                  soMaximumTime
                                  True
  modify $ insertMarloweTransaction soTransaction preparedMarloweTransaction

interpret CreateWallet {..} = do
  skey <- liftIO $ generateSigningKey AsPaymentKey
  ScriptEnv {..} <- ask
  let vkey = getVerificationKey skey
  let LocalNodeConnectInfo {localNodeNetworkId} = seConnection
  let
    address = makeShelleyAddressInEra localNodeNetworkId (PaymentCredentialByKey (verificationKeyHash vkey)) NoStakeAddress
  let wallet = Wallet vkey skey address (verificationKeyHash vkey)
  modify $ insertWallet soWalletNickname wallet

-- interpret Execute {..} = do
--   (prev, marloweTransaction) <- findMarloweTransaction soTransaction
--   runTransactionImpl

interpret (Fail message) = throwError $ CliError message

interpret _ = throwError $ CliError "Not implemented"

withError :: MonadError e m => (e -> e) -> m a -> m a
withError modifyError action = catchError action \e -> do
                                throwError $ modifyError e

withCliErrorMsg :: MonadError CliError m => (String -> String) -> m a -> m a
withCliErrorMsg f = withError (\(CliError msg) -> CliError (f msg))

insertMarloweTransaction :: TransactionNickname -> MarloweTransaction era -> ScriptState era -> ScriptState era
insertMarloweTransaction nickname transaction scriptState@ScriptState { transactions } = do
  let
    f (Just (_, prevTransaction)) = Just (Just prevTransaction, transaction)
    f Nothing                     = Just (Nothing, transaction)
  scriptState{ transactions = Map.alter f nickname transactions }

insertWallet :: WalletNickname -> Wallet era -> ScriptState era -> ScriptState era
insertWallet ownerName wallet scriptState@ScriptState { wallets } =
  scriptState{ wallets = Map.insert ownerName wallet wallets }


-- | Find Marlowe Transaction corresponding to a transaction nickname.
findMarloweTransaction :: MonadError CliError m
                => MonadState (ScriptState era) m
                => TransactionNickname   -- ^ The nickname.
                -> m (Maybe (MarloweTransaction era), MarloweTransaction era) -- ^ Action returning the instance.
findMarloweTransaction nickname = do
  ScriptState { transactions } <- get
  case Map.lookup nickname transactions of
    Nothing -> throwError $ CliError ("[findMarloweTransaction] Marlowe Transaction was not found for nickname " <> show nickname <> ".")
    Just t -> pure t

findWallet :: MonadError CliError m
              => MonadState (ScriptState era) m
              => WalletNickname
              -> m (Wallet era)
findWallet nickname = do
  ScriptState { wallets } <- get
  liftCliMaybe ("[findWallet] Unable to find wallet:" <> show nickname) $ Map.lookup nickname wallets

-- | Test a Marlowe contract.
scriptTest  :: forall era m
             . MonadError CliError m
            => IsShelleyBasedEra era
            => MonadIO m
            => ScriptDataSupportedInEra era
            -> CostModelParams
            -> NetworkId                         -- ^ The network magic.
            -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
            -> Faucet era
            -> SlotConfig                        -- ^ The time and slot correspondence.
            -> ScriptTest                        -- ^ The tests to be run.
            -> m ()                              -- ^ Action for running the tests.
scriptTest era costModel networkId _ faucet slotConfig ScriptTest{..} =
  do
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Test " <> show stTestName <> " *****"

    let
      interpretLoop = for_ stScriptOperations \operation ->
        interpret operation
    let
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = networkId
        , localNodeSocketPath      = "FIXME:NODESOCEKTPATH"
        }
    void $ catchError
      (runReaderT (execStateT interpretLoop (ScriptState mempty mempty)) (ScriptEnv connection slotConfig costModel era faucet))
      $ \e -> do
        -- TODO: Clean up wallets and instances.
        liftIO (print e)
        liftIO (putStrLn "***** FAILED *****")
        throwError (e :: CliError)
    liftIO $ putStrLn "***** PASSED *****"


