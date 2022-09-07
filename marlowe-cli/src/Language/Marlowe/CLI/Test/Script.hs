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
{-# LANGUAGE ViewPatterns       #-}


module Language.Marlowe.CLI.Test.Script where

import Cardano.Api (AsType (AsPaymentKey), CardanoMode, IsShelleyBasedEra,
                    Key (getVerificationKey, verificationKeyHash), LocalNodeConnectInfo (..),
                    PaymentCredential (PaymentCredentialByKey), ScriptDataSupportedInEra,
                    StakeAddressReference (NoStakeAddress), generateSigningKey, makeShelleyAddressInEra)
import Control.Monad (foldM, forM, forM_, void, when)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftIO, throwError)
import Control.Monad.State.Strict (MonadState, execStateT)
import Language.Marlowe.CLI.Command.Template (initialMarloweState, makeContract)
import Language.Marlowe.CLI.Types (CliEnv (CliEnv), CliError (..), MarlowePlutusVersion, MarloweTransaction,
                                   PrintStats (PrintStats), PublishingStrategy (PublishPermanently), toTimeout)
import Language.Marlowe.Extended.V1 as E (ChoiceId (ChoiceId), Party)
import Marlowe.Contracts (trivial)
import Plutus.V1.Ledger.Api (CostModelParams, TokenName)

import qualified Cardano.Api as C
import Control.Lens (modifying, use, view)
import Control.Monad.Extra (whenM)
import Control.Monad.RWS.Class (MonadReader)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.OneLine as A
import Data.Foldable (Foldable (fold), find, foldl')
import Data.Foldable.Extra (for_)
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S (singleton)
import qualified Data.Text as Text
import Data.Traversable (for)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import qualified Language.Marlowe.CLI.Data.Aeson.Traversals as A
import Language.Marlowe.CLI.IO (liftCliMaybe, queryInEra)
import Language.Marlowe.CLI.Run (initializeTransactionImpl, prepareTransactionImpl, runTransactionImpl)
import Language.Marlowe.CLI.Sync (classifyOutputs, isMarloweOut)
import Language.Marlowe.CLI.Sync.Types (MarloweOut (ApplicationOut, moTxIn))
import Language.Marlowe.CLI.Test.Types (AUTxO (AUTxO, unAUTxO), ContractNickname, ContractSource (..), CurrencyNickname,
                                        CustomCurrency (CustomCurrency, ccCurrencySymbol), MarloweContract (..),
                                        PartyRef (RoleRef, WalletRef), ScriptEnv (..), ScriptOperation (..),
                                        ScriptState, ScriptTest (ScriptTest, stScriptOperations, stTestName),
                                        TokenAssignment (TokenAssignment), UseTemplate (..), Wallet (..),
                                        WalletNickname (WalletNickname),
                                        WalletTransaction (WalletTransaction, wtFees, wtTxBody), anyMarloweThread,
                                        faucetNickname, foldrMarloweThread, fromUTxO, getMarloweThreadTransaction,
                                        getMarloweThreadTxIn, overAnyMarloweThread, scriptState, seConnection,
                                        seCostModelParams, seEra, seProtocolVersion, seSlotConfig, ssContracts,
                                        ssCurrencies, ssWallets, walletPubKeyHash)
import Language.Marlowe.CLI.Transaction (buildFaucetImpl, buildMintingImpl, publishImpl, selectUtxosImpl)
import qualified Language.Marlowe.CLI.Types as T
import qualified Language.Marlowe.Client as Client
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import Ledger.Tx.CardanoAPI (fromCardanoPaymentKeyHash, fromCardanoPolicyId)
import qualified Ledger.Tx.CardanoAPI as L
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))
import Plutus.V1.Ledger.Value (mpsSymbol, valueOf)
import qualified Plutus.V1.Ledger.Value as P
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api as P


-- FIXME: Drop this when proper coin selection is in place.
minCollateral :: C.Lovelace
minCollateral = C.Lovelace 10_000_000


transactionTimeout :: Int
transactionTimeout = 30


interpret :: forall era m
          .  IsShelleyBasedEra era
          => MonadError CliError m
          => MonadState (ScriptState MarlowePlutusVersion era) m
          => MonadReader (ScriptEnv era) m
          => MonadIO m
          => ScriptOperation
          -> m ()
interpret CreateWallet {..} = do
  skey <- liftIO $ generateSigningKey AsPaymentKey
  let vkey = getVerificationKey skey
  connection <- view seConnection
  let LocalNodeConnectInfo {localNodeNetworkId} = connection
  let
    address = makeShelleyAddressInEra localNodeNetworkId (PaymentCredentialByKey (verificationKeyHash vkey)) NoStakeAddress
  let wallet = Wallet address (Left skey) mempty mempty vkey
  ssWallets `modifying` Map.insert soWalletNickname wallet

interpret FundWallet {..} = do
  let
    -- Let's create a separate UTxO with pretty large collateral by default
    value = C.lovelaceToValue soValue
    values = case soCreateCollateral of
      Just True ->
        [ value <> C.negateValue (C.lovelaceToValue minCollateral)
        , C.lovelaceToValue minCollateral
        ]
      _ -> [ value ]

  (Wallet faucetAddress faucetSigningKey _ _ _) <- getFaucet
  (Wallet address _ _ _ _) <- findWallet soWalletNickname
  connection <- view seConnection
  txBody <- runCli "[FundWallet] " $ buildFaucetImpl
    connection
    values
    [address]
    faucetAddress
    faucetSigningKey
    (Just transactionTimeout)       -- FIXME: make this part of test env setup (--minting-timeout or --transaction-timeout)

  let
    transaction = WalletTransaction { wtFees = 0, wtTxBody=txBody  }

  updateFaucet \faucet@(Wallet _ _ _ faucetTransactions _) ->
    faucet { waTransactions = transaction : faucetTransactions }

interpret Mint {..} = do
  currencies <- use ssCurrencies
  when (isJust $ Map.lookup soCurrencyNickname currencies) do
    throwError "Currency with a given nickname already exist"
  Wallet faucetAddress faucetSigningKey _ _ _ <- getFaucet
  (tokenDistribution, walletAssignemnts) <- unzip <$>forM soTokenDistribution \(TokenAssignment amount tokenName owner) -> do
    let
      nickname = case owner of
        Just wn -> wn
        Nothing -> WalletNickname $ show tokenName
    wallet@(Wallet destAddress _ _ _ _) <- findWallet nickname
    pure ((tokenName, amount, Just destAddress), (nickname, wallet, tokenName, amount))
  connection <- view seConnection
  (_, policy) <- runCli "[Mint] " $ buildMintingImpl
    connection
    faucetSigningKey
    tokenDistribution
    soMetadata
    Nothing
    2_000_000       -- FIXME: should we compute minAda here?
    faucetAddress
    (Just transactionTimeout)       -- FIXME: make this part of test env setup (--minting-timeout or --transaction-timeout)

  let
    currencySymbol = mpsSymbol . fromCardanoPolicyId $ policy
    currency = CustomCurrency policy currencySymbol

  forM_ walletAssignemnts \(nickname, wallet@(Wallet _ _ tokens _ _), tokenName, amount) -> do
    let
      value = Value.singleton currencySymbol tokenName amount
    ssWallets `modifying` Map.insert nickname (wallet { waTokens = value <> tokens })

  ssCurrencies `modifying` Map.insert soCurrencyNickname currency

interpret Initialize {..} = do
  marloweContract <- case soContractSource of
    InlineContract json -> decodeContractJSON json
    UseTemplate setup   -> useTemplate soRoleCurrency setup

  liftIO $ print marloweContract

  faucetParty <- do
    Wallet _ _ _ _ faucetVerificationKey <- getFaucet
    let
      pubKeyHash = fromCardanoPaymentKeyHash . verificationKeyHash $ faucetVerificationKey
    pure $ M.PK pubKeyHash

  currencySymbol <- case soRoleCurrency of
    Nothing -> pure P.adaSymbol
    Just nickname -> do
      CustomCurrency { ccCurrencySymbol } <- findCurrency nickname
      pure ccCurrencySymbol

  let
    marloweState = initialMarloweState faucetParty soMinAda
    marloweParams = Client.marloweParams currencySymbol

  slotConfig <- view seSlotConfig
  costModelParams <- view seCostModelParams
  protocolVersion <- view seProtocolVersion
  LocalNodeConnectInfo { localNodeNetworkId } <- view seConnection
  marloweTransaction <- runCli "[Initialize] " $ initializeTransactionImpl
    marloweParams
    slotConfig
    protocolVersion
    costModelParams
    localNodeNetworkId
    NoStakeAddress
    marloweContract
    marloweState
    False
    True

  whenM (isJust . Map.lookup soContractNickname <$> use ssContracts) do
    throwError "[Initialize] Contract with a given nickname already exist."

  modifying ssContracts $ Map.insert soContractNickname $ MarloweContract
    {
      mcContract = marloweContract
    , mcPlan = marloweTransaction :| []
    , mcThread = Nothing
    , mcCurrency = soRoleCurrency
    }

interpret Prepare {..} = do
  marloweContract@MarloweContract {..} <- findMarloweContract soContractNickname

  let
    curr = NE.head mcPlan
  inputs <- for soInputs \input -> decodeInputJSON input
  new <- runCli "[Prepare] " $ prepareTransactionImpl
    curr
    inputs
    soMinimumTime
    soMaximumTime
    True

  let
    plan = new <| mcPlan
    marloweContract' = marloweContract{ mcPlan = plan }

  modifying ssContracts $ Map.insert soContractNickname marloweContract'

interpret AutoRun {..} = do
  marloweContract@MarloweContract {..} <- findMarloweContract soContractNickname
  let
    plan = do
      let
        whole = reverse $ NE.toList mcPlan
      case mcThread of
        Just thread -> do
          let
            l = overAnyMarloweThread (foldrMarloweThread (const (+ 1)) 0) thread
          drop l whole
        Nothing -> whole
    step th mt = do
      let
        prev :: Maybe (MarloweTransaction MarlowePlutusVersion era, C.TxIn)
        prev = do
          pmt <- overAnyMarloweThread getMarloweThreadTransaction <$> th
          txIn <- overAnyMarloweThread getMarloweThreadTxIn =<< th
          pure (pmt, txIn)

      (txBody, mTxIn) <- autoRunTransaction mcCurrency prev mt
      case anyMarloweThread mt txBody mTxIn th of
        Just th' -> pure $ Just th'
        Nothing  -> throwError "[AutoRun] Extending of the marlowe thread failed."

  thread' <- foldM step mcThread plan
  let
    marloweContract' = marloweContract { mcThread = thread' }
  ssContracts `modifying`  Map.insert soContractNickname marloweContract'

interpret Publish {..} = do
  Wallet { waAddress, waSigningKey } <- maybe getFaucet findWallet soPublisher

  connection <- view seConnection
  txBody <- runCli "[Publish]" $ publishImpl
    connection
    waSigningKey
    Nothing
    waAddress
    (PublishPermanently NoStakeAddress)
    transactionTimeout
    (PrintStats True)

  liftIO $ print txBody

interpret FindPublished {} = throwError $ CliError "[FindPublished] Not implemented yet."


interpret (Fail message) = throwError $ CliError message


autoRunTransaction :: forall era lang m
                    . IsPlutusScriptLanguage lang
                   => MonadError CliError m
                   => MonadReader (ScriptEnv era) m
                   => MonadState (ScriptState lang era) m
                   => MonadIO m
                   => Maybe CurrencyNickname
                   -> Maybe (MarloweTransaction lang era, C.TxIn)
                   -> MarloweTransaction lang era
                   -> m (C.TxBody era, Maybe C.TxIn)
autoRunTransaction currency prev curr = do
  let
    T.MarloweTransaction { mtInputs } = curr
    getNormalInputParty = \case
      M.IDeposit _ party _ _         -> Just party
      M.IChoice (ChoiceId _ party) _ -> Just party
      M.INotify                      -> Nothing

    getInputParty :: Maybe Party -> M.Input -> m (Maybe Party)
    getInputParty reqParty (M.NormalInput input) = case getNormalInputParty input of
      Nothing -> pure reqParty
      reqParty' | reqParty /= reqParty' ->
        throwError "[autoRunTransaction] can handle only inputs which can be executed by a single party."
      _ -> pure reqParty
    getInputParty _ M.MerkleizedInput {} =
      throwError "[autoRunTransaction] merkleized input handling is not implemented yet."

    getInputDepositAmount = \case
      M.NormalInput (M.IDeposit _ _ (M.Token symbol name) amount) -> pure $ P.singleton symbol name amount
      M.NormalInput M.IChoice {} -> pure mempty
      M.NormalInput M.INotify -> pure mempty
      M.MerkleizedInput {} ->
        throwError "[autoRunTransaction] merkleized input handling is not implemented yet."

  (_, wallet@(Wallet address skey _ _ _)) <- foldM getInputParty Nothing mtInputs >>= \case
    Nothing          -> (faucetNickname,) <$> getFaucet
    Just (M.PK pkh)  -> findWalletByPkh pkh
    Just (M.Role rn) -> case currency of
      Just cn -> findWalletByUniqueToken cn rn
      Nothing -> throwError "[autoRunTransaction] Contract requires a role currency which was not specified."

  (marloweInBundle, utxos) <- case prev of
      Just (mt, mTxIn) -> do
        (AUTxO (cTxIn, _) :| cs, rest) <- ensureWalletCollaterals (Right wallet)
        pure (Just (mt, mTxIn, cTxIn), rest <> cs)
      Nothing -> do
        inputs <- getWalletUTxO (Right wallet)
        pure (Nothing, fromUTxO inputs)

  requiredDeposit <- fold <$> for mtInputs getInputDepositAmount

  inputs <- do
    let
      txOutValue (unAUTxO -> (_, C.TxOut _ value _ _)) = L.fromCardanoValue (C.txOutValueToValue value)
      check candidates = P.geq (foldMap txOutValue candidates) requiredDeposit
    case find check (inits utxos) of
      Nothing  -> throwError "Unable to cover the deposit"
      Just txs -> pure $ map (fst . unAUTxO) txs

  connection <- view seConnection
  txBody <- runCli "[AutoRun] " $ runTransactionImpl
      connection
      marloweInBundle
      curr
      inputs
      []
      address
      [skey]
      C.TxMetadataNone
      (Just 30)       -- FIXME: make this part of test env setup (--minting-timeout or --transaction-timeout)
      True
      False

  let
    C.TxBody C.TxBodyContent{..} = txBody
    mTxId = C.getTxId txBody

  case classifyOutputs mTxId txOuts of
    Right meOuts -> case filter isMarloweOut meOuts of
      [ApplicationOut {moTxIn}] -> pure (txBody, Just moTxIn)
      []                        -> pure (txBody, Nothing)
      _                         -> throwError "[AutoRun] Multiple Marlowe outputs detected - unable to handle them yet."
    Left e -> throwError . CliError $ "[AutoRun] Marlowe output anomaly: " <> show e


-- This helper is present in the newer version of mtl
withError :: MonadError e m => (e -> e) -> m a -> m a
withError modifyError action = catchError action \e -> do
                                throwError $ modifyError e

withCliErrorMsg :: MonadError CliError m => (String -> String) -> m a -> m a
withCliErrorMsg f = withError (\(CliError msg) -> CliError (f msg))


findMarloweContract :: MonadError CliError m
                => MonadState (ScriptState lang era) m
                => ContractNickname   -- ^ The nickname.
                -> m (MarloweContract lang era)
findMarloweContract nickname = do
  contracts <- use ssContracts
  liftCliMaybe
    ("[findMarloweContract] Marlowe contract structure was not found for a given nickname " <> show nickname <> ".")
    $ Map.lookup nickname contracts


useTemplate :: MonadError CliError m
            => MonadState (ScriptState lang era) m
            => MonadIO m
            => Maybe CurrencyNickname
            -> UseTemplate
            -> m M.Contract
useTemplate currency = do
  \case
    UseTrivial{..} -> do
      timeout' <- toTimeout utTimeout
      let
        partyRef = fromMaybe (WalletRef faucetNickname) utParty
      party <- buildParty currency partyRef
      makeContract $ trivial
        party
        utDepositLovelace
        utWithdrawalLovelace
        timeout'
    --UseEscrow{..} -> do paymentDeadline' <- toTimeout paymentDeadline
    --                    complaintDeadline' <- toTimeout complaintDeadline
    --                    disputeDeadline' <- toTimeout disputeDeadline
    --                    mediationDeadline' <- toTimeout mediationDeadline
    --                    makeContract $ escrow
    --                       (E.Constant price)
    --                       seller
    --                       buyer
    --                       mediator
    --                       paymentDeadline'
    --                       complaintDeadline'
    --                       disputeDeadline'
    --                       mediationDeadline'
    --UseSwap{..} -> do  aTimeout' <- toTimeout aTimeout
    --                   bTimeout' <- toTimeout bTimeout
    --                   makeContract $ swap
    --                       aParty
    --                       aToken
    --                       (Constant aAmount)
    --                       aTimeout'
    --                       bParty
    --                       bToken
    --                       (Constant bAmount)
    --                       bTimeout'
    --                       Close
    --UseZeroCouponBond{..} -> do  lendingDeadline' <- toTimeout lendingDeadline
    --                             paybackDeadline' <- toTimeout paybackDeadline
    --                             makeContract $
    --                               zeroCouponBond
    --                                 lender
    --                                 borrower
    --                                 lendingDeadline'
    --                                 paybackDeadline'
    --                                 (Constant principal)
    --                                 (Constant principal `AddValue` Constant interest)
    --                                 ada
    --                                 Close
    --UseCoveredCall{..} -> do issueDate' <- toTimeout issueDate
    --                         maturityDate' <- toTimeout maturityDate
    --                         settlementDate' <- toTimeout settlementDate
    --                         makeContract $ coveredCall
    --                             issuer
    --                             counterparty
    --                             Nothing
    --                             currency
    --                             underlying
    --                             (Constant strike)
    --                             (Constant amount)
    --                             issueDate'
    --                             maturityDate'
    --                             settlementDate'
    template -> throwError $ CliError $ "Template not implemented: " <> show template


buildParty :: (MonadState (ScriptState lang era) m, MonadError CliError m) => Maybe CurrencyNickname -> PartyRef -> m Party
buildParty currencyNickname = \case
  WalletRef nickname -> do
      wallet <- findWallet nickname
      pure $ M.PK . walletPubKeyHash $ wallet
  RoleRef token -> do
    -- Cosistency check
    currency <- case currencyNickname of
      Nothing -> fst <$> getCurrency
      Just cn -> pure cn
    void $ findWalletByUniqueToken currency token
    -- We are allowed to use this M.Role
    pure $ M.Role token


findWallet :: MonadError CliError m
              => MonadState (ScriptState lang era) m
              => WalletNickname
              -> m (Wallet era)
findWallet nickname = do
  wallets <- use ssWallets
  liftCliMaybe ("[findWallet] Unable to find wallet:" <> show nickname) $ Map.lookup nickname wallets

updateWallet :: MonadError CliError m
             => MonadState (ScriptState lang era) m
             => WalletNickname
             -> (Wallet era -> Wallet era)
             -> m ()
updateWallet nickname update = do
  wallet <- findWallet nickname
  let
    wallet' = update wallet
  modifying ssWallets $ Map.insert nickname wallet'

getFaucet :: MonadError CliError m
          => MonadState (ScriptState lang era) m
          => m (Wallet era)
getFaucet = do
  findWallet faucetNickname

updateFaucet :: MonadError CliError m
             => MonadState (ScriptState lang era) m
             => (Wallet era -> Wallet era)
             -> m ()
updateFaucet update = do
  updateWallet faucetNickname update


findWalletByUniqueToken :: MonadError CliError m
                        => MonadState (ScriptState lang era) m
                        => CurrencyNickname
                        -> TokenName
                        -> m (WalletNickname, Wallet era)
findWalletByUniqueToken currencyNickname tokenName = do
  CustomCurrency {..} <- findCurrency currencyNickname
  let
    check value = valueOf value ccCurrencySymbol tokenName == 1
    step Nothing (n, wallet@(Wallet _ _ tokens _ _)) = pure $ if check tokens
      then Just (n, wallet)
      else Nothing
    step res c@(n, Wallet _ _ tokens _ _) = if check tokens
      then case res of
        Just (n', _) ->
          throwError $ CliError $ "[findByUniqueToken] Token is not unique - found in two wallets: " <> show n <> " and " <> show n' <> "."
        Nothing ->
          pure (Just c)
      else pure res
  wallets <- use ssWallets
  walletInfo <- foldM step Nothing (Map.toList wallets)
  liftCliMaybe
    ("[findWalletByUniqueToken] Wallet not found for a given token: " <> show ccCurrencySymbol <> ":" <> show tokenName)
    walletInfo


findWalletByPkh :: MonadError CliError m
                => MonadState (ScriptState lang era) m
                => P.PubKeyHash
                -> m (WalletNickname, Wallet era)
findWalletByPkh pkh = do
  wallets <- use ssWallets
  let
    step _ (n, w) = if pkh == walletPubKeyHash w
      then Just (n, w)
      else Nothing
    wallet = foldl' step Nothing (Map.toList wallets)

  liftCliMaybe
    ("[findWalletByPkh] Wallet not found for a given pkh: " <> show pkh)
    wallet


runCli :: MonadError CliError m
       => MonadReader (ScriptEnv era) m
       => String
       -> ReaderT (CliEnv era) m a
       -> m a
runCli msg action = do
  era <- view seEra
  withCliErrorMsg (mappend msg) $ runReaderT action (CliEnv era)


getCurrency :: MonadError CliError m
                => MonadState (ScriptState lang era) m
                => m (CurrencyNickname, CustomCurrency)
getCurrency = do
  currencies <- use ssCurrencies
  case Map.toList currencies of
    [c] -> pure c
    _   -> throwError "Ambigious currency lookup."

findCurrency :: (MonadState (ScriptState lang era) m, MonadError CliError m) => CurrencyNickname -> m CustomCurrency
findCurrency nickname = do
  currencies <- use ssCurrencies
  liftCliMaybe ("[findWallet] Unable to find currency:" <> show nickname) $ Map.lookup nickname currencies


selectWalletUTxOs :: (MonadIO m, MonadReader (ScriptEnv era) m, MonadState (ScriptState lang era) m, MonadError CliError m) => Either WalletNickname (Wallet era) -> T.OutputQuery -> m (T.OutputQueryResult era)
selectWalletUTxOs w q = do
  Wallet address _ _ _ _ <- either findWallet pure w
  connection <- view seConnection
  runCli "[selectUtxosImpl]" $ selectUtxosImpl connection address q

getWalletUTxO :: (MonadIO m, MonadReader (ScriptEnv era) m, MonadState (ScriptState lang era) m, MonadError CliError m) => Either WalletNickname (Wallet era) -> m (C.UTxO era)
getWalletUTxO w = do
  Wallet address _ _ _ _ <- either findWallet pure w
  connection <- view seConnection
  runCli "[getWalletUTxO]" $ queryInEra connection
    . C.QueryUTxO
    . C.QueryUTxOByAddress
    . S.singleton
    . T.toAddressAny'
    $ address

-- | Test a Marlowe contract.
scriptTest  :: forall era m
             . MonadError CliError m
            => IsShelleyBasedEra era
            => MonadIO m
            => ScriptDataSupportedInEra era
            -> ProtocolVersion
            -> CostModelParams
            -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
            -> Wallet era                        -- ^ Wallet which should be used as faucet.
            -> SlotConfig                        -- ^ The time and slot correspondence.
            -> ScriptTest                        -- ^ The tests to be run.
            -> m ()                              -- ^ Action for running the tests.
scriptTest era protocolVersion costModel connection faucet slotConfig ScriptTest{..} =
  do
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Test " <> show stTestName <> " *****"

    let
      interpretLoop = for_ stScriptOperations \operation ->
        interpret operation
    void $ catchError
      (runReaderT (execStateT interpretLoop (scriptState faucet)) (ScriptEnv connection costModel era protocolVersion slotConfig))
      $ \e -> do
        -- TODO: Clean up wallets and instances.
        liftIO (print e)
        liftIO (putStrLn "***** FAILED *****")
        throwError (e :: CliError)
    liftIO $ putStrLn "***** PASSED *****"

rewritePartyRefs :: (MonadIO m, MonadState (ScriptState lang era) m, MonadError CliError m) => A.Value -> m A.Value
rewritePartyRefs = A.rewriteBottomUp rewrite
  where
    rewrite = \case
      A.Object (KeyMap.toList -> [("pk_hash", A.String walletNickname)]) -> do
        wallet <- findWallet (WalletNickname $ Text.unpack walletNickname)
        let
          pkh = walletPubKeyHash wallet
        pure $ A.toJSON (M.PK pkh)
      v -> do
        pure v


decodeContractJSON :: MonadIO m => MonadState (ScriptState lang era) m => MonadError CliError m => A.Value -> m M.Contract
decodeContractJSON json = do
  contractJSON <- rewritePartyRefs json
  case A.fromJSON contractJSON of
    A.Error err -> throwError . CliError $ "[decodeContractJSON] contract json (" <> Text.unpack (A.renderValue json) <> ") parsing error: " <> show err
    A.Success contract -> pure contract


decodeInputJSON :: MonadIO m => MonadState (ScriptState lang era) m => MonadError CliError m => A.Value -> m M.Input
decodeInputJSON json = do
  contractJSON <- rewritePartyRefs json
  case A.fromJSON contractJSON of
    A.Error err -> throwError . CliError $ "[decodeInputJSON] contract input json (" <> Text.unpack (A.renderValue json) <> ") parsing error: " <> show err
    A.Success input -> pure input


createCollaterals :: MonadReader (ScriptEnv era) m => MonadIO m => MonadState (ScriptState lang era) m => MonadError CliError m => Int -> Either WalletNickname (Wallet era) -> m (NonEmpty (AUTxO era), [AUTxO era])
createCollaterals number wallet = do
  Wallet address skey _ _ _ <- either findWallet pure wallet
  connection <- view seConnection
  let
    outputs = replicate number (C.lovelaceToValue minCollateral)

  void $ runCli "[createCollaterals] " $ buildFaucetImpl
    connection
    outputs
    [address]
    address
    skey
    (Just 30)       -- FIXME: make this part of test env setup (--minting-timeout or --transaction-timeout)

  possibleCollaterals <- selectWalletUTxOs wallet (T.LovelaceOnly ((==) minCollateral))

  case possibleCollaterals of
    (T.OutputQueryResult (fromUTxO -> c:cs) (fromUTxO -> rest)) ->
      pure (c:|cs, rest)
    _ ->
      throwError "[createCollaterals] Unable to find collaterals which were just created..."


ensureWalletCollaterals :: MonadIO m => MonadReader (ScriptEnv era) m => MonadState (ScriptState lang era) m => MonadError CliError m => Either WalletNickname (Wallet era) -> m (NonEmpty (AUTxO era), [AUTxO era])
ensureWalletCollaterals wallet = do
  possibleCollaterals <- selectWalletUTxOs wallet (T.LovelaceOnly ((==) minCollateral))
  case possibleCollaterals of
    T.OutputQueryResult (fromUTxO -> c:cs) (fromUTxO -> rest) ->
      pure (c:|cs, rest)
    _ -> createCollaterals 2 wallet
