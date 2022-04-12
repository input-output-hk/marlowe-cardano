-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test Marlowe contracts using the PAB.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}


module Language.Marlowe.CLI.Test.PAB (
-- * Testing
  pabTest
) where


import Cardano.Api (AddressAny (..), AddressInEra (..), AlonzoEra, AsType (AsAddress, AsScriptHash, AsShelleyAddr),
                    AssetId (..), AssetName (..), LocalNodeConnectInfo (..), PolicyId (..), Quantity (..), ShelleyEra,
                    StakeAddressReference (NoStakeAddress), anyAddressInShelleyBasedEra, deserialiseAddress,
                    deserialiseFromRawBytes, lovelaceToValue, negateValue, quantityToLovelace, selectLovelace,
                    serialiseAddress, toAddressAny, valueFromList, valueToList)
import Cardano.Api.Shelley (shelleyPayAddrToPlutusPubKHash)
import Cardano.Mnemonic (SomeMnemonic (..), mnemonicToText)
import Cardano.Wallet.Api (GetTransaction, MigrateShelleyWallet)
import Cardano.Wallet.Api.Client (addressClient, getWallet, listAddresses, postWallet, walletClient)
import Cardano.Wallet.Api.Types (ApiMnemonicT (..), ApiT (..), ApiTransaction (..), ApiTxId (..), ApiWallet (..),
                                 ApiWalletAssetsBalance (ApiWalletAssetsBalance), ApiWalletBalance (ApiWalletBalance),
                                 ApiWalletMigrationPostData (..), WalletOrAccountPostData (..), WalletPostData (..))
import Cardano.Wallet.Gen (genMnemonic)
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (..), Passphrase (..))
import Cardano.Wallet.Primitive.SyncProgress (SyncProgress (Ready))
import Cardano.Wallet.Primitive.Types (WalletName (..))
import Cardano.Wallet.Primitive.Types.TokenPolicy (TokenName (UnsafeTokenName), TokenPolicyId (UnsafeTokenPolicyId))
import Cardano.Wallet.Primitive.Types.TokenQuantity (TokenQuantity (TokenQuantity))
import Cardano.Wallet.Shelley.Compatibility (fromCardanoAddress)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, catch, displayException)
import Control.Lens (use, (%=))
import Control.Monad (unless, void, when)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, catchError, liftIO, runExceptT, throwError)
import Control.Monad.Extra (untilJustM, whenJust)
import Control.Monad.State.Strict (MonadState, StateT, execStateT, get, lift, put)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Null, Object, String))
import Data.Aeson.OneLine (renderValue)
import Data.Aeson.Types (parseEither)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.CLI.Export (buildAddress, buildRoleAddress)
import Language.Marlowe.CLI.IO (liftCli, liftCliIO, liftCliMaybe)
import Language.Marlowe.CLI.PAB (receiveStatus)
import Language.Marlowe.CLI.Test.Types (AppInstanceInfo (..), FollowerInstanceInfo (..), InstanceNickname,
                                        PabAccess (..), PabOperation (..), PabState (..), PabTest (..),
                                        PatternJSON (Exact, Parts), RoleName, WalletInfo (..), psAppInstances,
                                        psBurnAddress, psFaucetAddress, psFaucetKey, psFollowerInstances, psPassphrase,
                                        psWallets)
import Language.Marlowe.CLI.Transaction (buildFaucet, queryUtxos)
import Language.Marlowe.CLI.Types (CliError (..), SomePaymentSigningKey)
import Language.Marlowe.Client (ApplyInputsEndpointSchema, AutoEndpointSchema, CreateEndpointSchema,
                                EndpointResponse (..), MarloweEndpointResult (..), RedeemEndpointSchema)
import Language.Marlowe.Contract (MarloweContract (..))
import Language.Marlowe.Semantics (MarloweParams (rolesCurrency))
import Language.Marlowe.SemanticsTypes (Party (Role))
import Network.WebSockets (Connection)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (InstanceClient (..), PabClient (PabClient, activateContract, instanceClient))
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..), InstanceStatusToClient (..))
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..), fromBuiltin, toBuiltin)
import Plutus.V1.Ledger.Time (DiffMilliSeconds (..), POSIXTime (..))
import Servant.API ((:>))
import System.Timeout (timeout)
import Test.QuickCheck (generate)
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))

import qualified Cardano.Api as C (Value)
import qualified Cardano.Wallet.Api.Types as W (ApiWallet (id, state))
import qualified Cardano.Wallet.Primitive.Types as W (WalletId (..))
import qualified Cardano.Wallet.Primitive.Types.Hash as W (Hash (Hash))
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W (AssetId (AssetId), toFlatList)
import qualified Data.Aeson as A (Value (..))
import qualified Data.ByteArray as BA (pack)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.HashMap.Strict as H (foldrWithKey, lookup)
import qualified Data.Map.Strict as M (adjust, insert, lookup)
import qualified Data.Quantity as W (Quantity (..))
import qualified Data.Text as T (pack, unpack)
import qualified Data.Time.Clock.POSIX as Time (getPOSIXTime)
import qualified Data.Vector as V (all, zip)
import qualified PlutusTx.AssocMap as AM (fromList)
import qualified Servant.Client as Servant (client)


-- | Whether to report verbosely.
verbose :: Bool
verbose = True


-- | Test a Marlowe contract on the PAB.
pabTest :: MonadError CliError m
        => MonadIO m
        => PabAccess              -- ^ Access to the PAB APIs.
        -> SomePaymentSigningKey  -- ^ The key to the faucet.
        -> AddressAny             -- ^ The faucet address.
        -> AddressAny             -- ^ The address for burning tokens.
        -> String                 -- ^ The wallet passphrase.
        -> PabTest                -- ^ The tests to be run.
        -> m ()                   -- ^ Action for running the tests.
pabTest access faucetKey faucetAddress burnAddress passphrase PabTest{..} =
  do
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Test " <> show ptTestName <> " *****"
    catchError
      (
        -- TODO: Add a timeout.
        void
          . execStateT (mapM_ (interpret access) ptPabOperations)
          $ PabState
            faucetKey faucetAddress burnAddress
            (Passphrase . BA.pack $ toEnum . fromEnum <$> passphrase)
            mempty mempty mempty
      )
      $ \e ->
        -- TODO: Clean up wallets and instances.
        liftIO (putStrLn "***** FAILED *****")
          >> throwError (e :: CliError)
    liftIO $ putStrLn "***** PASSED *****"


-- | Execute a test operation.
interpret :: MonadError CliError m
          => MonadIO m
          => PabAccess             -- ^ Access to the PAB APIs.
          -> PabOperation          -- ^ The test operation.
          -> StateT PabState m ()  -- ^ Action for running the test.

interpret access CreateWallet{..} =
  do
    passphrase <- use psPassphrase
    wi <- lift $ createWallet access poOwner passphrase
    liftIO . putStrLn $ "[CreateWallet] Created wallet identified as " <> show (W.getWalletId $ wiWalletId wi) <> " for role " <> show poOwner <> "."
    psWallets %= M.insert poOwner wi

interpret access UseWallet{..} =
  do
    let
      passphrase = Passphrase . BA.pack $ toEnum . fromEnum <$> poPassphrase
    wi <- lift $ useWallet access poWalletId passphrase
    liftIO . putStrLn $ "[UseWallet] Using wallet identified as " <> show (W.getWalletId $ wiWalletId wi) <> " for role " <> show poOwner <> "."
    psWallets %= M.insert poOwner wi

interpret PabAccess{..} FundWallet{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    faucetKey <- use psFaucetKey
    faucetAddress <- use psFaucetAddress
    void
      $ buildFaucet
          localConnection
          poValue wiAddress
          faucetAddress faucetKey
          (Just 600)
    liftIO . putStrLn $ "[FundWallet] Funded wallet for role " <> show poOwner <> " with " <> show poValue <> "."

interpret PabAccess{..} ReturnFunds{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    faucetAddress <- use psFaucetAddress
    faucetAddress' <-
      case faucetAddress of
        AddressShelley address -> pure $ fromCardanoAddress address
        _                      -> throwError $ CliError "ReturnFunds] Invalid wallet address."
    let
      migrate = Servant.client $ Proxy @("v2" :> MigrateShelleyWallet ('Testnet 0))  -- FIXME: Generalize to all network magic.
      fetch   = Servant.client $ Proxy @("v2" :> GetTransaction       ('Testnet 0))  -- FIXME: Generalize to all network magic.
    ApiTransaction{id = txHash}  :| _ <-
      liftCliIO
      . runWallet
      $ migrate
          (ApiT wiWalletId)
          (ApiWalletMigrationPostData (ApiT wiPassphrase) ((ApiT faucetAddress', Proxy) :| []))
    let
      go =
        do
          liftIO $ threadDelay 5_000_000
          ApiTransaction{pendingSince} <-
            liftCliIO
              . runWallet
              $ fetch (ApiT wiWalletId) (ApiTxId txHash)
          whenJust pendingSince
            $ const go
    go
    faucetKey <- use psFaucetKey
    burnAddress <- use psBurnAddress
    roleTokens <- findRoleTokens poOwner poInstances
    let
      minAda =
        lovelaceToValue
          . fromInteger
          $ 1300000 + 150000 * fromIntegral (length $ valueToList roleTokens)
    void
      $ buildFaucet
          localConnection
          (minAda <> roleTokens) burnAddress
          faucetAddress faucetKey
          (Just 600)
    liftIO . putStrLn $ "[ReturnFunds] Returned funds from wallet for role " <> show poOwner <> "."

interpret access CheckFunds{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    actual <- lift $ totalBalance access wiWalletId
    roleTokens <- findRoleTokens poOwner poInstances
    let
      actualLovelace = selectLovelace actual
      maximumLovelace = selectLovelace poValue
      minimumLovelace = maximumLovelace - poMaximumFees
      actualTokens = actual <> negateValue (lovelaceToValue actualLovelace)
      expectedTokens = poValue <> negateValue (lovelaceToValue maximumLovelace)
    unless (actualLovelace >= minimumLovelace && actualLovelace <= maximumLovelace)
      . throwError
      . CliError
      $ "[CheckFunds]: Wallet for role " <> show poOwner <> " contains " <> show actualLovelace <> ", which is outside the range (" <> show minimumLovelace <> "," <> show maximumLovelace <> ")."
    unless (actualTokens == expectedTokens <> roleTokens)
      . throwError
      . CliError
      $ "[CheckFunds]: Wallet for role " <> show poOwner <> " contains unexpected tokens " <> show (actualTokens <> negateValue expectedTokens <> negateValue roleTokens) <> "."
    liftIO . putStrLn $ "[CheckFunds] Wallet for role " <> show poOwner <> " contains " <> show actual <> "."

interpret access ActivateApp{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    (aiInstance, aiChannel) <- runContract access MarloweApp wiWalletId'
    let
      aiParams = Nothing
    liftIO . putStrLn $ "[ActivateApp] Activated MarloweApp instance " <> show poInstance <> " with identifier " <> show (unContractInstanceId aiInstance) <> " for role " <> show poOwner <> "."
    psAppInstances %= M.insert poInstance AppInstanceInfo{..}

interpret access CallCreate{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findAppInstance poInstance
    owners <-
      mapM
        (
          \owner ->
            (TokenName . toBuiltin $ BS8.pack owner, )
              . (anyAddressInShelleyBasedEra :: AddressAny -> AddressInEra ShelleyEra)
              . wiAddress
              <$> findOwner owner
        )
        poOwners
    lift
      $ call access aiInstance "create" ((uuid, AM.fromList owners, poContract) :: CreateEndpointSchema)
    liftIO . putStrLn $ "[CallCreate] Endpoint \"create\" called on instance " <> show poInstance <> " for owners " <> show owners <> "."

interpret PabAccess{localConnection = LocalNodeConnectInfo _ network _} AwaitCreate{..} =
  do
    result <- awaitApp poInstance (-1)
    case result of
      CreateResponse params -> do
                                 psAppInstances %=
                                   M.adjust
                                     (\ai -> ai {aiParams = Just params})
                                     poInstance
                                 let
                                   appAddress = T.unpack $ serialiseAddress (buildAddress params network NoStakeAddress :: AddressInEra AlonzoEra)
                                   roleAddress = T.unpack $ serialiseAddress (buildRoleAddress (rolesCurrency params) network NoStakeAddress :: AddressInEra AlonzoEra)
                                 liftIO . putStrLn $ "[AwaitCreate] Creation confirmed for app address " <> appAddress <> " and role address " <> roleAddress <> " with " <> show params <> "."
      _                     -> throwError . CliError $ "[AwaitCreate] received unexpected response " <> show result <> "."

interpret access CallApplyInputs{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findAppInstance poInstance
    params <- maybe (throwError $ CliError "[CallApplyInputs] Contract parameters are not known.") pure aiParams
    lift
      $ call access aiInstance "apply-inputs" ((uuid, params, poTimes, poInputs) :: ApplyInputsEndpointSchema)
    liftIO . putStrLn $ "[CallApplyInputs] Endpoint \"apply-inputs\" called on instance " <> show poInstance <> " for inputs " <> show poInputs <> " and times " <> show poTimes <> "."

interpret _ AwaitApplyInputs{..} =
  do
    result <- awaitApp poInstance (-1)
    if result == ApplyInputsResponse
      then liftIO . putStrLn $ "[AwaitApplyInputs] Input application confirmed."
      else throwError . CliError $ "[AwaitApplyInputs] received unexpected response " <> show result <> "."

interpret access CallAuto{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findAppInstance poInstance
    params <- maybe (throwError $ CliError "[CallAuto] Contract parameters are not known.") pure aiParams
    lift
      $ call access aiInstance "auto"
      ((
        uuid
      , params
      , Role . TokenName . toBuiltin $ BS8.pack poOwner
      , poAbsoluteTime
      ) :: AutoEndpointSchema)
    liftIO . putStrLn $ "[CallAuto] Endpoint \"auto\" called on instance " <> show poInstance <> " on behalf of role " <> show poOwner <> " until time " <> show poAbsoluteTime <> "."

interpret _ AwaitAuto{..} =
  do
    result <- awaitApp poInstance (-1)
    if result == AutoResponse
      then liftIO . putStrLn $ "[AwaitAuto] Automatic execution confirmed."
      else throwError . CliError $ "[AwaitAuto] received unexpected response " <> show result <> "."

interpret access CallRedeem{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findAppInstance poInstance
    WalletInfo{..} <- findOwner poOwner
    params <- maybe (throwError $ CliError "[CallRedeem] Contract parameters are not known.") pure aiParams
    lift
      $ call access aiInstance "redeem"
      ((
        uuid
      , params
      , TokenName . toBuiltin $ BS8.pack poOwner
      , anyAddressInShelleyBasedEra wiAddress :: AddressInEra ShelleyEra
      ) :: RedeemEndpointSchema)
    liftIO . putStrLn $ "[CallRedeem] Endpoint \"redeem\" called on instance " <> show poInstance <> " for role " <> show poOwner <> "."

interpret _ AwaitRedeem{..} =
  do
    result <- awaitApp poInstance (-1)
    if result == RedeemResponse
      then liftIO . putStrLn $ "[AwaitRedeem] Redemption confirmed."
      else throwError . CliError $ "[AwaitRedeem] received unexpected response " <> show result <> "."

interpret access CallClose{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findAppInstance poInstance
    lift
      $ call access aiInstance "close" uuid
    liftIO . putStrLn $ "[CallClose] Endpoint \"close\" called on instance " <> show poInstance <> "."

interpret _ AwaitClose{..} =
  do
    result <- awaitApp poInstance (-1)
    if result == CloseResponse
      then liftIO . putStrLn $ "[AwaitClose] Closing application confirmed."
      else throwError . CliError $ "[AwaitClose] received unexpected response " <> show result <> "."

interpret PabAccess{..} Stop{..} =
  do
    AppInstanceInfo{..} <- findAppInstance poInstance
    let
      PabClient{..} = client
      InstanceClient{..} = instanceClient aiInstance
    liftCliIO
      $ runApi stopInstance
    -- TODO: Update state.
    liftIO . putStrLn $ "[Stop] Instance " <> show poInstance <> " stopped."

interpret _ Follow{..} =
  do
    aiOther <- findAppInstance poOtherInstance
    psAppInstances %=
      M.adjust
        (\ai -> ai {aiParams = aiParams aiOther})
        poInstance
    liftIO . putStrLn $ "[Follow] Instance " <> show poInstance <> " now follows instance " <> show poOtherInstance <> "."

interpret access ActivateFollower{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    (fiInstance, fiChannel) <- runContract access MarloweFollower wiWalletId'
    AppInstanceInfo{..} <- findAppInstance poAppInstance
    case aiParams of
      Nothing -> throwError . CliError $ "[ActivateFollower] not able to follow AppInstance with missinng MarloweParams."
      Just params -> do
                        let
                          fiParams = params
                        liftIO . putStrLn $
                          "[ActivateFollower] Activated MarloweFollower instance "
                          <> show (unContractInstanceId fiInstance)
                          <> " for role "
                          <> show poOwner
                          <> "."
                        psFollowerInstances %= M.insert poInstance FollowerInstanceInfo{..}

interpret access CallFollow{..} =
  do
    FollowerInstanceInfo{..} <- findFollowerInstance poInstance
    lift
      $ call access fiInstance "follow" fiParams
    liftIO . putStrLn $ "[CallFollow] Endpoint \"follow\" called on " <> show (unContractInstanceId fiInstance) <> "."

interpret _ AwaitFollow{..} =
  do
    FollowerInstanceInfo{..} <- findFollowerInstance poInstance
    liftIO . putStrLn $ "[AwaitFollow] fetching channel messages."
    --  Skipp all preceeding `null`s
    contractHistoryJSON <- untilJustM $ do
      res <- liftIO $ readChan fiChannel
      if res == Null
        then do
          liftIO . putStrLn $ "[AwaitFollow] Skipping `null` response."
          pure Nothing
        else pure $ Just res

    let
      extractPatternJSON (Parts json) = json
      extractPatternJSON (Exact json) = json

    case poResponsePattern of
      Just pt -> if matchJSON pt contractHistoryJSON
        then liftIO . putStrLn $ "[AwaitFollow] Follow confirmed."
        else throwError $ CliError $ T.unpack $
          "[AwaitFollow] Received response does not match expected pattern. Expected: "
          <> renderValue (extractPatternJSON pt)
          <> ". Received: "
          <> renderValue contractHistoryJSON
          <> "."
      Nothing ->
        liftIO . putStrLn $ "[AwaitFollow] Follow confirmed."

interpret _ PrintState =
  do
    ps <- get
    liftIO . putStrLn $ "[PrintState] " <> show ps <> "."

interpret access PrintWallet{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    actual <- lift $ totalBalance access wiWalletId
    liftIO . putStrLn $ "[PrintWallet] Wallet for role " <> show poOwner <> " contains " <> show actual <> "."

interpret PabAccess{..} PrintAppUTxOs{..} =
  do
    AppInstanceInfo{..} <- findAppInstance poInstance
    params <- maybe (throwError $ CliError "[PrintAppUTxOs] Contract parameters are not known.") pure aiParams
    let
      toAddressAny' (AddressInEra _ address') = toAddressAny address'
      LocalNodeConnectInfo _ network _ = localConnection
      address = buildAddress params network NoStakeAddress :: AddressInEra AlonzoEra
    utxos <- queryUtxos localConnection $ toAddressAny' address
    liftIO . putStrLn $ "[PrintAppUTxOs] App address " <> T.unpack (serialiseAddress address) <> " contains " <> show utxos <> "."

interpret PabAccess{..} PrintRoleUTxOs{..} =
  do
    AppInstanceInfo{..} <- findAppInstance poInstance
    params <- maybe (throwError $ CliError "[PrintRoleUTxOs] Contract parameters are not known.") pure aiParams
    let
      toAddressAny' (AddressInEra _ address') = toAddressAny address'
      LocalNodeConnectInfo _ network _ = localConnection
      address = buildRoleAddress (rolesCurrency params) network NoStakeAddress :: AddressInEra AlonzoEra
    utxos <- queryUtxos localConnection $ toAddressAny' address
    liftIO . putStrLn $ "[PrintAppUTxOs] Role address " <> T.unpack (serialiseAddress address) <> " contains " <> show utxos <> "."

interpret _ Comment{..} =
  liftIO . putStrLn $ "[Comment] " <> poComment

interpret _ WaitFor{..} =
  do
    let
      DiffMilliSeconds delta = poRelativeTime
    liftIO . threadDelay $ 1000 * fromIntegral delta
    liftIO . putStrLn $ "[WaitFor] Waited for " <> show delta <> " milliseconds."

interpret _ WaitUntil{..} =
  do
    now <- liftIO $ floor . (1000 *) . nominalDiffTimeToSeconds <$> Time.getPOSIXTime
    let
      delta = maximum [0, getPOSIXTime poAbsoluteTime - now]
    liftIO . threadDelay $ 1000 * fromIntegral delta
    liftIO . putStrLn $ "[WaitUntil] Waited for " <> show delta <> " milliseconds until POSIX " <> show (getPOSIXTime poAbsoluteTime) <> " milliseconds."

interpret access Timeout{..} =
  do
    state <- get
    result <-
      liftIO
        . timeout (1_000_000 * poTimeoutSeconds)
        $ runOperationsToIO access poOperations state
    case result of
      Just (Right state') -> do
                               put state'
                               liftIO . putStrLn $ "[Timeout] Operations completed within " <> show poTimeoutSeconds <> " seconds."
      Just (Left e)       -> throwError e
      Nothing             -> throwError . CliError $ "[Timeout] Operations did not complete within " <> show poTimeoutSeconds <> " seconds: " <> show poOperations <> "."

interpret access ShouldFail{..} =
  do
    state <- get
    result <-
      liftIO
        $ runOperationsToIO access poOperations state
        `catch` (\e -> pure . Left . CliError $ displayException (e :: SomeException))
    case result of
      Right state' -> throwError . CliError $ "[ShouldFail] Operations unexpectedly suceeded: " <> show poOperations <> " resulting in " <> show state' <> "."
      Left e       -> liftIO . putStrLn $ "[ShouldFail] Operations failed as expected: " <> show e <> " occurred for " <> show poOperations <> "."


-- | Execut test operations into the IO monad.
runOperationsToIO :: PabAccess
                  -> [PabOperation]                 -- ^ The test operations.
                  -> PabState                       -- ^ The state of the PAB instances and wallets.
                  -> IO (Either CliError PabState)  -- ^ Action for running the test operations, or an error.
runOperationsToIO access operations =
  runExceptT
    . execStateT (mapM_ (interpret access) operations)


-- | Wait for a websocket message from the PAB.
awaitApp :: MonadError CliError m
         => MonadIO m
         => MonadState PabState m
         => InstanceNickname         -- ^ The contract instance.
         -> Int                      -- ^ How many empty responses to tolerate before failing.
         -> m MarloweEndpointResult  -- ^ Action to return the endpoint result from the websocket channel.
awaitApp nickname nothings =
  do
    AppInstanceInfo{..} <- findAppInstance nickname
    let
      go i =
        do
          mcs <- liftIO $ readChan aiChannel
          -- TODO: Figure out why Nothing is told."
          case mcs of
            Just (EndpointSuccess _ r)     -> pure r
            Just (EndpointException _ _ e) -> throwError . CliError $ "[awaitApp] Received unexpected response " <> show e <> "."
            Nothing                        -> if i == 0
                                                then throwError $ CliError "[awaitApp] Received no response."
                                                else go (i - 1)
    go nothings


-- | Find the wallet for a role.
findOwner :: MonadError CliError m
          => MonadState PabState m
          => RoleName      -- ^ The role name.
          -> m WalletInfo  -- ^ Action for finding the role's wallet.
findOwner owner =
  liftCliMaybe ("[findOwner] Wallet not found for role " <> show owner <> ".")
    . M.lookup owner
    =<< use psWallets


-- | Find MarloweApp contract instance corresponding to an instance nickname.
findAppInstance :: MonadError CliError m
                => MonadState PabState m
                => InstanceNickname   -- ^ The nickname.
                -> m AppInstanceInfo  -- ^ Action returning the instance.
findAppInstance nickname =
  liftCliMaybe ("[findAppInstance] App instance not found for nickname " <> show nickname <> ".")
    . M.lookup nickname
    =<< use psAppInstances


-- | Find MarloweFollower contract instance corresponding to an instance nickname.
findFollowerInstance :: MonadError CliError m
             => MonadState PabState m
             => InstanceNickname
             -> m FollowerInstanceInfo
findFollowerInstance nickname =
  liftCliMaybe ("[findFollowerInstance] Follower instance not found for nickname " <> show nickname <> ".")
    . M.lookup nickname
    =<< use psFollowerInstances


-- | Find the role tokens for the given instances.
findRoleTokens :: MonadError CliError m
               => MonadState PabState m
               => RoleName            -- ^ The role name of the owner.
               -> [InstanceNickname]  -- ^ The instance to search for role currencies.
               -> m C.Value           -- ^ Action returning the role tokens for the owner from the instances.
findRoleTokens poOwner poInstances =
  valueFromList
    <$> sequence
    [
      do
        AppInstanceInfo{..} <- findAppInstance poInstance
        CurrencySymbol currency <-
          liftCliMaybe ("[findRoleTokens] Parameters not found for instance " <> show poInstance <> ".")
            $ rolesCurrency
            <$> aiParams
        policy <-
          liftCliMaybe "[findRoleTokens] Failed deserialising currency symbol."
            . deserialiseFromRawBytes AsScriptHash
            $ fromBuiltin currency
        pure
          (AssetId (PolicyId policy) (AssetName $ BS8.pack poOwner), 1)
    |
      poInstance <- poInstances
    ]


-- | Create a new, random wallet.
createWallet :: MonadError CliError m
             => MonadIO m
             => PabAccess         -- ^ Access to the PAB API.
             -> RoleName          -- ^ The name of the owner.
             -> Passphrase "raw"  -- ^ The passphrase for the wallet.
             -> m WalletInfo      -- ^ Action returning the new wallet information.
createWallet PabAccess{..} owner wiPassphrase =
  do
    mnemonicSentence' <- liftIO . generate $ genMnemonic @24
    let
      mnemonicSentence = ApiMnemonicT . SomeMnemonic $ mnemonicSentence'
      addressPoolGap = Nothing
      mnemonicSecondFactor = Nothing
      name = ApiT . WalletName . T.pack $ show owner
      passphrase = ApiT wiPassphrase
    wallet <-
      liftCliIO
        . runWallet
        . postWallet walletClient
        . WalletOrAccountPostData
        $ Left WalletPostData{..}
    let
      wiWalletId = getApiT $ W.id wallet
      wiWalletId' = WalletId wiWalletId
    addresses <- liftCliIO . runWallet $ listAddresses addressClient (W.id wallet) Nothing
    (wiAddress, wiPubKeyHash) <-
      case addresses of
        Object o : _ -> liftCliMaybe "[createWallet] Failed to deserialise wallet address."
                          $ do
                            address <-
                              case H.lookup "id" o of
                                Just (String a) -> deserialiseAddress (AsAddress AsShelleyAddr) a
                                _               -> Nothing
                            pkh <- shelleyPayAddrToPlutusPubKHash address
                            pure (toAddressAny address, pkh)
        _            -> throwError $ CliError "[createWallet] No addresses found in wallet."
    liftIO . putStrLn $ "[createWallet] Mnemonic sentence is \"" <> unwords (T.unpack <$> mnemonicToText mnemonicSentence') <> "\"."
    liftIO . putStrLn $ "[createWallet] First wallet address is " <> T.unpack (serialiseAddress wiAddress) <> "."
    liftIO . putStrLn $ "[createWallet] First public key hash is " <> show wiPubKeyHash <> "."
    let
      go =
        do
          liftIO $ threadDelay 5_000_000
          s <- W.state <$> liftCliIO (runWallet (getWallet walletClient $ W.id wallet))
          unless (s == ApiT Ready) go
    go
    pure WalletInfo{..}


-- | Create a new, random wallet.
useWallet :: MonadError CliError m
             => MonadIO m
             => PabAccess         -- ^ Access to the PAB API.
             -> WalletId          -- ^ The wallet ID.
             -> Passphrase "raw"  -- ^ The passphrase for the wallet.
             -> m WalletInfo      -- ^ Action returning the new wallet informatio.
useWallet PabAccess{..} wiWalletId'@(WalletId wiWalletId) wiPassphrase =
  do
    addresses <- liftCliIO . runWallet $ listAddresses addressClient (ApiT wiWalletId) Nothing
    (wiAddress, wiPubKeyHash) <-
      case addresses of
        Object o : _ -> liftCliMaybe "[useWallet] Failed to deserialise wallet address."
                          $ do
                            address <-
                              case H.lookup "id" o of
                                Just (String a) -> deserialiseAddress (AsAddress AsShelleyAddr) a
                                _               -> Nothing
                            pkh <- shelleyPayAddrToPlutusPubKHash address
                            pure (toAddressAny address, pkh)
        _            -> throwError $ CliError "[useWallet] No addresses found in wallet."
    liftIO . putStrLn $ "[useWallet] First wallet address is " <> T.unpack (serialiseAddress wiAddress) <> "."
    liftIO . putStrLn $ "[useWallet] First public key hash is " <> show wiPubKeyHash <> "."
    let
      go =
        do
          liftIO $ threadDelay 5_000_000
          s <- W.state <$> liftCliIO (runWallet (getWallet walletClient $ ApiT wiWalletId))
          unless (s == ApiT Ready) go
    go
    pure WalletInfo{..}


-- | Compute the total balance of a wallet.
totalBalance :: MonadError CliError m
             => MonadIO m
             => PabAccess   -- ^ Access to the PAB APIs.
             -> W.WalletId  -- ^ The wallet identifier.
             -> m C.Value   -- ^ Action returning the total balance.
totalBalance PabAccess{..} walletId =
  do
    -- Cardano Wallet may lag the PAB and chain index, so wait a bit before querying funds.
    liftIO $ threadDelay 15000
    ApiWallet{balance,assets} <- liftCliIO $ runWallet (getWallet walletClient $ ApiT walletId)
    let
      ApiWalletBalance (W.Quantity lovelace) _ _ = balance
      ApiWalletAssetsBalance (ApiT tokenMap) _ = assets
      lovelace' = quantityToLovelace . Quantity $ toInteger lovelace
    assets' <-
      liftCliMaybe "[totalBalance] Failed converting currency symbol."
        $ sequence
        [
          (\p -> (AssetId (PolicyId p) (AssetName name), Quantity $ toInteger quantity)) <$> policy'
        |
          (W.AssetId (UnsafeTokenPolicyId (W.Hash policy)) (UnsafeTokenName name), TokenQuantity quantity) <- W.toFlatList tokenMap
        , let policy' = deserialiseFromRawBytes AsScriptHash policy
        ]
    pure
      . valueFromList
      . filter ((/= 0) . snd)
      . valueToList
      $ lovelaceToValue lovelace' <> valueFromList assets'


-- | Run a contract instance.
runContract :: MonadError CliError m
            => MonadIO m
            => FromJSON a
            => PabAccess                       -- ^ Access to the PAB APIs.
            -> MarloweContract                 -- ^ The contract to be run.
            -> WalletId                        -- ^ The wallet to be associated with the instance.
            -> m (ContractInstanceId, Chan a)  -- ^ Action returning the contract's instance identifier and the channel for messages from the PAB.
runContract PabAccess{..} contract walletId =
  do
    let
      PabClient{..} = client
    instanceChan <- liftIO newChan
    instanceId <-
      liftCliIO
        . runApi
        . activateContract
        $ ContractActivationArgs
          {
            caID     = contract
          , caWallet = Just $ Wallet Nothing walletId
          }
    let
      go :: Connection -> ExceptT CliError IO ()
      go connection =
        do
          let
            repr (NewObservableState s) = T.unpack $ renderValue s
            repr other                  = show other

          status <- receiveStatus connection
          when verbose
            $ liftIO . putStrLn $ "[runContract] Instance " <> show (unContractInstanceId instanceId) <> " received " <> repr status <> "."
          case status of
            NewObservableState s -> do
                                      state <- liftCli $ parseEither parseJSON s
                                      liftIO $ writeChan instanceChan state
                                      go connection
            ContractFinished _   -> pure ()
            _                    -> go connection
    void
      . liftIO
      . forkIO
      $ runWs instanceId go
    pure (instanceId, instanceChan)


-- | Call the endpoint of a contract instance.
call :: MonadError CliError m
     => MonadIO m
     => ToJSON a
     => PabAccess           -- ^ Access to the PAB APIs.
     -> ContractInstanceId  -- ^ The instance identifier for the contract.
     -> String              -- ^ The name of the endpoint.
     -> a                   -- ^ The arguments to the endpoint call.
     -> m ()                -- ^ Action for calling the endpoint.
call PabAccess{..} instanceId endpoint arguments =
  let
    PabClient{..} = client
    InstanceClient{..} = instanceClient instanceId
  in
    liftCliIO
      . runApi
      . callInstanceEndpoint endpoint
      $ toJSON arguments

matchJSON :: PatternJSON -> A.Value -> Bool
matchJSON (Exact expected) given = expected == given
-- ^ Pattern match on the the array prefixes and subsets of fields.
matchJSON (Parts expected) given = case (expected, given) of
  (A.Array eItems, A.Array gItems) ->
    length gItems == length eItems && (V.all (\(ev, gv) -> matchJSON (Parts ev) gv) . V.zip eItems $ gItems)
  (A.Object eProps, A.Object gProps) ->
    let
      step key ev acc = (&&) acc $ fromMaybe False $ do
        gv <- H.lookup key gProps
        pure $ matchJSON (Parts ev) gv
    in
      H.foldrWithKey step True eProps
  (A.String ev, A.String gv) -> ev == gv
  (A.Number ev, A.Number gv) -> ev == gv
  (A.Bool ev, A.Bool gv) -> ev == gv
  (A.Null, A.Null) -> True
  (_, _) -> False
