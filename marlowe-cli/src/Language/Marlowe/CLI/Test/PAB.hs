
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
  pabTest
) where


import Cardano.Api (AddressAny (..), AddressInEra, AsType (AsAddress, AsScriptHash, AsShelleyAddr), AssetId (..),
                    AssetName (..), PolicyId (..), Quantity (..), ShelleyEra, anyAddressInShelleyBasedEra,
                    deserialiseAddress, deserialiseFromRawBytes, lovelaceToValue, negateValue, quantityToLovelace,
                    selectLovelace, toAddressAny, valueFromList, valueToList)
import Cardano.Api.Shelley (shelleyPayAddrToPlutusPubKHash)
import Cardano.Mnemonic (SomeMnemonic (..))
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
import Control.Monad.Extra (whenJust)
import Control.Monad.State.Strict (MonadState, StateT, execStateT, get, lift, put)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object, String))
import Data.Aeson.Types (parseEither)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.CLI.IO (liftCli, liftCliIO, liftCliMaybe)
import Language.Marlowe.CLI.PAB (receiveStatus)
import Language.Marlowe.CLI.Test.Types (AppInstanceInfo (..), InstanceNickname, PabAccess (..), PabOperation (..),
                                        PabState (..), PabTest (..), RoleName, WalletInfo (..), psAppInstances,
                                        psBurnAddress, psFaucetAddress, psFaucetKey, psPassphrase, psWallets)
import Language.Marlowe.CLI.Transaction (buildFaucet)
import Language.Marlowe.CLI.Types (CliError (..), SomePaymentSigningKey)
import Language.Marlowe.Client (EndpointResponse (..), MarloweEndpointResult (..))
import Language.Marlowe.Contract (MarloweContract (..))
import Language.Marlowe.Semantics (MarloweParams (rolesCurrency))
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
import qualified Data.ByteArray as BA (pack)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.HashMap.Strict as H (lookup)
import qualified Data.Map.Strict as M (adjust, insert, lookup)
import qualified Data.Quantity as W (Quantity (..))
import qualified Data.Text as T (pack)
import qualified Data.Time.Clock.POSIX as Time (getPOSIXTime)
import qualified PlutusTx.AssocMap as AM (fromList)
import qualified Servant.Client as Servant (client)


verbose :: Bool
verbose = True


pabTest :: MonadError CliError m
        => MonadIO m
        => PabAccess
        -> SomePaymentSigningKey
        -> AddressAny
        -> AddressAny
        -> String
        -> PabTest
        -> m ()
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
            mempty mempty
      )
      $ \e ->
        -- TODO: Clean up wallets and instances.
        liftIO (putStrLn "***** FAILED *****")
          >> throwError (e :: CliError)
    liftIO $ putStrLn "***** SUCCEEDED *****"


interpret :: MonadError CliError m
          => MonadIO m
          => PabAccess
          -> PabOperation
          -> StateT PabState m ()
interpret access CreateWallet{..} =
  do
    passphrase <- use psPassphrase
    wi <- lift $ createWallet access poOwner passphrase
    liftIO . putStrLn $ "[CreateWallet] Created wallet " <> show (W.getWalletId $ wiWalletId wi) <> " for role " <> show poOwner <> "."
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
    liftIO . putStrLn $ "[FundWallet] Funded wallet " <> show (W.getWalletId wiWalletId) <> " for role " <> show poOwner <> " with " <> show poValue <> "."
interpret PabAccess{..} ReturnFunds{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    passphrase <- use psPassphrase
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
          (ApiWalletMigrationPostData (ApiT passphrase) ((ApiT faucetAddress', Proxy) :| []))
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
    liftIO . putStrLn $ "[ReturnFunds] Returned funds from wallet " <> show (W.getWalletId wiWalletId) <> "."
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
    (aiInstance, aiChannel) <- lift $ runContract access MarloweApp wiWalletId'
    let
      aiParams = Nothing
    liftIO . putStrLn $ "[ActivateApp] Activated MarloweApp instance " <> show (unContractInstanceId aiInstance) <> " for role " <> show poOwner <> "."
    psAppInstances %= M.insert poInstance AppInstanceInfo{..}
interpret access CallCreate{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findInstance poInstance
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
      $ call access aiInstance "create" (uuid, AM.fromList owners, poContract)
    liftIO . putStrLn $ "[CallCreate] Endpoint \"create\" called on instance " <> show (unContractInstanceId aiInstance) <> " for owners " <> show owners <> "."
interpret _ AwaitCreate{..} =
  do
    result <- awaitApp poInstance (-1)
    case result of
      CreateResponse params -> do
                                 psAppInstances %=
                                   M.adjust
                                     (\ai -> ai {aiParams = Just params})
                                     poInstance
                                 liftIO . putStrLn $ "[AwaitCreate] Creation confirmed with " <> show params <> "."
      _                     -> throwError . CliError $ "[AwaitCreate] received unexpected response " <> show result <> "."
interpret access CallApplyInputs{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findInstance poInstance
    lift
      $ call access aiInstance "apply-inputs" (uuid, aiParams, poTimes, poInputs)
    liftIO . putStrLn $ "[CallApplyInputs] Endpoint \"apply-inputs\" called on " <> show (unContractInstanceId aiInstance) <> " for inputs " <> show poInputs <> " and times " <> show poTimes <> "."
interpret _ AwaitApplyInputs{..} =
  do
    result <- awaitApp poInstance (-1)
    if result == ApplyInputsResponse
      then liftIO . putStrLn $ "[AwaitApplyInputs] Input application confirmed."
      else throwError . CliError $ "[AwaitApplyInputs] received unexpected response " <> show result <> "."
interpret access CallRedeem{..} =
  do
    uuid <- liftIO nextRandom
    AppInstanceInfo{..} <- findInstance poInstance
    WalletInfo{..} <- findOwner poOwner
    lift
      $ call access aiInstance "redeem"
      (
        uuid
      , aiParams
      , TokenName . toBuiltin $ BS8.pack poOwner
      , anyAddressInShelleyBasedEra wiAddress :: AddressInEra ShelleyEra
      )
    liftIO . putStrLn $ "[CallRedeem] Endpoint \"redeem\" called on " <> show (unContractInstanceId aiInstance) <> " for role " <> show poOwner <> "."
interpret _ AwaitRedeem{..} =
  do
    result <- awaitApp poInstance (-1)
    if result == RedeemResponse
      then liftIO . putStrLn $ "[AwaitRedeem] Redemption confirmed."
      else throwError . CliError $ "[AwaitRedeem] received unexpected response " <> show result <> "."
interpret PabAccess{..} Stop{..} =
  do
    AppInstanceInfo{..} <- findInstance poInstance
    let
      PabClient{..} = client
      InstanceClient{..} = instanceClient aiInstance
    liftCliIO
      $ runApi stopInstance
    -- TODO: Update state.
    liftIO . putStrLn $ "[Stop] Instance " <> show (unContractInstanceId aiInstance) <> " stopped."
interpret _ Follow{..} =
  do
    aiThis <- findInstance poInstance
    aiOther <- findInstance poOtherInstance
    psAppInstances %=
      M.adjust
        (\ai -> ai {aiParams = aiParams aiOther})
        poInstance
    liftIO . putStrLn $ "[Follow] Instance " <> show (unContractInstanceId $ aiInstance aiThis) <> " now follows instance " <> show (unContractInstanceId $ aiInstance aiOther) <> "."
interpret _ PrintState =
  do
    ps <- get
    liftIO . putStrLn $ "[PrintState] " <> show ps <> "."
interpret access PrintWallet{..} =
  do
    WalletInfo{..} <- findOwner poOwner
    actual <- lift $ totalBalance access wiWalletId
    liftIO . putStrLn $ "[PrintWallet] Wallet for role " <> show poOwner <> " contains " <> show actual <> "."
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
        $ runOperationsToIO access [poOperation] state
    case result of
      Just (Right state') -> do
                               put state'
                               liftIO . putStrLn $ "[Timeout] Operation completed within " <> show poTimeoutSeconds <> " seconds."
      Just (Left e)       -> throwError e
      Nothing             -> throwError . CliError $ "[Timeout] Operation did not complete within " <> show poTimeoutSeconds <> " seconds: " <> show poOperation <> "."
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


runOperationsToIO :: PabAccess
                  -> [PabOperation]
                  -> PabState
                  -> IO (Either CliError PabState)
runOperationsToIO access operations =
  runExceptT
    . execStateT (mapM_ (interpret access) operations)


awaitApp :: MonadError CliError m
         => MonadIO m
         => MonadState PabState m
         => InstanceNickname
         -> Int
         -> m MarloweEndpointResult
awaitApp nickname nothings =
  do
    AppInstanceInfo{..} <- findInstance nickname
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


findOwner :: MonadError CliError m
          => MonadState PabState m
          => RoleName
          -> m WalletInfo
findOwner owner =
  liftCliMaybe ("[findOwner] Wallet not found for role " <> show owner <> ".")
    . M.lookup owner
    =<< use psWallets


findInstance :: MonadError CliError m
             => MonadState PabState m
             => InstanceNickname
             -> m AppInstanceInfo
findInstance nickname =
  liftCliMaybe ("[findInstance] Instance not found for nickname " <> show nickname <> ".")
    . M.lookup nickname
    =<< use psAppInstances


findRoleTokens :: MonadError CliError m
               => MonadState PabState m
               => RoleName
               -> [InstanceNickname]
               -> m C.Value
findRoleTokens poOwner poInstances =
  valueFromList
    <$> sequence
    [
      do
        AppInstanceInfo{..} <- findInstance poInstance
        CurrencySymbol currency <-
          liftCliMaybe ("[findRoleTokens] Parameters not found for instance " <> show (unContractInstanceId aiInstance) <> ".")
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


createWallet :: MonadError CliError m
             => MonadIO m
             => PabAccess
             -> RoleName
             -> Passphrase "raw"
             -> m WalletInfo
createWallet PabAccess{..} owner passphrase' =
  do
    mnemonicSentence <-
      liftIO . generate
        $ ApiMnemonicT . SomeMnemonic
        <$> genMnemonic @24
    let
      addressPoolGap = Nothing
      mnemonicSecondFactor = Nothing
      name = ApiT . WalletName . T.pack $ show owner
      passphrase = ApiT passphrase'
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
        Object o : _ -> liftCliMaybe "[CreateWallet] Failed to deserialise wallet address."
                          $ do
                            address <-
                              case H.lookup "id" o of
                                Just (String a) -> deserialiseAddress (AsAddress AsShelleyAddr) a
                                _               -> Nothing
                            pkh <- shelleyPayAddrToPlutusPubKHash address
                            pure (toAddressAny address, pkh)
        _            -> throwError $ CliError "[CreateWallet] No addresses found in wallet."
    let
      go =
        do
          liftIO $ threadDelay 5_000_000
          s <- W.state <$> liftCliIO (runWallet (getWallet walletClient $ W.id wallet))
          unless (s == ApiT Ready) go
    go
    pure WalletInfo{..}


totalBalance :: MonadError CliError m
             => MonadIO m
             => PabAccess
             -> W.WalletId
             -> m C.Value
totalBalance PabAccess{..} walletId =
  do
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


runContract :: MonadError CliError m
            => MonadIO m
            => FromJSON a
            => PabAccess
            -> MarloweContract
            -> WalletId
            -> m (ContractInstanceId, Chan a)
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
          , caWallet = Just . Wallet $ walletId
          }
    let
      go :: Connection -> ExceptT CliError IO ()
      go connection =
        do
          status <- receiveStatus connection
          when verbose
            $ liftIO . putStrLn $ "[runContract] Instance " <> show (unContractInstanceId instanceId) <> " received " <> show status <> "."
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


call :: MonadError CliError m
     => MonadIO m
     => ToJSON a
     => PabAccess
     -> ContractInstanceId
     -> String
     -> a
     -> m ()
call PabAccess{..} instanceId endpoint arguments =
  let
    PabClient{..} = client
    InstanceClient{..} = instanceClient instanceId
  in
    liftCliIO
      . runApi
      . callInstanceEndpoint endpoint
      $ toJSON arguments
