{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Test.Integration.Marlowe.Script
  ( Commands
  , ContractRef
  , ContractState(..)
  , MarloweScript
  , Party
  , PartyType(..)
  , V1ContractBuilder
  , Wallet
  , adaIsDeposited
  , allocateParty
  , allocateWallet
  , applyInput
  , applyInputs
  , assert
  , assertMsg
  , buildV1ApplyInputs
  , buildV1Contract
  , buildV1Contract_
  , choiceIsMade
  , choose
  , close
  , create
  , depositAda
  , depositToken
  , enumChoiceIsMade
  , getAddress
  , getAllContracts
  , getContract
  , getContractBalance
  , getContractDatum
  , getContractState
  , getContractTotalBalance
  , getContracts
  , getPaymentKeyHash
  , getWalletBalance
  , if_
  , isNotified
  , let_
  , mkChoiceId
  , notify
  , payAda
  , payToken
  , runMarloweScript
  , submit
  , submit'
  , toAccount
  , toParty
  , tokenIsDeposited
  , when_
  , withMetadata
  , withdraw
  ) where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , CardanoEra(..)
  , CtxTx
  , MultiAssetSupportedInEra(..)
  , ShelleyBasedEra(..)
  , ShelleyWitnessSigningKey(..)
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxOut(..)
  , TxOutDatum(..)
  , calculateMinimumUTxO
  , deserialiseFromTextEnvelope
  , selectLovelace
  , signShelleyTransaction
  )
import Cardano.Api.Shelley (ReferenceScript(..))
import Control.Applicative (liftA2)
import Control.Concurrent.Async.Lifted (concurrently, mapConcurrently)
import Control.Exception (Exception, throwIO)
import Control.Monad (mfilter, when, (<=<))
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.RWS (RWS, RWST(..), execRWS)
import Control.Monad.Writer (tell)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (Max(..), Min(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Some (Some(Some))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Data.Void (absurd)
import Data.Word (Word64)
import qualified Language.Marlowe as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Protocol.Sync.Client
  (ClientStFollow(..), ClientStIdle(..), ClientStInit(..), ClientStNext(..), ClientStWait(..), MarloweSyncClient(..))
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoAddressInEra, fromCardanoLovelace, fromCardanoTxOutValue, toCardanoAddressInEra, toCardanoTxOutValue)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , AssetId(..)
  , Assets(..)
  , BlockHeader
  , ChainSyncQuery(GetProtocolParameters)
  , DatumHash(..)
  , Metadata
  , PaymentKeyHash(..)
  , PolicyId(..)
  , TokenName
  , Tokens(..)
  , TransactionMetadata(..)
  , fromBech32
  , unTokenName
  )
import Language.Marlowe.Runtime.Core.Api
  ( Contract
  , ContractId
  , Datum
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , SomeMarloweVersion(..)
  , TransactionScriptOutput(..)
  , renderContractId
  )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.Discovery.Api as Discovery
import Language.Marlowe.Runtime.History.Api (ContractStep(..), CreateStep(..))
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError
  , ContractCreated(..)
  , CreateError
  , InputsApplied(..)
  , MarloweTxCommand(..)
  , RoleTokensConfig(..)
  , SubmitError
  , WalletAddresses(..)
  , WithdrawError
  , mkMint
  )
import Network.Protocol.Job.Client (liftCommand, liftCommandWait)
import Network.Protocol.Query.Client (liftFoldQuery, liftQuery)
import Plutus.Script.Utils.V2.Scripts (dataHash)
import Plutus.V1.Ledger.Api (fromBuiltin)
import Plutus.V2.Ledger.Api (ToData(..))
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Integration.Marlowe.Local hiding (TestnetException(..))

newtype MarloweScript a = MarloweScript
  { unMarloweScript :: ExceptT ScriptFailure (RWST MarloweRuntime () ScriptState IO) a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

data ContractRef v = ContractRef
  { contractVersion :: MarloweVersion v
  , contractId :: ContractId
  , contractName :: Text
  , roleTokenPolicy :: PolicyId
  }

data ContractState v
  = Closed
  | Active (TransactionScriptOutput v)

data BuildContract v a where
  BuildV1Contract :: V1ContractBuilder (V1.Contract, a) -> BuildContract 'V1 a

type V1ContractBuilderW =
  ( Map TokenName Wallet -- Role assignments
  , Map ByteString V1.Contract -- Continuations
  , Set AssetId -- Native tokens in deposits
  , TransactionMetadata
  )

newtype V1ContractBuilder a = V1ContractBuilder
  { runBuildV1Contract :: RWST UTCTime V1ContractBuilderW () IO a
  } deriving newtype (Functor, Applicative, Monad)

data BuildApplyInputs v where
  BuildV1ApplyInputs :: Party 'V1 -> V1ApplyInputsBuilder -> BuildApplyInputs 'V1

type V1ApplyInputsBuilderW =
  ( [V1.InputContent] -- inputs to apply
  , Maybe (Max UTCTime) -- validity interval lower bound
  , Maybe (Min UTCTime) -- validity interval upper bound
  , TransactionMetadata
  )

newtype V1ApplyInputsBuilder' a = V1ApplyInputsBuilder
  { runV1ApplyInputsBuilder :: RWS (Party 'V1) V1ApplyInputsBuilderW () a
  } deriving newtype (Functor, Applicative, Monad)

type V1ApplyInputsBuilder = V1ApplyInputsBuilder' ()

data Commands a where
  CreateCommand :: Text -> BuildContract v a -> Commands (ContractRef v, a)
  ApplyInputsCommand :: ContractRef v -> BuildApplyInputs v -> Commands ()
  WithdrawCommand :: ContractRef v -> TokenName -> Commands Assets
  MapCommands :: (a -> b) -> Commands a -> Commands b
  ApCommands :: Commands (a -> b) -> Commands a -> Commands b
  PureCommands :: a -> Commands a

instance Functor Commands where
  fmap = MapCommands

instance Applicative Commands where
  pure = PureCommands
  (<*>) = ApCommands

data Wallet = Wallet
  { walletName :: Text
  , walletPaymentKeyPair :: PaymentKeyPair
  } deriving (Show, Eq, Ord)

data ScriptFailure
  = OutOfWallets Text Int
  | AssertFailed (Maybe Text)
  | ContractNotFound ContractId
  | forall v. CreateFailed Text (MarloweVersion v) (CreateError v)
  | forall v. ApplyInputsFailed (ContractRef v) (ApplyInputsError v)
  | forall v. WithdrawFailed (ContractRef v) (WithdrawError v)
  | SubmitFailed SubmitError
  | ApplyClosedV1 (ContractRef 'V1) V1.InputContent
  | InvalidV1Input (ContractRef 'V1) V1.InputContent
  | ContinuationNotFound ByteString

instance Show ScriptFailure where
  show = \case
    OutOfWallets name maxWallets -> "Unable to allocate wallet "
      <> show name
      <> ", maximum number of wallets reached ("
      <> show maxWallets
      <> "). Configure your testnet with more wallets to create more paries."
    AssertFailed Nothing -> "Assertion failed"
    AssertFailed (Just msg) -> "Assertion failed: " <> T.unpack msg
    ContractNotFound contractId -> "Contract not found: " <> show contractId
    CreateFailed name MarloweV1 err ->
      "Failed to create contract " <> show name <> ": " <> show err
    ApplyInputsFailed ContractRef{contractName, contractVersion = MarloweV1} err ->
      "Failed to apply inputs to contract " <> show contractName <> ": " <> show err
    WithdrawFailed ContractRef{contractName, contractVersion = MarloweV1} err ->
      "Failed to withdraw funds from contract " <> show contractName <> ": " <> show err
    SubmitFailed err -> "Failed to submit tx: " <> show err
    ApplyClosedV1 ContractRef{contractName} inputContent ->
      "Attempted to apply input to closed contract " <> show contractName <> ". Input: " <> show inputContent
    InvalidV1Input ContractRef{contractName} inputContent ->
      "Attempted to apply invalid input to contract " <> show contractName <> ". Input: " <> show inputContent
    ContinuationNotFound hash -> "No contract found for hash" <> show (DatumHash hash)

instance Exception ScriptFailure

data ScriptState = ScriptState
  { walletCount :: Int
  , contractNames :: Map ContractId Text
  , v1Continuations :: Map ByteString V1.Contract
  }

-- Running scripts

runMarloweScript :: MarloweScript a -> MarloweRuntime -> IO a
runMarloweScript script runtime = do
  let rwst = runExceptT $ unMarloweScript script
  (result, _, _) <- runRWST rwst runtime initialState
  either throwIO pure result

initialState :: ScriptState
initialState = ScriptState
  { walletCount = 0
  , contractNames = mempty
  , v1Continuations = mempty
  }

-- Wallet management

allocateWallet :: Text -> MarloweScript Wallet
allocateWallet walletName = MarloweScript do
  ws <- asks $ wallets . testnet
  count <- gets walletCount
  when (count >= length ws) $ throwError $ OutOfWallets walletName $ length ws
  modify \s -> s { walletCount = count + 1 }
  pure Wallet
    { walletName
    , walletPaymentKeyPair = ws !! count
    }

-- Commands

submit :: Wallet -> Commands a -> MarloweScript a
submit wallet = fmap snd . submit' wallet

submit' :: Wallet -> Commands a -> MarloweScript (BlockHeader, a)
submit' wallet commands = do
  time <- liftIO getCurrentTime
  (txBodies, a) <- commandsToTxBodies time wallet commands
  txs <- traverse (signTxBody wallet) txBodies
  blockHeader <- submitTxs txs
  pure (blockHeader, a)

commandsToTxBodies :: UTCTime -> Wallet -> Commands a -> MarloweScript ([TxBody BabbageEra], a)
commandsToTxBodies time wallet = \case
  CreateCommand contractName (BuildV1Contract builder) -> do
    MarloweRuntime{..} <- MarloweScript ask
    ((contract, a), (), (roles, continuations, nativeTokens, metadata)) <- liftIO $ runRWST (runBuildV1Contract builder) time ()
    MarloweScript $ modify \state -> state { v1Continuations = v1Continuations state <> continuations }
    protocolParameters <- liftIO
      $ fromRight (error "failed to load protocol parameters from chain seek")
      <$> runChainSyncQueryClient (liftQuery GetProtocolParameters)
    let contractVersion = MarloweV1
    walletAddresses <- getWalletAddresses wallet
    mintPairs <- Map.toList <$> for roles (fmap (, Left 1) . getAddressIO)
    let
      roleTokensConfig = case mintPairs of
        [] -> RoleTokensNone
        x : xs -> RoleTokensMint $ mkMint $ x :| xs
      dummyAddress = fromJust $ toCardanoAddressInEra BabbageEra $ changeAddress walletAddresses
      -- Construct an asset that contains all the native tokens in the contract
      -- (this is the maximum theoretical min UTxO requirement for the
      -- contract).
      assetsForMinLovelace = Assets 1_000_000 $ Tokens $ Map.fromSet (const 1) nativeTokens
      txOutValueForMinLovelace = fromJust $ toCardanoTxOutValue MultiAssetInBabbageEra assetsForMinLovelace
      txOutForMinLovelace = TxOut dummyAddress txOutValueForMinLovelace TxOutDatumNone ReferenceScriptNone
      minLovelace = fromCardanoLovelace
        $ selectLovelace
        $ fromRight (error "calculateMinimumUTxO failed (this should be impossible)")
        $ calculateMinimumUTxO ShelleyBasedEraBabbage txOutForMinLovelace protocolParameters
    result <- liftIO $ runTxJobClient $ liftCommand $ Create
      Nothing
      contractVersion
      walletAddresses
      roleTokensConfig
      metadata
      (minLovelace + 1_000_000)
      contract
    case result of
      Left err -> MarloweScript $ throwError $ CreateFailed contractName contractVersion err
      Right ContractCreated{contractId, txBody, rolesCurrency = roleTokenPolicy} ->
        pure ([txBody], (ContractRef{..}, a))

  ApplyInputsCommand ref@ContractRef{..} (BuildV1ApplyInputs party builder) -> do
    MarloweRuntime{..} <- MarloweScript ask
    let
      ((), (inputs, invalidBefore, invalidHereafter, metadata)) =
        execRWS (runV1ApplyInputsBuilder builder) party ()
    let invalidBefore' = getMax <$> invalidBefore
    let invalidHereafter' = getMin <$> invalidHereafter
    inputs' <- merkleizeV1Inputs ref invalidBefore' invalidHereafter' inputs
    walletAddresses <- getWalletAddresses wallet
    result <- liftIO $ runTxJobClient $ liftCommand $ ApplyInputs
      contractVersion
      walletAddresses
      contractId
      metadata
      invalidBefore'
      invalidHereafter'
      inputs'
    case result of
      Left err -> MarloweScript $ throwError $ ApplyInputsFailed ref err
      Right InputsApplied{txBody} -> pure ([txBody], ())

  WithdrawCommand ref@ContractRef{..} role -> do
    MarloweRuntime{..} <- MarloweScript ask
    walletAddresses <- getWalletAddresses wallet
    result <- liftIO
      $ runTxJobClient
      $ liftCommand
      $ Withdraw contractVersion walletAddresses contractId role
    case result of
      Left err -> MarloweScript $ throwError $ WithdrawFailed ref err
      Right txBody -> pure
        ( [txBody]
        , foldMap (getWithdrawnAssets (changeAddress walletAddresses) roleTokenPolicy role) case txBody of
            TxBody TxBodyContent{txOuts} -> txOuts
        )

  MapCommands f commands -> fmap f <$> commandsToTxBodies time wallet commands

  ApCommands f commands -> MarloweScript do
    ((txBodies1, f'), (txBodies2, a)) <- concurrently
      (unMarloweScript $ commandsToTxBodies time wallet f)
      (unMarloweScript $ commandsToTxBodies time wallet commands)
    pure (txBodies1 <> txBodies2, f' a)

  PureCommands a -> pure ([], a)

getWithdrawnAssets :: Address -> PolicyId -> TokenName -> TxOut CtxTx BabbageEra -> Assets
getWithdrawnAssets changeAddress roleTokenPolicy role (TxOut address value _ _)
  | changeAddress == fromCardanoAddressInEra BabbageEra address =
      excludeRoleToken $ fromCardanoTxOutValue value
  | otherwise = mempty
  where
    excludeRoleToken Assets{..} = Assets ada
      $ Tokens
      $ Map.delete (AssetId roleTokenPolicy role)
      $ unTokens tokens

merkleizeV1Inputs
  :: ContractRef 'V1
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> [V1.InputContent]
  -> MarloweScript [V1.Input]
merkleizeV1Inputs contractRef invalidBefore invalidHereafter inputs = do
  contractState <- getContractState contractRef <&> \case
    Closed -> Nothing
    Active TransactionScriptOutput{..} -> Just datum
  snd <$> foldlM
    (\(state, acc) input -> fmap (:acc) <$> merkleizeV1InputState contractRef invalidBefore invalidHereafter state input)
    (contractState, [])
    inputs

merkleizeV1InputState
  :: ContractRef 'V1
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Maybe V1.MarloweData
  -> V1.InputContent
  -> MarloweScript (Maybe V1.MarloweData, V1.Input)
merkleizeV1InputState ref invalidBefore invalidHereafter = \case
  Nothing -> \inputContent -> MarloweScript $ throwError $ ApplyClosedV1 ref inputContent
  Just datum -> \inputContent -> findHashForInput inputContent datum
  where

  findHashForInput inputContent V1.MarloweData{..} = case marloweContract of
    V1.When cases _ _ -> do
      results <- traverse (findHashForInputInCase marloweParams inputContent marloweState) cases
      case results of
        [] -> MarloweScript $ throwError $ InvalidV1Input ref inputContent
        x : _ -> pure x
    _ -> MarloweScript $ throwError $ InvalidV1Input ref inputContent

  findHashForInputInCase marloweParams inputContent state = \case
    V1.Case action contract -> returnIfApplies action $ Left contract
    V1.MerkleizedCase action hash -> returnIfApplies action $ Right hash
    where
      env = V1.Environment
        ( maybe 0 utcTimeToPOSIXTime invalidBefore
        , maybe (fromIntegral $ maxBound @Word64) utcTimeToPOSIXTime invalidHereafter
        )
      notClosed V1.Close = False
      notClosed _ = True
      returnIfApplies action mHash = first (mfilter $ notClosed . V1.marloweContract)
        <$> returnIfApplies' action mHash
      returnIfApplies' action mHash = case V1.applyAction env state inputContent action of
        V1.NotAppliedAction -> MarloweScript $ throwError $ InvalidV1Input ref inputContent
        V1.AppliedAction _ marloweState -> case mHash of
          Left marloweContract -> pure (Just V1.MarloweData{..}, V1.NormalInput inputContent)
          Right hash -> do
            continuations <- MarloweScript $ gets v1Continuations
            marloweContract <- case Map.lookup (PV2.fromBuiltin hash) continuations of
              Nothing -> MarloweScript $ throwError $ ContinuationNotFound (PV2.fromBuiltin hash)
              Just c -> pure c
            pure (Just V1.MarloweData{..}, V1.MerkleizedInput inputContent hash marloweContract)

utcTimeToPOSIXTime :: UTCTime -> PV2.POSIXTime
utcTimeToPOSIXTime = PV2.POSIXTime . floor . (* 1000) . utcTimeToPOSIXSeconds

signTxBody :: Wallet -> TxBody BabbageEra -> MarloweScript (Tx BabbageEra)
signTxBody wallet txBody = signShelleyTransaction txBody . pure <$> getSigningKey wallet

submitTxs :: [Tx BabbageEra] -> MarloweScript BlockHeader
submitTxs = fmap maximum . MarloweScript . mapConcurrently (unMarloweScript . submitTx)

submitTx :: Tx BabbageEra -> MarloweScript BlockHeader
submitTx tx = do
  MarloweRuntime{..} <- MarloweScript ask
  result <- liftIO $ runTxJobClient $ liftCommandWait $ Submit tx
  case result of
    Left err -> MarloweScript $ throwError $ SubmitFailed err
    Right block -> pure block

create :: Text -> BuildContract v a -> Commands (ContractRef v, a)
create = CreateCommand

applyInputs :: ContractRef v -> BuildApplyInputs v -> Commands ()
applyInputs = ApplyInputsCommand

withdraw :: ContractRef v -> TokenName -> Commands Assets
withdraw = WithdrawCommand

-- Building contracts

data PartyType
  = ByAddress
  | ByRole TokenName

data Party v where
  V1Party :: V1.AccountId -> Party 'V1

data Payee v where
  V1Payee :: V1.Payee -> Payee 'V1

withMetadata :: Word64 -> Metadata -> V1ContractBuilder ()
withMetadata key metadata = V1ContractBuilder do
  tell (mempty, mempty, mempty, TransactionMetadata $ Map.singleton key metadata)

allocateParty :: Wallet -> PartyType -> V1ContractBuilder (Party 'V1)
allocateParty wallet = \case
  ByAddress -> V1ContractBuilder do
    paymentKeyHash <- getPaymentKeyHashIO wallet
    let credential = PV2.PubKeyCredential $ PV2.PubKeyHash $ PV2.toBuiltin $ unPaymentKeyHash paymentKeyHash
    pure $ V1Party $ V1.Address V1.testnet $ PV2.Address credential Nothing
  ByRole role -> V1ContractBuilder do
    tell (Map.singleton role wallet, mempty, mempty, mempty)
    pure $ V1Party $ V1.Role $ PV2.TokenName $ PV2.toBuiltin $ unTokenName role

toParty :: Party v -> Payee v
toParty (V1Party party) = V1Payee $ V1.Party party

toAccount :: Party v -> Payee v
toAccount (V1Party party) = V1Payee $ V1.Account party

buildV1Contract_ :: V1ContractBuilder V1.Contract -> BuildContract 'V1 ()
buildV1Contract_ = buildV1Contract . fmap (,())

buildV1Contract :: V1ContractBuilder (V1.Contract, a) -> BuildContract 'V1 a
buildV1Contract = BuildV1Contract

close :: V1ContractBuilder V1.Contract
close = pure V1.Close

payAda
  :: Party 'V1
  -> Payee 'V1
  -> V1.Value V1.Observation
  -> V1ContractBuilder V1.Contract
  -> V1ContractBuilder V1.Contract
payAda (V1Party accountId) (V1Payee payee) value =
  fmap $ V1.Pay accountId payee (V1.Token "" "") value

payToken
  :: Party 'V1
  -> Payee 'V1
  -> AssetId
  -> V1.Value V1.Observation
  -> V1ContractBuilder V1.Contract
  -> V1ContractBuilder V1.Contract
payToken (V1Party accountId) (V1Payee payee) AssetId{..} value =
  fmap $ V1.Pay
    accountId
    payee
    ( V1.Token
      (PV2.CurrencySymbol $ PV2.toBuiltin $ unPolicyId policyId)
      (PV2.TokenName $ PV2.toBuiltin $ unTokenName tokenName)
    )
    value

if_
  :: V1.Observation
  -> V1ContractBuilder V1.Contract
  -> V1ContractBuilder V1.Contract
  -> V1ContractBuilder V1.Contract
if_ = liftA2 . V1.If

when_
  :: [(V1ContractBuilder V1.Action, V1ContractBuilder V1.Contract)]
  -> NominalDiffTime
  -> V1ContractBuilder V1.Contract
  -> V1ContractBuilder V1.Contract
when_ cases timeout timeoutContinuation = V1ContractBuilder do
  cases' <- traverse handleCase cases
  startTime <- ask
  V1.When
    cases'
    (utcTimeToPOSIXTime $ addUTCTime timeout startTime)
    <$> runBuildV1Contract timeoutContinuation
  where
    handleCase (mAction, mContract) = do
      action <- runBuildV1Contract mAction
      contract <- runBuildV1Contract mContract
      let contractHash = dataHash $ toBuiltinData contract
      tell (mempty, Map.singleton (fromBuiltin contractHash) contract, mempty, mempty)
      pure $ V1.MerkleizedCase action $ dataHash $ toBuiltinData contract

let_
  :: V1.ValueId
  -> V1.Value V1.Observation
  -> V1ContractBuilder V1.Contract
  -> V1ContractBuilder V1.Contract
let_ valueId = fmap . V1.Let valueId

adaIsDeposited :: Party 'V1 -> Party 'V1 -> V1.Value V1.Observation -> V1ContractBuilder V1.Action
adaIsDeposited (V1Party account) (V1Party party) = pure . V1.Deposit account party (V1.Token "" "")

tokenIsDeposited
  :: Party 'V1
  -> Party 'V1
  -> AssetId
  -> V1.Value V1.Observation
  -> V1ContractBuilder V1.Action
tokenIsDeposited (V1Party account) (V1Party party) AssetId{..} value = V1ContractBuilder do
  tell (mempty, mempty, Set.singleton AssetId{..}, mempty)
  pure $ V1.Deposit
    account
    party
    ( V1.Token
      (PV2.CurrencySymbol $ PV2.toBuiltin $ unPolicyId policyId)
      (PV2.TokenName $ PV2.toBuiltin $ unTokenName tokenName)
    )
    value

enumChoiceIsMade :: V1.ChoiceId -> Integer -> V1ContractBuilder V1.Action
enumChoiceIsMade choiceId i = choiceIsMade choiceId [V1.Bound i i]

choiceIsMade :: V1.ChoiceId -> [V1.Bound] -> V1ContractBuilder V1.Action
choiceIsMade choiceId = pure . V1.Choice choiceId

mkChoiceId :: String -> Party v -> V1.ChoiceId
mkChoiceId name (V1Party party) = V1.ChoiceId (fromString name) party

isNotified :: V1.Observation -> V1ContractBuilder V1.Action
isNotified = pure . V1.Notify

-- applying inputs

buildV1ApplyInputs :: Party 'V1 -> V1ApplyInputsBuilder -> BuildApplyInputs 'V1
buildV1ApplyInputs = BuildV1ApplyInputs

applyInput :: V1.InputContent -> V1ApplyInputsBuilder
applyInput inputContent = V1ApplyInputsBuilder $ tell ([inputContent], Nothing, Nothing, mempty)

depositAda :: Party 'V1 -> Integer -> V1ApplyInputsBuilder
depositAda (V1Party account) value = do
  (V1Party party) <- V1ApplyInputsBuilder ask
  applyInput $ V1.IDeposit account party (V1.Token "" "") value

depositToken :: Party 'V1 -> AssetId -> Integer -> V1ApplyInputsBuilder
depositToken (V1Party account) AssetId{..} value = do
  (V1Party party) <- V1ApplyInputsBuilder ask
  applyInput $ V1.IDeposit
    account
    party
    ( V1.Token
      (PV2.CurrencySymbol $ PV2.toBuiltin $ unPolicyId policyId)
      (PV2.TokenName $ PV2.toBuiltin $ unTokenName tokenName)
    )
    value

choose :: String -> Integer -> V1ApplyInputsBuilder
choose choiceName chosenNum = do
  (V1Party party) <- V1ApplyInputsBuilder ask
  applyInput $ V1.IChoice (V1.ChoiceId (fromString choiceName) party) chosenNum

notify :: V1ApplyInputsBuilder
notify = applyInput V1.INotify

-- Querying wallets

getSigningKey :: Wallet -> MarloweScript ShelleyWitnessSigningKey
getSigningKey Wallet{..} = do
  textEnvelope <- liftIO $ either error id <$> eitherDecodeFileStrict (paymentSKey walletPaymentKeyPair)
  pure
    $ WitnessGenesisUTxOKey
    $ fromRight (error "Failed to parse signing key")
    $ deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) textEnvelope

getWalletAddresses :: Wallet -> MarloweScript WalletAddresses
getWalletAddresses wallet = do
  changeAddress <- getAddress wallet
  pure $ WalletAddresses changeAddress mempty mempty

getAddress :: Wallet -> MarloweScript Address
getAddress = MarloweScript . getAddressIO

getPaymentKeyHash :: Wallet -> MarloweScript PaymentKeyHash
getPaymentKeyHash = MarloweScript . getPaymentKeyHashIO

getAddressIO :: MonadIO m => Wallet -> m Address
getAddressIO Wallet{..} = fromJust . fromBech32 . T.pack <$> execCli
  [ "address", "build"
  , "--payment-verification-key-file", paymentVKey walletPaymentKeyPair
  , "--testnet-magic", "1" -- the testnetMagic doesn't actually matter. All that matters is that it's testnet and not mainnet.
  ]

getPaymentKeyHashIO :: MonadIO m => Wallet -> m PaymentKeyHash
getPaymentKeyHashIO Wallet{..} = fromString . T.unpack . T.strip . T.pack <$> execCli
  [ "address", "key-hash"
  , "--payment-verification-key-file", paymentVKey walletPaymentKeyPair
  ]

getWalletBalance :: Wallet -> MarloweScript Assets
getWalletBalance = error "not implemented"

getContracts :: Wallet -> MarloweScript [Some ContractRef]
getContracts _ = error "not implemented"

-- Querying contracts

getAllContracts :: MarloweScript [Some ContractRef]
getAllContracts = MarloweScript do
  MarloweRuntime{..} <- ask
  names <- gets contractNames
  result <- liftIO $ either absurd id <$> runDiscoveryQueryClient (liftFoldQuery Discovery.GetContractHeaders)
  pure $ result <&> \Discovery.ContractHeader{..} -> case marloweVersion of
    SomeMarloweVersion contractVersion ->
      let
        contractName = fromMaybe (renderContractId contractId) $ Map.lookup contractId names
        roleTokenPolicy = rolesCurrency
       in
        Some $ ContractRef{..}

getContractState :: ContractRef v -> MarloweScript (ContractState v)
getContractState ContractRef{..} = MarloweScript do
  MarloweRuntime{..} <- ask
  ExceptT $ liftIO $ runHistorySyncClient client
  where
    client = MarloweSyncClient $ pure $ SendMsgFollowContract contractId ClientStFollow
      { recvMsgContractNotFound = pure $ Left $ ContractNotFound contractId
      , recvMsgContractFound = \block MarloweV1 CreateStep{..} -> case contractVersion of
          MarloweV1 -> pure $ clientIdle $ pure (block, Active createOutput)
      }

    clientIdle
      :: NonEmpty (BlockHeader, ContractState v)
      -> ClientStIdle v IO (Either ScriptFailure (ContractState v))
    clientIdle states = SendMsgRequestNext ClientStNext
      { recvMsgRollForward = \block steps -> do
          let
            foldStep Closed _ = Closed
            foldStep state RedeemPayout{} = state
            foldStep _ (ApplyTransaction Core.Transaction{output}) = maybe Closed Active $ Core.scriptOutput output
          let state = foldl' foldStep (snd $ NE.head states) steps
          pure case state of
            Closed -> SendMsgDone $ Right Closed
            _ -> clientIdle $ (block, state) <| states
      , recvMsgRollBackward = \block -> case NE.dropWhile ((> block) . fst) states of
          [] -> pure $ SendMsgDone $ Left $ ContractNotFound contractId
          x : xs -> pure $ clientIdle $ x :| xs
      , recvMsgRollBackCreation = pure $ Left $ ContractNotFound contractId
      , recvMsgWait = pure $ SendMsgCancel $ SendMsgDone $ Right $ snd $ NE.head states
      }

getContractBalance :: forall v. Wallet -> ContractRef v -> MarloweScript Assets
getContractBalance _ = maybe (pure mempty) extractAllocatedBalance <=< getContractDatum
  where
    extractAllocatedBalance :: Datum v -> MarloweScript Assets
    extractAllocatedBalance = error "not implemented"

getContractTotalBalance :: ContractRef v -> MarloweScript Assets
getContractTotalBalance ref = getContractState ref <&> \case
  Closed -> mempty
  Active TransactionScriptOutput{..} -> assets

getContract :: ContractRef v -> MarloweScript (Contract v)
getContract ref = getContractState ref <&> \case
  Closed -> case contractVersion ref of
    MarloweV1 -> V1.Close
  Active TransactionScriptOutput{..} -> case contractVersion ref of
    MarloweV1 -> V1.marloweContract datum

getContractDatum :: ContractRef v -> MarloweScript (Maybe (Datum v))
getContractDatum ref = getContractState ref <&> \case
  Closed -> Nothing
  Active TransactionScriptOutput{..} -> Just datum

-- Assertions

assert :: Bool -> MarloweScript ()
assert True = pure ()
assert _ = MarloweScript $ throwError $ AssertFailed Nothing

assertMsg :: Text -> Bool -> MarloweScript ()
assertMsg _ True = pure ()
assertMsg msg _ = MarloweScript $ throwError $ AssertFailed $ Just msg
