{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.App.Types (
  TxBodyInEraWithReferenceScripts (..),
  TxInEraWithReferenceScripts (..),
  App,
  Client (..),
  Config (..),
  FinishOnClose (..),
  FinishOnWait (..),
  MarloweRequest (..),
  MarloweResponse (..),
  PollingFrequency (..),
  RunClient,
  Services (..),
  mkBody,
) where

import Control.Applicative (Alternative)
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.With (MonadWith (..))
import Data.Default (Default (..))
import Data.Functor ((<&>))
import Data.GeneralAllocate (GeneralAllocate (..), GeneralAllocated (..))
import Data.String (fromString)
import Language.Marlowe (POSIXTime (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  ChainSyncCommand,
  Lovelace (..),
  RuntimeChainSeekClient,
  TokenName,
  TransactionMetadata,
  TxId,
  TxOutRef,
  fromBech32,
  fromJSONEncodedTransactionMetadata,
  parseTxOutRef,
  toBech32,
 )
import Language.Marlowe.Runtime.Core.Api (
  ContractId,
  IsMarloweVersion (Contract, Inputs),
  MarloweVersionTag (V1),
  Payout (..),
  Transaction (
    Transaction,
    blockHeader,
    contractId,
    inputs,
    output,
    transactionId,
    validityLowerBound,
    validityUpperBound
  ),
  TransactionOutput (payouts, scriptOutput),
  TransactionScriptOutput (..),
  renderContractId,
 )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.History.Api (
  ContractStep (..),
  CreateStep (..),
  RedeemStep (RedeemStep, datum, redeemingTx, utxo),
 )
import Network.Protocol.Job.Client (JobClient)
import Network.Socket (HostName, PortNumber)

import Cardano.Api (AnyCardanoEra (..))
import Cardano.Api.Shelley (
  ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra),
 )
import qualified Cardano.Api.Shelley as C
import Control.Monad.Trans.Marlowe (MarloweT)
import Control.Monad.Trans.Marlowe.Class (MonadMarlowe (..))
import qualified Data.Aeson.Types as A (
  FromJSON (parseJSON),
  Parser,
  ToJSON (toJSON),
  Value (String),
  object,
  parseFail,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Foldable (fold)
import qualified Data.Map.Strict as M (Map, map, mapKeys)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T (Text)
import Data.Time.Units (Second)
import Language.Marlowe.Protocol.Client (hoistMarloweRuntimeClient)
import Language.Marlowe.Protocol.Query.Types (ContractFilter)
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS (Transaction)

data TxBodyInEraWithReferenceScripts where
  TxBodyInEraWithReferenceScripts
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> C.TxBody era -> TxBodyInEraWithReferenceScripts

data TxInEraWithReferenceScripts where
  TxInEraWithReferenceScripts
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> C.Tx era -> TxInEraWithReferenceScripts

type App = ExceptT String IO

data Config = Config
  { chainSeekHost :: HostName
  , chainSeekSyncPort :: PortNumber
  , chainSeekCommandPort :: PortNumber
  , runtimeHost :: HostName
  , runtimePort :: PortNumber
  , timeoutSeconds :: Int
  , buildSeconds :: Int
  , confirmSeconds :: Int
  , retrySeconds :: Int
  , retryLimit :: Int
  }
  deriving (Read, Show)

instance Default Config where
  def =
    Config
      { chainSeekHost = "127.0.0.1"
      , chainSeekSyncPort = 3715
      , chainSeekCommandPort = 3720
      , runtimeHost = "127.0.0.1"
      , runtimePort = 3700
      , timeoutSeconds = 900
      , buildSeconds = 3
      , confirmSeconds = 3
      , retrySeconds = 10
      , retryLimit = 5
      }

data Services m = Services
  { runChainSeekCommandClient :: RunClient m (JobClient ChainSyncCommand)
  , runChainSeekSyncClient :: RunClient m RuntimeChainSeekClient
  }

-- | A monad type for Marlowe Runtime.Client programs.
newtype Client a = Client {runClient :: MarloweT (ReaderT (Services IO) IO) a}
  deriving newtype
    (Alternative, Applicative, Functor, Monad, MonadBase IO, MonadBaseControl IO, MonadFail, MonadFix, MonadIO)

instance MonadWith Client where
  type WithException Client = WithException (MarloweT (ReaderT (Services IO) IO))
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate Client (WithException Client) releaseReturn b a
    -> (a -> Client b)
    -> Client (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = Client $ do
    stateThreadingGeneralWith (GeneralAllocate allocA') $ runClient . go
    where
      allocA'
        :: (forall x. MarloweT (ReaderT (Services IO) IO) x -> MarloweT (ReaderT (Services IO) IO) x)
        -> MarloweT
            (ReaderT (Services IO) IO)
            ( GeneralAllocated
                (MarloweT (ReaderT (Services IO) IO))
                (WithException (MarloweT (ReaderT (Services IO) IO)))
                releaseReturn
                b
                a
            )
      allocA' restore =
        runClient (allocA restore') <&> \case
          GeneralAllocated a releaseA -> GeneralAllocated a $ runClient . releaseA
        where
          restore' :: forall x. Client x -> Client x
          restore' = Client . restore . runClient

instance MonadMarlowe Client where
  runMarloweRuntimeClient client = Client $ runMarloweRuntimeClient $ hoistMarloweRuntimeClient runClient client

-- | A function signature for running a client for some protocol in some monad m.
type RunClient m client = forall a. client m a -> m a

data MarloweRequest v
  = ListContracts
      { reqFilter :: ContractFilter
      }
  | ListHeaders
      { reqFilter :: ContractFilter
      }
  | Get
      { reqContractId :: ContractId
      }
  | Create
      { reqContract :: Contract v
      , reqRoles :: M.Map TokenName Address
      , reqMinUtxo :: Lovelace
      , --  , reqStakeAddress :: Maybe StakeCredential
        reqMetadata :: TransactionMetadata
      , reqAddresses :: [Address]
      , reqChange :: Address
      , reqCollateral :: [TxOutRef]
      }
  | Apply
      { reqContractId :: ContractId
      , reqInputs :: Inputs v
      , reqValidityLowerBound :: Maybe POSIXTime
      , reqValidityUpperBound :: Maybe POSIXTime
      , reqMetadata :: TransactionMetadata
      , reqAddresses :: [Address]
      , reqChange :: Address
      , reqCollateral :: [TxOutRef]
      }
  | Withdraw
      { reqPayouts :: Set TxOutRef
      , reqAddresses :: [Address]
      , reqChange :: Address
      , reqCollateral :: [TxOutRef]
      }
  | forall era.
    Sign
      { reqTxEra :: C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
      , reqTxBody :: C.TxBody era
      , reqPaymentKeys :: [C.SigningKey C.PaymentKey]
      , reqPaymentExtendedKeys :: [C.SigningKey C.PaymentExtendedKey]
      }
  | forall era.
    Submit
      { reqTxEra :: C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
      , reqTx :: C.Tx era
      , reqPollingSeconds :: Int
      }

{-
  | Wait
    { reqTxId :: TxId
    , reqPollingSeconds :: Int
    }
-}

instance A.FromJSON (MarloweRequest 'V1) where
  parseJSON =
    A.withObject "MarloweRequest" $
      \o ->
        (o A..: "request" :: A.Parser String)
          >>= \case
            "list" -> ListContracts . fold <$> o A..:? "filter"
            "headers" -> ListHeaders . fold <$> o A..:? "filter"
            "get" -> do
              reqContractId <- fromString <$> o A..: "contractId"
              pure Get{..}
            "create" -> do
              reqContract <- o A..: "contract"
              reqRoles <- M.mapKeys fromString . M.map fromString <$> (o A..: "roles" :: A.Parser (M.Map String String))
              reqMinUtxo <- Lovelace <$> o A..: "minUtxo"
              reqMetadata <- metadataFromJSON =<< o A..: "metadata"
              reqAddresses <- mapM addressFromJSON =<< o A..: "addresses"
              reqChange <- addressFromJSON =<< o A..: "change"
              reqCollateral <- fmap fromString <$> o A..: "collateral"
              pure Create{..}
            "apply" -> do
              reqContractId <- fromString <$> o A..: "contractId"
              reqInputs <- o A..: "inputs"
              reqValidityLowerBound <- Just . POSIXTime <$> o A..: "validityLowerBound"
              reqValidityUpperBound <- Just . POSIXTime <$> o A..: "validityUpperBound"
              reqMetadata <- metadataFromJSON =<< o A..: "metadata"
              reqAddresses <- mapM addressFromJSON =<< o A..: "addresses"
              reqChange <- addressFromJSON =<< o A..: "change"
              reqCollateral <- fmap fromString <$> o A..: "collateral"
              pure Apply{..}
            "withdraw" -> do
              reqPayouts <-
                Set.fromList <$> (traverse (maybe (fail "invalid tx out ref syntax") pure . parseTxOutRef) =<< o A..: "payouts")
              reqAddresses <- mapM addressFromJSON =<< o A..: "addresses"
              reqChange <- addressFromJSON =<< o A..: "change"
              reqCollateral <- fmap fromString <$> o A..: "collateral"
              pure Withdraw{..}
            "sign" -> do
              AnyCardanoEra reqTxEra' <- o A..: "era"
              case reqTxEra' of
                C.BabbageEra -> do
                  let reqTxEra = ReferenceTxInsScriptsInlineDatumsInBabbageEra
                  reqTxBody <- textEnvelopeFromJSON (C.AsTxBody C.AsBabbageEra) =<< o A..: "body"
                  reqPaymentKeys <- mapM (textEnvelopeFromJSON $ C.AsSigningKey C.AsPaymentKey) =<< o A..: "paymentKeys"
                  reqPaymentExtendedKeys <-
                    mapM (textEnvelopeFromJSON $ C.AsSigningKey C.AsPaymentExtendedKey) =<< o A..: "paymentExtendedKeys"
                  pure Sign{..}
                _ -> fail $ "Unsupported era " <> show reqTxEra'
            "submit" -> do
              AnyCardanoEra reqTxEra' <- o A..: "era"
              case reqTxEra' of
                C.BabbageEra -> do
                  let reqTxEra = ReferenceTxInsScriptsInlineDatumsInBabbageEra
                  reqTx <- textEnvelopeFromJSON (C.AsTx C.AsBabbageEra) =<< o A..: "tx"
                  reqPollingSeconds <- o A..: "pollingSeconds"
                  pure Submit{..}
                _ -> fail $ "Unsupported era " <> show reqTxEra'
            {-
                        "wait" -> do
                                    reqTxId <- fromString <$> o A..: "txId"
                                    reqPollingSeconds <- o A..: "pollingSeconds"
                                    pure Wait{..}
            -}
            request -> fail $ "Invalid request: " <> request <> "."

instance A.ToJSON (MarloweRequest 'V1) where
  toJSON ListContracts{..} =
    A.object
      [ "request" A..= ("list" :: String)
      , "filter" A..= reqFilter
      ]
  toJSON ListHeaders{..} =
    A.object
      [ "request" A..= ("headers" :: String)
      , "filter" A..= reqFilter
      ]
  toJSON Get{..} =
    A.object
      [ "request" A..= ("get" :: String)
      , "contractId" A..= renderContractId reqContractId
      ]
  toJSON Create{..} =
    A.object
      [ "request" A..= ("create" :: String)
      , "contract" A..= reqContract
      , "roles" A..= M.mapKeys show reqRoles
      , "minUtxo" A..= unLovelace reqMinUtxo
      , "metadata" A..= reqMetadata
      , "addresses" A..= fmap addressToJSON reqAddresses
      , "change" A..= addressToJSON reqChange
      , "collateral" A..= reqCollateral
      ]
  toJSON Apply{..} =
    A.object
      [ "request" A..= ("apply" :: String)
      , "inputs" A..= reqInputs
      , "validityLowerBound" A..= fmap getPOSIXTime reqValidityLowerBound
      , "validityUpperBound" A..= fmap getPOSIXTime reqValidityUpperBound
      , "metadata" A..= reqMetadata
      , "addresses" A..= fmap addressToJSON reqAddresses
      , "change" A..= addressToJSON reqChange
      , "collateral" A..= reqCollateral
      ]
  toJSON Withdraw{..} =
    A.object
      [ "request" A..= ("withdraw" :: String)
      , "payouts" A..= reqPayouts
      , "addresses" A..= fmap addressToJSON reqAddresses
      , "change" A..= addressToJSON reqChange
      , "collateral" A..= reqCollateral
      ]
  toJSON Sign{..} =
    A.object
      [ "request" A..= ("sign" :: String)
      , case reqTxEra of
          ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "era" A..= C.BabbageEra
      , case reqTxEra of
          ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "body" A..= textEnvelopeToJSON reqTxBody
      , "paymentKeys" A..= fmap textEnvelopeToJSON reqPaymentKeys
      , "paymentExtendedKeys" A..= fmap textEnvelopeToJSON reqPaymentExtendedKeys
      ]
  toJSON Submit{..} =
    A.object
      [ "request" A..= ("submit" :: String)
      , case reqTxEra of
          ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "era" A..= C.BabbageEra
      , case reqTxEra of
          ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "tx" A..= textEnvelopeToJSON reqTx
      ]

{-
  toJSON Wait{..} =
    A.object
      [ "request" A..= ("wait" :: String)
      , "txId" A..= reqTxId
      , "pollingSeconds" A..= reqPollingSeconds
      ]
-}

data MarloweResponse v
  = Contracts
      { resContractIds :: [ContractId]
      }
  | Headers
      { resContractHeaders :: [ContractHeader]
      }
  | FollowResult
      { resResult :: Bool
      }
  | Info
      { resCreation :: CreateStep v
      , resSteps :: [ContractStep v]
      }
  | forall era.
    Body
      { resTxEra :: C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
      , resContractId :: Maybe ContractId
      , resTxId :: TxId
      , resTxBody :: C.TxBody era
      }
  | forall era.
    Tx
      { resTxEra :: C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
      , resTxId :: TxId
      , resTx :: C.Tx era
      }
  | TxId
      { resTxId :: TxId
      }
  | TxInfo
      { resTransaction :: CS.Transaction
      }

instance A.ToJSON (MarloweResponse 'V1) where
  toJSON Contracts{..} =
    A.object
      [ "response" A..= ("contracts" :: String)
      , "contractIds" A..= fmap renderContractId resContractIds
      ]
  toJSON Headers{..} =
    A.object
      [ "response" A..= ("headers" :: String)
      , "contractHeaders" A..= resContractHeaders
      ]
  toJSON FollowResult{..} =
    A.object
      [ "response" A..= ("result" :: String)
      , "result" A..= resResult
      ]
  toJSON Info{..} =
    A.object
      [ "response" A..= ("info" :: String)
      , "creation" A..= contractCreationToJSON resCreation
      , "steps" A..= fmap contractStepToJSON resSteps
      ]
  toJSON Body{..} =
    A.object $
      [ "response" A..= ("body" :: String)
      , "txId" A..= C.getTxId resTxBody
      , case resTxEra of
          C.ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "era" A..= C.BabbageEra
      , case resTxEra of
          C.ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "body" A..= textEnvelopeToJSON resTxBody
      ]
        <> case resContractId of
          Nothing -> []
          Just contractId -> ["contractId" A..= contractId]
  toJSON Tx{..} =
    A.object
      [ "response" A..= ("tx" :: String)
      , "txId" A..= resTxId
      , case resTxEra of
          C.ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "era" A..= C.BabbageEra
      , case resTxEra of
          C.ReferenceTxInsScriptsInlineDatumsInBabbageEra -> "tx" A..= textEnvelopeToJSON resTx
      ]
  toJSON TxId{..} =
    A.object
      [ "response" A..= ("txId" :: String)
      , "txId" A..= resTxId
      ]
  toJSON TxInfo{..} =
    A.object
      [ "response" A..= ("txInfo" :: String)
      , "transaction" A..= resTransaction
      ]

newtype FinishOnWait = FinishOnWait {unFinishOnWait :: Bool}
  deriving (Eq, Show)

newtype FinishOnClose = FinishOnClose {unFinishOnClose :: Bool}
  deriving (Eq, Show)

newtype PollingFrequency = PollingFrequency {unPollingFrequency :: Second}
  deriving (Eq, Ord, Show)

contractCreationToJSON :: CreateStep 'V1 -> A.Value
contractCreationToJSON CreateStep{..} =
  A.object
    [ "output" A..= transactionScriptOutputToJSON createOutput
    , "payoutValidatorHash" A..= filter (/= '"') (show payoutValidatorHash)
    ]

contractStepToJSON :: ContractStep 'V1 -> A.Value
contractStepToJSON (ApplyTransaction Transaction{..}) =
  A.object
    [ "step" A..= ("apply" :: String)
    , "txId" A..= transactionId
    , "contractId" A..= renderContractId contractId
    , "redeemer" A..= inputs
    , "scriptOutput" A..= fmap transactionScriptOutputToJSON (scriptOutput output)
    , "payouts" A..= M.map payoutToJSON (payouts output)
    ]
contractStepToJSON (RedeemPayout RedeemStep{..}) =
  A.object
    [ "step" A..= ("payout" :: String)
    , "utxo" A..= utxo
    , "redeemingTx" A..= redeemingTx
    , "datum" A..= datum
    ]

transactionScriptOutputToJSON :: TransactionScriptOutput 'V1 -> A.Value
transactionScriptOutputToJSON TransactionScriptOutput{..} =
  A.object
    [ "address" A..= address
    , "assets" A..= assets
    , "utxo" A..= utxo
    , "datum" A..= datum
    ]

payoutToJSON :: Payout 'V1 -> A.Value
payoutToJSON Payout{..} =
  A.object
    [ "address" A..= address
    , "assets" A..= assets
    , "datum" A..= datum
    ]

textEnvelopeFromJSON :: (C.HasTextEnvelope a) => C.AsType a -> C.TextEnvelope -> A.Parser a
textEnvelopeFromJSON asType envelope =
  do
    case C.deserialiseFromTextEnvelope asType envelope of
      Left msg -> fail $ show msg
      Right body -> pure body

textEnvelopeToJSON :: (C.HasTextEnvelope a) => a -> A.Value
textEnvelopeToJSON x =
  let envelope = C.serialiseToTextEnvelope Nothing x
   in A.toJSON envelope

mkBody :: Maybe ContractId -> TxBodyInEraWithReferenceScripts -> MarloweResponse v
mkBody resContractId (TxBodyInEraWithReferenceScripts resTxEra resTxBody) =
  let resTxId = fromCardanoTxId $ C.getTxId resTxBody
   in Body{..}

addressFromJSON :: T.Text -> A.Parser Address
addressFromJSON = maybe (A.parseFail "Failed decoding Bech32 address.") pure . fromBech32

addressToJSON :: Address -> A.Value
addressToJSON = maybe (error "Failed encoding Bech32 address.") A.String . toBech32

metadataFromJSON :: A.Value -> A.Parser TransactionMetadata
metadataFromJSON = maybe (A.parseFail "Failed decoding transaction metadata.") pure . fromJSONEncodedTransactionMetadata
