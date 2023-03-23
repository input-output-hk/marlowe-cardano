module Main
  where

import Cardano.Api
  ( AsType(..)
  , ShelleyWitnessSigningKey(..)
  , TextEnvelope(..)
  , TextEnvelopeType(..)
  , deserialiseFromTextEnvelope
  , serialiseToTextEnvelope
  , signShelleyTransaction
  )
import Cardano.Api.SerialiseTextEnvelope (TextEnvelopeDescr(..))
import Control.Concurrent (threadDelay)
import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decodeFileStrict)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (Address(..), fromBech32, toBech32)
import Language.Marlowe.Runtime.Web (CreateTxEnvelope(CreateTxEnvelope), PostContractsRequest(..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client
  (getContract, getTransaction, postContract, postTransaction, putContract, putTransaction)
import Test.Integration.Marlowe
import qualified Test.Integration.Marlowe as M (LocalTestnet(..))

main :: IO ()
main = withLocalMarloweRuntime \MarloweRuntime{..} -> do
  putStr "Workspace: "
  putStrLn $ workspaceDir $ M.workspace testnet

  (address, signingKey) <- getFirstWallet testnet
  putStr "Loaded wallet: "
  let webAddress = Web.Address $ fromJust $ toBech32 address
  print webAddress

  either throw pure =<< runWebClient do
    Web.CreateTxEnvelope{txEnvelope = createTxBody, ..} <- postContract webAddress Nothing Nothing Web.PostContractsRequest
      { metadata = mempty
      , tags = mempty
      , version = Web.V1
      , roles = Nothing
      , contract = V1.Close
      , minUTxODeposit = 2_000_000
      }

    liftIO $ print CreateTxEnvelope{txEnvelope = createTxBody, ..}

    createTx <- liftIO $ signShelleyTransaction' createTxBody [signingKey]

    putContract contractId createTx

    contractState <- waitUntilConfirmed (\Web.ContractState{status} -> status) $ getContract contractId

    liftIO $ print contractState

    Web.ApplyInputsTxEnvelope{transactionId, txEnvelope = applyTxBody} <- postTransaction webAddress Nothing Nothing contractId Web.PostTransactionsRequest
      { version = Web.V1
      , tags = mempty
      , metadata = mempty
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , inputs = []
      }

    applyTx <- liftIO $ signShelleyTransaction' applyTxBody [signingKey]

    putTransaction contractId transactionId applyTx

    tx <- waitUntilConfirmed (\Web.Tx{status} -> status) $ getTransaction contractId transactionId

    liftIO $ print tx

waitUntilConfirmed :: MonadIO m => (a -> Web.TxStatus) -> m a -> m a
waitUntilConfirmed getStatus getResource = do
  resource <- getResource
  case getStatus resource of
    Web.Confirmed -> pure resource
    _ -> do
      liftIO $ threadDelay 1000
      waitUntilConfirmed getStatus getResource

signShelleyTransaction' :: Web.TextEnvelope -> [ShelleyWitnessSigningKey] -> IO Web.TextEnvelope
signShelleyTransaction' Web.TextEnvelope{..} wits = do
  let te = TextEnvelope { teType = TextEnvelopeType (T.unpack teType), teDescription = TextEnvelopeDescr (T.unpack teDescription), teRawCBOR = Web.unBase16 teCborHex }
  txBody <- case deserialiseFromTextEnvelope (AsTxBody AsBabbage) te of
    Left err -> fail $ show err
    Right a -> pure a
  pure case serialiseToTextEnvelope Nothing $ signShelleyTransaction txBody wits of
    TextEnvelope (TextEnvelopeType ty) _ bytes -> Web.TextEnvelope (T.pack ty) "" $ Web.Base16 bytes

getFirstWallet :: LocalTestnet -> IO (Address, ShelleyWitnessSigningKey)
getFirstWallet LocalTestnet{..} = do
  let PaymentKeyPair{..} = head wallets
  address <- fromJust . fromBech32 . T.pack <$> execCli
    [ "address", "build"
    , "--verification-key-file", paymentVKey
    , "--testnet-magic", "1"
    ]
  textEnvelope <- fromJust <$> decodeFileStrict paymentSKey
  pure
    ( address
    , WitnessGenesisUTxOKey
        $ fromRight (error "Failed to decode text envelope")
        $ deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) textEnvelope
    )
