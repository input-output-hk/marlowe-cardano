{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main (
  main,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.String (fromString)
import Language.Marlowe.Runtime.App.Sign (sign)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant (Handler, JSON, Post, Proxy (..), ReqBody, Server, serve, type (:>))
import System.Environment (getArgs)

import qualified Cardano.Api as C (
  AsType (AsBabbageEra, AsPaymentExtendedKey, AsPaymentKey, AsSigningKey, AsTxBody),
  BabbageEra,
  HasTextEnvelope,
  PaymentExtendedKey,
  PaymentKey,
  SigningKey,
  TextEnvelope,
  Tx,
  TxBody,
  deserialiseFromTextEnvelope,
  serialiseToTextEnvelope,
 )

data SignRequest = Sign
  { reqTxBody :: C.TxBody C.BabbageEra
  , reqPaymentKeys :: [C.SigningKey C.PaymentKey]
  , reqPaymentExtendedKeys :: [C.SigningKey C.PaymentExtendedKey]
  }
  deriving (Show)

instance FromJSON SignRequest where
  parseJSON =
    withObject "SignRequest" $
      \o ->
        do
          reqTxBody <- textEnvelopeFromJSON (C.AsTxBody C.AsBabbageEra) =<< o .: "body"
          reqPaymentKeys <- mapM (textEnvelopeFromJSON $ C.AsSigningKey C.AsPaymentKey) =<< o .: "paymentKeys"
          reqPaymentExtendedKeys <-
            mapM (textEnvelopeFromJSON $ C.AsSigningKey C.AsPaymentExtendedKey) =<< o .: "paymentExtendedKeys"
          pure Sign{..}

data SignResponse = Tx
  { resTxId :: TxId
  , resTx :: C.Tx C.BabbageEra
  }
  deriving (Show)

instance ToJSON SignResponse where
  toJSON Tx{..} =
    object
      [ "txId" .= resTxId
      , "tx" .= textEnvelopeToJSON resTx
      ]

textEnvelopeFromJSON :: (C.HasTextEnvelope a) => C.AsType a -> C.TextEnvelope -> Parser a
textEnvelopeFromJSON asType envelope =
  do
    case C.deserialiseFromTextEnvelope asType envelope of
      Left msg -> fail $ show msg
      Right body -> pure body

textEnvelopeToJSON :: (C.HasTextEnvelope a) => a -> Value
textEnvelopeToJSON x =
  let envelope = C.serialiseToTextEnvelope Nothing x
   in toJSON envelope

signingHandler
  :: SignRequest
  -> Handler SignResponse
signingHandler Sign{..} =
  let (resTxId, resTx) = sign reqTxBody reqPaymentKeys reqPaymentExtendedKeys
   in pure Tx{..}

type SigningApi = "sign" :> ReqBody '[JSON] SignRequest :> Post '[JSON] SignResponse

signingApi :: Proxy SigningApi
signingApi = Proxy

signingServer
  :: Server SigningApi
signingServer = signingHandler

application
  :: Application
application = serve signingApi signingServer

main :: IO ()
main =
  do
    [host, port] <- getArgs
    let settings =
          setHost (fromString host) $
            setPort (read port) defaultSettings
    runSettings settings application
