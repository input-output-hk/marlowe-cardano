{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (
  main,
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value, eitherDecode, object, withObject, (.:), (.=))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.String (fromString)
import Network.HTTP.Types (hContentType)
import Network.Wai (Application, requestHeaders, strictRequestBody)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import OpenSSL.EVP.Base64 (decodeBase64BS)
import OpenSSL.EVP.Digest (Digest, getDigestByName)
import OpenSSL.EVP.PKey (SomePublicKey)
import OpenSSL.EVP.Verify (VerifyStatus (VerifySuccess), verifyBS)
import OpenSSL.PEM (readPublicKey)
import Servant (
  Context (..),
  ErrorFormatters,
  Handler,
  HasContextEntry,
  HasServer (..),
  JSON,
  Post,
  Proxy (..),
  Server,
  ServerError (..),
  ServerT,
  err400,
  err403,
  err415,
  getContextEntry,
  serveWithContext,
  type (:>),
 )
import Servant.API (ReqBody')
import Servant.Server.Internal.Delayed (addBodyCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail, delayedFailFatal, withRequest)
import Servant.Server.Internal.ErrorFormatter (MkContextWithErrorFormatter)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8

-- | A request for a ProofSpace credential.
data ProofRequest = ProofRequest
  { reqProtocolVersion :: Integer
  , reqPublicServiceDid :: String
  , reqSubscriberConnectDid :: String
  , reqActionId :: String
  , reqActionInstanceId :: String
  , reqActionEventId :: String
  , reqActionParams :: Value
  , reqReceivedCredentials :: [ReceivedCredential]
  , reqIssuedProjects :: Value
  , reqIssuedCredentials :: Value
  }
  deriving (Show)

instance FromJSON ProofRequest where
  parseJSON =
    withObject "ProofRequest" $
      \o ->
        do
          reqProtocolVersion <- o .: "protocolVersion"
          reqPublicServiceDid <- o .: "publicServiceDid"
          reqSubscriberConnectDid <- o .: "subscriberConnectDid"
          reqActionId <- o .: "actionId"
          reqActionInstanceId <- o .: "actionInstanceId"
          reqActionEventId <- o .: "actionEventId"
          reqActionParams <- o .: "actionParams"
          reqReceivedCredentials <- o .: "receivedCredentials"
          reqIssuedProjects <- o .: "issuedProjects"
          reqIssuedCredentials <- o .: "issuedCredentials"
          pure ProofRequest{..}

-- | A received ProofSpace credential.
data ReceivedCredential = ReceivedCredential
  { schemaId :: String
  , credentialId :: String
  , credWalletId :: String
  , fields :: Value
  , utcIssuedAt :: Integer
  }
  deriving (Show)

instance FromJSON ReceivedCredential where
  parseJSON =
    withObject "ReceivedCredential" $
      \o ->
        do
          schemaId <- o .: "schemaId"
          credentialId <- o .: "credentialId"
          credWalletId <- o .: "credWalletId"
          fields <- o .: "fields"
          utcIssuedAt <- o .: "utcIssuedAt"
          pure ReceivedCredential{..}

-- | The response for a ProofSpace webhook.
data ProofResponse = ProofResponse
  { resServiceDid :: String
  , resSubscriberConnectDid :: String
  , resActionEventId :: String
  , resIssuedCredentials :: [Value]
  , resRevokedCredentials :: [Value]
  , resOk :: Bool
  }
  deriving (Show)

instance ToJSON ProofResponse where
  toJSON ProofResponse{..} =
    object
      [ "serviceDid" .= resServiceDid
      , "subscriberConnectDid" .= resSubscriberConnectDid
      , "actionEventId" .= resActionEventId
      , "issuedCredentials" .= resIssuedCredentials
      , "revokedCredentials" .= resRevokedCredentials
      , "ok" .= resOk
      ]

-- | Type for processing credentials.
type ProofProcessor = ProofRequest -> IO Bool

-- | Handle a request with a verification of credentials.
proofHandler
  :: ProofProcessor
  -> ProofRequest
  -> Handler ProofResponse
proofHandler process req@ProofRequest{..} =
  do
    let resServiceDid = reqPublicServiceDid
        resSubscriberConnectDid = reqSubscriberConnectDid
        resActionEventId = reqActionEventId
        resIssuedCredentials = mempty
        resRevokedCredentials = mempty
    resOk <- liftIO $ process req
    pure ProofResponse{..}

-- | Type for the API.
type ProofApi = "sync-issue" :> ProofRequestBody :> Post '[JSON] ProofResponse

-- | The API.
proofApi :: Proxy ProofApi
proofApi = Proxy

-- | Verify the signature for the request body.
verifyBody
  :: Digest
  -- ^ The digest algorithm.
  -> SomePublicKey
  -- ^ The public key.
  -> BS.ByteString
  -- ^ The signature, in base-64 encoding.
  -> BS.ByteString
  -- ^ The request body.
  -> Bool
  -- ^ Whether the signature is valid.
verifyBody digest pubKey signature body =
  let signature' = decodeBase64BS signature
      result = unsafePerformIO $ verifyBS digest signature' pubKey body
   in result == VerifySuccess

-- | Custom API combinator for verifying signatures.
data ProofRequestBody

instance
  ( HasServer api ctx
  , HasContextEntry (MkContextWithErrorFormatter ctx) ErrorFormatters
  , HasContextEntry ctx Digest
  , HasContextEntry ctx SomePublicKey
  )
  => HasServer (ProofRequestBody :> api :: Type) ctx
  where
  -- The type of the server is the same as a normal request body
  type
    ServerT (ProofRequestBody :> api) m =
      ServerT (ReqBody' '[] '[JSON] ProofRequest :> api) m

  -- Delegate hoisting to the instance for ReqBody'
  hoistServerWithContext _ = hoistServerWithContext $ Proxy @(ReqBody' '[] '[JSON] ProofRequest :> api)

  -- This method decides if the request should be routed to our server, and if so returns the server for the request.
  route _ ctx subServer =
    -- Unfortunately, we can't just extend the instance for ReqBody', because the Delayed constructor
    -- uses existential quantification. This means a lot of the internals are inaccessible, and we need to
    -- duplicate the logic from the base instance instead (open/closed principle failed!) Luckily, our use-case is narrow so we can
    -- simplify a few things.
    route (Proxy @api) ctx $ addBodyCheck subServer contentCheck bodyCheck
    where
      digest = getContextEntry ctx
      pubKey = getContextEntry ctx
      -- Our content check extracts the signature from the headers.
      -- We also look at the content-type header and return a body decoder
      -- which will be passed to the body checker.
      contentCheck :: DelayedIO (LBS8.ByteString -> Either ServerError ProofRequest)
      contentCheck = withRequest \req -> do
        -- We only accept JSON, so this is hard-coded.
        unless (lookup hContentType (requestHeaders req) == Just "application/json") do
          delayedFail err415
        -- Lookup the signature header
        case lookup "x-body-signature" $ requestHeaders req of
          -- Fail request if not found
          Nothing -> delayedFail $ err403{errBody = "X-Body-Signature header is absent"}
          -- If found, return the body decoding function
          Just signature -> pure \bodyBytes -> do
            -- Verify the body with the signature
            unless (verifyBody digest pubKey signature (LBS8.toStrict bodyBytes)) $ Left $ err403{errBody = "Invalid signature"}
            -- Decode the body
            first (\msg -> err400{errBody = LBS8.pack msg}) $ eitherDecode bodyBytes

      -- Our body check reads and verifies the body. It receives the decoding function returned by the content check.
      bodyCheck :: (LBS8.ByteString -> Either ServerError ProofRequest) -> DelayedIO ProofRequest
      bodyCheck decodeBody = withRequest \req -> do
        -- Read the body - no going back now, we have modified the state of the request!
        body <- liftIO $ strictRequestBody req
        -- Decode it
        case decodeBody body of
          -- Fail fatally here because we have fully committed to handling the request by reading the body.
          -- Alternative handlers would now see no body, so we can't allow them to be used as fallbacks.
          Left e -> delayedFailFatal e
          Right proofRequest -> pure proofRequest

-- | Context containing the digest algorithm and public key for verifying signatures.
type ProofServerContext = Context (Digest ': SomePublicKey ': '[])

-- | Construct the context for the server.
proofServerContext
  :: String
  -- ^ The name of the digest, conforms to OpenSSL naming convention.
  -> FilePath
  -- ^ The path to the public-key PEM file for verifying signatures.
  -> IO ProofServerContext
  -- ^ Action for constructing the context.
proofServerContext digestName pubKeyFile =
  do
    Just digest <- getDigestByName digestName
    pubKey <- readPublicKey =<< readFile pubKeyFile
    pure $ digest :. pubKey :. EmptyContext

-- | The server.
proofServer
  :: ProofProcessor
  -> Server ProofApi
proofServer = proofHandler

-- | The application.
application
  :: ProofServerContext
  -> ProofProcessor
  -> Application
application context = serveWithContext proofApi context . proofServer

-- | Main entry point.
main :: IO ()
main =
  do
    [pubKeyFile, host, port] <- getArgs
    context <- proofServerContext "sha3-256" pubKeyFile
    let settings = setHost (fromString host) $ setPort (read port) defaultSettings
        processor = const $ pure True -- FIXME: To be implemented.
    runSettings settings $ application context processor
