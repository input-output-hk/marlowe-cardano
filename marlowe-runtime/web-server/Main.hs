{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}

module Main
  where

import Data.Proxy (Proxy(Proxy))
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync.Api
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api
-- import qualified Cardano.Api.SerialiseTextEnvelope
import qualified Data.ByteString.Lazy as BL
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction.Api
import qualified Network.Wai.Handler.Warp as Warp
import Servant (type (:<|>)((:<|>)), type (:>))
-- import qualified Network.Protocol.Job.Client as Job.Client
import qualified Cardano.Api
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Network.Protocol.Job.Client as Job.Client
import qualified Servant

-- data TxCommand cmd = TxCommand
--   { walletAddresses :: WalletAddresses
--   , signingMethod :: SigningMethod
--   , metadataFile :: Maybe FilePath§
--   , subCommand :: cmd
--   }

-- data CreateCommand = CreateCommand
--   { marloweVersion :: SomeMarloweVersion
--   , roles :: Maybe RolesConfig
--   , contractFiles :: ContractFiles
--   , minUTxO :: Lovelace
--   }

-- data RolesConfig
--   = MintSimple (NonEmpty (TokenName, Address))
--   | MintConfig FilePath
--   | UseExistingPolicyId PolicyId
--   deriving (Show)

-- data ContractFiles
--   = CoreFile FilePath
--   | ExtendedFiles FilePath ContractArgs

-- data ContractArgs
--   = ContractArgsByFile FilePath
--   | ContractArgsByValue ContractArgsValue

-- data ContractArgsValue = ContractArgsValue
--   { timeoutArguments :: Map String POSIXTime
--   , valueArguments :: Map String Integer
--   }

-- data TxOutRef = TxOutRef
--   { txId :: !TxId
--   , txIx :: !TxIx
--   }
--   deriving stock (Show, Eq, Ord, Generic)
--   deriving anyclass (Binary, ToJSON, ToJSONKey)

data CreateV1Request = CreateV1Request
  { minUTxO :: ChainSync.Api.Lovelace
  , walletAddresses :: Transaction.Api.WalletAddresses
  , initialRoleTokenDistribution :: Maybe (Either ChainSync.Api.PolicyId Transaction.Api.Mint)
  , metadata :: ChainSync.Api.TransactionMetadata
  , contract :: V1.Contract
  } deriving (Generic, Aeson.FromJSON)

data CreateV1Response = CreateV1Response
  { contractId :: ChainSync.Api.TxOutRef
  , transactionEnvelope :: BL.ByteString
  }

instance Aeson.ToJSON CreateV1Response where
  toJSON (CreateV1Response contractId transactionEnvelope) = Aeson.Object
    [ ("contractId", Aeson.toJSON contractId)
    , ("transactionEnvelope", Aeson.String $ Text.pack $ show transactionEnvelope)
    ]

-- Cardano.Api.SerialiseTextEnvelope.textEnvelopeToJSON

-- type Whatevs m a = Job.Client.JobClient Transaction.Api.MarloweTxCommand m a

-- tjosan = Job.Client.liftCommand

-- runClientPeerOverSocket
--   :: Exception ex
--   => AddrInfo -- ^ Socket address to connect to
--   -> Codec protocol ex IO ByteString -- ^ A codec for the protocol
--   -> (forall a. client IO a -> Peer protocol 'AsClient st IO a) -- ^ Interpret the client as a protocol peer
--   -> RunClient IO client
-- runClientPeerOverSocket addr codec clientToPeer client = bracket open close \socket -> do
--   let channel = socketAsChannel socket
--   let driver = mkDriver throwIO codec channel
--   let peer = clientToPeer client
--   fst <$> runPeerWithDriver driver peer (startDState driver)
--   where
--     open = bracketOnError (openSocket addr) close \sock -> do
--       connect sock $ addrAddress addr
--       pure sock

-- runClientPeerOverSocket'
--   :: Exception ex
--   => String -- ^ Client failure stderr extra message
--   -> AddrInfo -- ^ Socket address to connect to
--   -> Codec protocol ex IO LB.ByteString -- ^ A codec for the protocol
--   -> (forall a. client IO a -> Peer protocol 'AsClient st IO a) -- ^ Interpret the client as a protocol peer
--   -> RunClient IO client
-- runClientPeerOverSocket' errMsg addr codec clientToPeer client = do
--   let
--     run = runClientPeerOverSocket addr codec clientToPeer client
--   run `catch` \(err :: SomeException)-> do
--     hPutStrLn stderr errMsg
--     throw err

-- runTxJobClient :: JobClient MarloweTxCommand CLI a -> CLI a
-- runTxJobClient client = do
--   Env{..} <- askEnv
--   liftBaseWith \runInBase ->
--     envRunTxJobClient $ hoistJobClient runInBase client

-- runTxCommand :: MarloweTxCommand Void err result -> CLI (Either err result)
-- runTxCommand = runTxJobClient . liftCommand

runTxCommand :: Transaction.Api.MarloweTxCommand Void err result -> IO (Either err result)
runTxCommand = undefined . Job.Client.liftCommand

type StaticFilesApi = Servant.Raw
type RootRedirectApi = Servant.Get '[Servant.PlainText] String
type CreateV1Api = "create" :> "v1" :> Servant.ReqBody '[Servant.JSON] CreateV1Request :> Servant.Post '[Servant.JSON] CreateV1Response
type DepositApi = "deposit" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type ChooseApi = "choose" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type NotifyApi = "notify" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type AdvanceApi = "advance" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type WithdrawalApi = "withdrawal" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()

type Api = RootRedirectApi
      :<|> StaticFilesApi
      :<|> CreateV1Api
      :<|> DepositApi
      :<|> ChooseApi
      :<|> NotifyApi
      :<|> AdvanceApi
      :<|> WithdrawalApi

server :: Servant.Server Api
server = rootRedirectApiH
    :<|> staticFilesApiH
    :<|> createV1ApiH
    :<|> depositApiH
    :<|> chooseApiH
    :<|> notifyApiH
    :<|> advanceApiH
    :<|> withdrawalApiH
  where
    staticFilesApiH :: Servant.Server Servant.Raw
    staticFilesApiH = Servant.serveDirectoryWebApp "web-client"

    rootRedirectApiH :: Servant.Server RootRedirectApi
    rootRedirectApiH = Servant.throwError Servant.err301
      { Servant.errHeaders = [("Location", "index.html")] }

    createV1ApiH :: Servant.Server CreateV1Api
    createV1ApiH req = do
      let c = Transaction.Api.Create
            Nothing
            Core.Api.MarloweV1
            (walletAddresses req)
            (initialRoleTokenDistribution req)
            (metadata req)
            (minUTxO req)
            (contract req)
      _ :: Either
            (Transaction.Api.CreateError 'Core.Api.V1)
            (Core.Api.ContractId, Cardano.Api.TxBody Cardano.Api.BabbageEra) <-
        liftIO $ runTxCommand c
      pure $ CreateV1Response undefined undefined

    depositApiH :: Servant.Server DepositApi
    depositApiH () = pure ()

    chooseApiH :: Servant.Server ChooseApi
    chooseApiH () = pure ()

    notifyApiH :: Servant.Server NotifyApi
    notifyApiH () = pure ()

    advanceApiH :: Servant.Server AdvanceApi
    advanceApiH () = pure ()

    withdrawalApiH :: Servant.Server WithdrawalApi
    withdrawalApiH () = pure ()

main :: IO ()
main = do
  let port = 8000 :: Warp.Port
  putStrLn $ "Listening on port " <> show port <> "..."
  Warp.run port $ Servant.serve (Proxy @Api) server
