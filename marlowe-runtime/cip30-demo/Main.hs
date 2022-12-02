{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  where

import Cardano.Api (BabbageEra)
import Control.Exception (throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction.Api
import Network.Protocol.Driver (RunClient, runClientPeerOverSocket)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import qualified Network.Protocol.Job.Client as JobClient
import Network.Protocol.Job.Codec (codecJob)
import Network.Socket (SocketType(..))
import qualified Network.Socket as Socket
import System.Environment (getArgs)
import qualified Text.Blaze as Blaze
import Text.Julius (juliusFile)
import Yesod (Yesod)
import qualified Yesod
import qualified Yesod.Core.Types

data CIP30 = CIP30 Socket.HostName Socket.PortNumber

newtype WalletName = WalletName String
  deriving newtype
    ( Eq
    , Read
    , Show
    , Yesod.PathPiece
    , Blaze.ToMarkup
    )

data PostWalletCreateRequestDTO = PostWalletCreateRequestDTO
  { version :: ()
  , source :: V1.Contract
  -- , walletAddresses :: Transaction.Api.WalletAddresses
  }
  deriving (Show, Generic, Aeson.FromJSON)

data PostWalletCreateResponseDTO = PostWalletCreateResponseDTO
  { unsignedTransaction :: Text
  , contractId :: Text
  }

Yesod.mkYesod "CIP30" [Yesod.parseRoutes|
/                   HomeR         GET
/#WalletName        WalletR       GET
/#WalletName/create WalletCreateR GET POST
|]

instance Yesod CIP30 where
  makeSessionBackend _ = pure Nothing

getHomeR :: Handler Yesod.Html
getHomeR = Yesod.defaultLayout do
  Yesod.toWidget $(juliusFile "./cip30-demo/main.js")

getWalletR :: WalletName -> Handler Yesod.Html
getWalletR walletName = Yesod.defaultLayout do
  Yesod.toWidget [Yesod.hamlet|
    <h1>#{walletName} wallet options
    <ul>
      <li>
        <h2><a>create
        <i>Create a new contract
      <li>
        <h2><a>deposit
        <i>Deposit funds into a running contract
      <li>
        <h2><a>choose
        <i>Make a choice in a running contract
      <li>
        <h2><a>notify
        <i>Prompt a running contract to recheck a condition it is currently awaiting
      <li>
        <h2><a>withdraw
        <i>Withdraw payouts from a running contract
  |]

getWalletCreateR :: WalletName -> Handler Yesod.Html
getWalletCreateR walletName = Yesod.defaultLayout do
  Yesod.toWidget [Yesod.hamlet|
    <input id="contractSource" type="text">
    <button id="postContractSource">click
    <script>
      const contractSource = document.getElementById('contractSource')
      const postContractSource = document.getElementById('postContractSource')
      postContractSource.addEventListener("click", async () => {
        const body = {
          version: "v1"
          source: JSON.parse(contractSource.value)
        }
        const options = {
          method: "POST",
          headers: { "Content-Type": "application/json;charset=utf-8" },
          body: JSON.stringify(body)
        }
        console.log(options)
        const response = await fetch("/#{walletName}/create", options)
        const data = await response.json()
        console.log(data)
      })
  |]
  pure ()

{- todo
DONE steal from
  DONE TxClientDependencies
    type RunClient m client = forall a. client m a -> m a
  DONE optionsToServerDependencies
  DONE txClient
DOING put it all together
  DONE JSON -> V1.Contract
    contractFromJSON :: MarloweVersion v -> Value -> Parser (Contract v)
    parseEither :: (a -> Parser b) -> a -> Either String b
    withSomeMarloweVersion :: (forall v. MarloweVersion v -> r) -> SomeMarloweVersion -> r
  DOING get all the other parameters required for creating a contract
    DONE get hostName and portName as command line arguments
    DONE hard code marlowe v1
    DONE require json to be V1.Contract
    DOING get wallet addresses from wallet in client
  TODO return the contract creation result to the client and handle it
TODO clean up code
TODO manual testing
  TODO host name and port number for tx commands
-}

runTxJobClient :: Socket.HostName -> Socket.PortNumber -> RunClient IO (JobClient Transaction.Api.MarloweTxCommand)
runTxJobClient hostName (show -> portNumber) jobclient = do
  (head -> txCommandAddr :: Socket.AddrInfo) <- Socket.getAddrInfo
    (Just Socket.defaultHints { Socket.addrSocketType = Stream })
    (Just hostName)
    (Just portNumber)
  runClientPeerOverSocket throwIO txCommandAddr codecJob jobClientPeer jobclient

createContract ::
     Socket.HostName
  -> Socket.PortNumber
  -> Transaction.Api.WalletAddresses
  -> V1.Contract
  -> IO (Either (Transaction.Api.CreateError 'Core.Api.V1) (Transaction.Api.ContractCreated BabbageEra 'Core.Api.V1))
createContract hostName portNumber addresses =
  runTxJobClient hostName portNumber
    . JobClient.liftCommand
    . Transaction.Api.Create
        Nothing
        Core.Api.MarloweV1
        addresses
        Transaction.Api.RoleTokensNone
        mempty
        2_000_000

postWalletCreateR :: WalletName -> Handler (Yesod.Core.Types.JSONResponse Text)
postWalletCreateR _ = do
  CIP30 hostName portNumber <- Yesod.getYesod
  postWalletCreateRequestDTO :: PostWalletCreateRequestDTO <- Yesod.requireCheckJsonBody
  Yesod.liftIO do
    _ <- createContract
          hostName
          portNumber
          undefined
          (source postWalletCreateRequestDTO)
    putStrLn "hey"
  pure $ Yesod.Core.Types.JSONResponse "lol"

main :: IO ()
main = do
  [hostName :: Socket.HostName, read -> portNumber :: Socket.PortNumber] <- getArgs
  putStrLn "Listening on port 8000..."
  Yesod.warp 8000 (CIP30 hostName portNumber)
