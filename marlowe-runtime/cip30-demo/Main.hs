{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  where

import Cardano.Api (BabbageEra)
import Control.Exception (throwIO)
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync.Api
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api
import qualified Language.Marlowe.Runtime.Transaction.Api as Transaction.Api
import Network.Protocol.Driver (RunClient, runClientPeerOverSocket)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import qualified Network.Protocol.Job.Client as JobClient
import Network.Protocol.Job.Codec (codecJob)
import Network.Socket (SocketType(..))
import qualified Network.Socket as Socket
import qualified Text.Blaze as Blaze
import Text.Julius (juliusFile)
import Yesod (Yesod)
import qualified Yesod
import qualified Yesod.Core.Types

data CIP30 = CIP30

newtype WalletName = WalletName String
  deriving newtype
    ( Eq
    , Read
    , Show
    , Yesod.PathPiece
    , Blaze.ToMarkup
    )

newtype PostWalletCreateRequestDTO = PostWalletCreateRequestDTO { source :: Text }
  deriving Show

data PostWalletCreateResponseDTO = PostWalletCreateResponseDTO
  { unsignedTransaction :: Text
  , contractId :: Text
  }

instance Aeson.FromJSON PostWalletCreateRequestDTO where
  parseJSON = Aeson.withObject "PostWalletCreateRequestDTO" \o ->
    PostWalletCreateRequestDTO <$> o .: "source"

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
        const options = {
          method: "POST",
          headers: { "Content-Type": "application/json;charset=utf-8" },
          body: JSON.stringify({ source: contractSource.value })
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
  -> Maybe ChainSync.Api.StakeCredential
  -> Core.Api.MarloweVersion v
  -> Transaction.Api.WalletAddresses
  -> Transaction.Api.RoleTokensConfig
  -> ChainSync.Api.TransactionMetadata
  -> ChainSync.Api.Lovelace
  -> Core.Api.Contract v
  -> IO (Either (Transaction.Api.CreateError v) (Transaction.Api.ContractCreated BabbageEra v))
createContract hostName portNumber stakeCredential version addresses roles metadata minUTxODeposit contract =
  runTxJobClient hostName portNumber
    $ JobClient.liftCommand
    $ Transaction.Api.Create stakeCredential version addresses roles metadata minUTxODeposit contract

postWalletCreateR :: WalletName -> Handler (Yesod.Core.Types.JSONResponse Text)
postWalletCreateR _ = do
  Yesod.liftIO $ putStrLn "hey ho"
  Yesod.liftIO . print @PostWalletCreateRequestDTO =<< Yesod.requireCheckJsonBody
  pure $ Yesod.Core.Types.JSONResponse "lol"

main :: IO ()
main = do
  putStrLn "Listening on port 8000..."
  Yesod.warp 8000 CIP30
  putStrLn "Done!"
