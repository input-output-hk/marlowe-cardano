{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main
  where

import Data.Proxy (Proxy(Proxy))
import qualified Network.Wai.Handler.Warp as Warp
import Servant (type (:<|>)((:<|>)), type (:>))
-- import qualified Language.Marlowe.Runtime.API.Command as API.Command
import qualified Servant

type StaticFilesApi = Servant.Raw
type RootRedirectApi = Servant.Get '[Servant.PlainText] String
type CreateApi = "create" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type DepositApi = "deposit" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type ChooseApi = "choose" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type NotifyApi = "notify" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type AdvanceApi = "advance" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()
type WithdrawalApi = "withdrawal" :> Servant.ReqBody '[Servant.JSON] () :> Servant.Post '[Servant.JSON] ()

type Api = RootRedirectApi
      :<|> StaticFilesApi
      :<|> CreateApi
      :<|> DepositApi
      :<|> ChooseApi
      :<|> NotifyApi
      :<|> AdvanceApi
      :<|> WithdrawalApi

server :: Servant.Server Api
server = rootRedirectApiH
    :<|> staticFilesApiH
    :<|> createApiH
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
      { Servant.errHeaders = [("Location", "http://localhost:8000/index.html")] }

    createApiH :: Servant.Server CreateApi
    createApiH () = pure ()

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
