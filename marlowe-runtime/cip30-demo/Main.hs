{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  where

import qualified Text.Blaze as Blaze
import Text.Julius (juliusFile)
import Yesod (Yesod)
import qualified Yesod

data CIP30 = CIP30

newtype WalletName = WalletName String
  deriving newtype
    ( Eq
    , Read
    , Show
    , Yesod.PathPiece
    , Blaze.ToMarkup
    )

Yesod.mkYesod "CIP30" [Yesod.parseRoutes|
/            HomeR   GET
/#WalletName WalletR GET
|]

instance Yesod CIP30

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
        <i>Poke a running contract to continue
      <li>
        <h2><a>withdraw
        <i>Withdraw payouts from a running contract
  |]

main :: IO ()
main = do
  putStrLn "Listening on port 8000..."
  Yesod.warp 8000 CIP30
  putStrLn "Done!"
