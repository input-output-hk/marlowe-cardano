{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  where

import Text.Julius (juliusFile)
import Yesod (Yesod)
import qualified Yesod

data CIP30 = CIP30

Yesod.mkYesod "CIP30" [Yesod.parseRoutes|
/ HomeR GET
|]

instance Yesod CIP30

getHomeR :: Handler Yesod.Html
getHomeR = Yesod.defaultLayout do
  Yesod.toWidget $(juliusFile "./cip30-demo/main.js")

main :: IO ()
main = do
  putStrLn "Listening on port 8000..."
  Yesod.warp 8000 CIP30
  putStrLn "Done!"
