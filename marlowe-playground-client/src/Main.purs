module Main where

import Prologue

import AppM (runAppM)
import Control.Monad.Error.Class (liftEither)
import Data.Argonaut
  ( Json
  , JsonDecodeError
  , decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (getTimezoneOffset)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainFrame.State (component) as MainFrame
import MainFrame.Types (Query(..)) as MainFrame
import Router as Router
import Routing.Duplex as Routing
import Routing.Hash (matchesWith)
import Types (WebpackBuildMode(..))

decodeMainArgs :: Json -> Either JsonDecodeError WebpackBuildMode
decodeMainArgs = decodeJson >=> \obj -> obj .: "webpackBuildMode" <#>
  (eq "development" >>> if _ then Development else Production)

main :: Json -> Effect Unit
main args = do
  let
    badArgsError e = error $ "Failed to start: bad startup args.\n\n" <>
      printJsonDecodeError e
  webpackBuildMode <- liftEither $ lmap badArgsError $ decodeMainArgs args
  tzOffset <- getTimezoneOffset
  HA.runHalogenAff do
    body <- HA.awaitBody
    let mainFrame = H.hoist (runAppM { webpackBuildMode }) MainFrame.component
    driver <- runUI mainFrame { tzOffset, webpackBuildMode } body
    void
      $ liftEffect
      $ matchesWith (Routing.parse Router.route) \old new -> do
          when (old /= Just new) $ launchAff_ $ driver.query
            (MainFrame.ChangeRoute new unit)
