module Main where

import Prologue

import AppM (runAppM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainFrame.State (component) as MainFrame
import MainFrame.Types (Query(..)) as MainFrame
import Router as Router
import Routing.Duplex as Routing
import Routing.Hash (matchesWith)

main :: Effect Unit
main = do
  tzOffset <- getTimezoneOffset
  HA.runHalogenAff do
    body <- HA.awaitBody
    let mainFrame = H.hoist runAppM MainFrame.component
    driver <- runUI mainFrame tzOffset body
    void
      $ liftEffect
      $ matchesWith (Routing.parse Router.route) \old new -> do
          when (old /= Just new) $ launchAff_ $ driver.query
            (MainFrame.ChangeRoute new unit)
