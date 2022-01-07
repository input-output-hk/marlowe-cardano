module Main where

import Prologue
import AppM (runAppM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Env (Env(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainFrame.State (component) as MainFrame
import MainFrame.Types (Query(..)) as MainFrame
import Router as Router
import Routing.Duplex as Routing
import Routing.Hash (matchesWith)

main :: Effect Unit
main =
  HA.runHalogenAff do
    let
      env = Env { ajaxSettings: { baseURL: "/" } }

      store = {}
    body <- HA.awaitBody
    mainFrame <- runAppM env store MainFrame.component
    driver <- runUI mainFrame unit body
    void $ liftEffect
      $ matchesWith (Routing.parse Router.route) \old new -> do
          when (old /= Just new) $ launchAff_
            $ driver.query
                (MainFrame.ChangeRoute new unit)
