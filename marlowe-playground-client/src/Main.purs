module Main where

import Prologue
import AppM (runAppM)
import Data.BigInt.Argonaut as BigInt
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Env (Env(..))
import Halogen as H
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
    BigInt.withJsonPatch do
      body <- HA.awaitBody
      let
        mainFrame = H.hoist (runAppM $ Env { ajaxSettings: { baseURL: "/" } })
          MainFrame.component
      driver <- runUI mainFrame unit body
      void $ liftEffect
        $ matchesWith (Routing.parse Router.route) \old new -> do
            when (old /= Just new) $ launchAff_ $ driver.query
              (MainFrame.ChangeRoute new unit)
