module Main where

import Prologue
import AppM (runAppM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Env (Env)
import Foreign.Generic (defaultOptions)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import MainFrame.State (component) as MainFrame
import MainFrame.Types (Query(..)) as MainFrame
import Marlowe (SPSettings_)
import Router as Router
import Routing.Duplex as Routing
import Routing.Hash (matchesWith)

environment :: Env
environment =
  { ajaxSettings: settings
  }

main ::
  Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      mainFrame :: H.Component HH.HTML MainFrame.Query Unit Void Aff
      mainFrame = H.hoist (runAppM environment) MainFrame.component
    driver <- runUI mainFrame unit body
    void $ liftEffect
      $ matchesWith (Routing.parse Router.route) \old new -> do
          when (old /= Just new) $ launchAff_ $ driver.query (MainFrame.ChangeRoute new unit)

onLoad :: Unit
onLoad = unsafePerformEffect main
