module Page.Welcome.Forms.Render where

import Prologue

import Component.Button.Types as Button
import Component.Button.View (button)
import Component.Progress.Circular as Progress
import Data.Foldable (foldMap)
import Halogen.Css (classNames)
import Halogen.HTML (HTML)
import Halogen.HTML as HH

type RenderOpts widget action =
  { body :: Array (HTML widget action)
  , inProgress :: Boolean
  , onCancel ::
      { action :: Maybe action
      , label :: String
      }
  , onSkip ::
      Maybe
        { action :: Maybe action
        , label :: String
        }
  , onSubmit ::
      { action :: Maybe action
      , label :: String
      }
  , title :: String
  }

render :: forall action widget. RenderOpts widget action -> HTML widget action
render { body, inProgress, onCancel, onSkip, onSubmit, title } =
  HH.div
    [ classNames [ "p-5", "lg:p-6", "space-y-2" ] ]
    $
      [ HH.h2
          [ classNames [ "font-bold" ] ]
          [ HH.text title ]
      ]
        <> body
        <>
          [ HH.div
              [ classNames [ "flex", "justify-center", "gap-4" ] ]
              if inProgress then
                [ Progress.view Progress.defaultSpec
                    { color = "text-purple"
                    , width = "w-14"
                    , height = "h-14"
                    }
                ]
              else
                [ button
                    Button.Secondary
                    onCancel.action
                    [ "flex-1" ]
                    [ HH.text onCancel.label ]
                , button
                    Button.Primary
                    onSubmit.action
                    [ "flex-1" ]
                    [ HH.text onSubmit.label ]
                ]
                  <> flip foldMap onSkip \{ action, label } -> pure $ button
                    Button.Secondary
                    action
                    [ "flex-1" ]
                    [ HH.text label ]

          ]
