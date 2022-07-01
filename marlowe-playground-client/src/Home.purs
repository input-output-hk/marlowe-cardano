module Home where

import Prologue hiding (div)

import Component.NewProject.Types as NewProject
import Halogen (ComponentHTML)
import Halogen.Classes
  ( arrowLeftDown
  , arrowLeftUp
  , arrowRightDown
  , arrowRightUp
  , marloweLogo
  , newProjectBlocklyIcon
  , newProjectHaskellIcon
  , newProjectJavascriptIcon
  , primaryButton
  , secondaryButton
  , simulationIconBlack
  )
import Halogen.Css (classNames)
import Halogen.HTML (a, button, div, h1, img, span, span_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href, src, target)
import MainFrame.Types (Action(..), ChildSlots, ModalView(..), State)
import Project (Workflow(..))

render :: forall m. State -> ComponentHTML Action ChildSlots m
render _ =
  div [ classNames [ "flex", "flex-col", "items-center", "my-16" ] ]
    [ h1 [ classNames [ "font-semibold", "text-4xl", "mb-16" ] ]
        [ text "Get started" ]
    , div [ classNames [ "mb-6" ] ]
        [ button
            [ classNames
                ( secondaryButton <>
                    [ "mr-small", "w-56", "text-base", "cursor-pointer" ]
                )
            , onClick $ const $ OpenModal OpenProject
            ]
            [ text "Open existing project" ]
        , button
            [ classNames
                ( primaryButton <>
                    [ "ml-small", "w-56", "text-base", "cursor-pointer" ]
                )
            , onClick $ const $ OpenModal OpenDemo
            ]
            [ text "Open an example" ]
        ]
    , span [ classNames [ "text-base", "mb-4" ] ] [ text "Or" ]
    , div [ classNames [ "mb-16", "text-base" ] ]
        [ span [ classNames [ "font-bold" ] ] [ text "Choose" ]
        , span_ [ text " a starting environment that's right for you." ]
        ]
    , div
        [ classNames [ "flex", "mb-8" ] ]
        [ a
            [ classNames (newProjectClasses <> [ "mr-24" ])
            , href "#/javascript"
            , onClick
                ( const $ NewProjectAction $ NewProject.CreateProject
                    JavascriptWorkflow
                )
            ]
            [ img
                [ src newProjectJavascriptIcon, classNames [ "h-16", "mb-4" ] ]
            , text
                "Start in Javascript"
            ]
        , a
            [ classNames (newProjectClasses <> [ "mr-24" ])
            , href "#/haskell"
            , onClick
                ( const $ NewProjectAction $ NewProject.CreateProject
                    HaskellWorkflow
                )
            ]
            [ img [ src newProjectHaskellIcon, classNames [ "h-16", "mb-4" ] ]
            , text
                "Start in Haskell"
            ]
        , div
            [ classNames
                [ "border-0"
                , "border-l"
                , "border-black"
                , "border-solid"
                , "h-10"
                , "mt-2"
                ]
            ]
            []
        , a
            [ classNames (newProjectClasses <> [ "ml-24", "mr-4" ])
            , href "#"
            , onClick
                ( const $ NewProjectAction $ NewProject.CreateProject
                    MarloweWorkflow
                )
            ]
            [ img [ src marloweLogo, classNames [ "h-16", "mb-4" ] ]
            , text
                "Start in Marlowe"
            ]
        , div [ classNames [ "flex", "flex-col" ] ]
            [ img [ classNames [ "mt-4" ], src arrowRightUp ]
            , img [ classNames [ "mt-3" ], src arrowLeftUp ]
            ]
        , a
            [ classNames (newProjectClasses <> [ "ml-4", "mr-1" ])
            , href "#"
            , onClick
                ( const $ NewProjectAction $ NewProject.CreateProject
                    BlocklyWorkflow
                )
            ]
            [ img [ src newProjectBlocklyIcon, classNames [ "h-16", "mb-4" ] ]
            , text
                "Start in Blockly"
            ]
        ]
    , div [ classNames [ "mb-10", "flex", "items-end" ] ]
        [ img [ classNames [ "mr-24" ], src arrowRightDown ]
        , div [ classNames [ "flex", "flex-col", "text-sm" ] ]
            [ img [ src simulationIconBlack, classNames [ "mb-4" ] ]
            , text "Simulate"
            ]
        , img [ classNames [ "ml-24" ], src arrowLeftDown ]
        ]
    , div [ classNames [ "font-bold", "text-sm" ] ]
        [ a [ href "./doc/marlowe/tutorials/index.html", target "_blank" ]
            [ text "Read our Getting Started guide" ]
        ]
    ]
  where
  newProjectClasses =
    [ "flex"
    , "flex-col"
    , "font-bold"
    , "text-sm"
    , "cursor-pointer"
    , "text-center"
    ]
