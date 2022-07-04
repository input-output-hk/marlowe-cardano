module Component.Projects.Index where

import Prologue hiding (div)

import Component.Modal.ViewHelpers (modalHeader)
import Contrib.Data.Array.Builder ((+>), (:>), (<:))
import Contrib.Data.Array.Builder as AB
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (decodeJson, fromString)
import Data.Array (catMaybes, filter)
import Data.Array (sortBy)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.DateTime (DateTime)
import Data.DateTime.ISO (ISO)
import Data.Either (hush)
import Data.Foldable (null)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.Function (on)
import Data.Lens (assign, view)
import Data.Lens (to, (^.))
import Data.List (List, fromFoldable)
import Data.Map as Map
import Data.Map.Internal as Map.Internal
import Data.Maybe (fromMaybe)
import Data.Monoid as Monoid
import Data.Newtype (unwrap)
import Data.Ordering (invert)
import Data.Profunctor.Strong ((***))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Gist (Gist(..), gistFiles)
import Gist (Gist, gistCreatedAt, gistDescription, gistId, gistUpdatedAt)
import Halogen (ClassName(..), ComponentHTML, ComponentSlot)
import Halogen (HalogenM)
import Halogen.Classes
  ( fontSemibold
  , modalContent
  , paddingRight
  , smallPaddingRight
  , textSm
  )
import Halogen.HTML (HTML, a, a_, div, div_, span, text)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Halogen.Hooks (HookM)
import Halogen.Hooks as H
import Halogen.Hooks.Extra.Hooks (usePutState)
import MainFrame.Types (Action(..), ChildSlots)
import Marlowe (Api, getApiGists, getApiGistsByGistId)
import Marlowe.Gists (fileExists, filenames, isPlaygroundGist)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Project (FileName(..), Language(..))
import Project (Workflow(..))
import Project as Project
import Project.Bundle as Bundle
import Safe.Coerce (coerce)
import Servant.PureScript (class MonadAjax, printAjaxError)
import Store.OverlayState (withOverlay)
import Type.Constraints (class MonadAffAjaxStore)
import Types (WebData)

type PossibleProjectGist = { gist :: Gist, projectLanguage :: Language }

toPossibleProjectGist :: Gist -> Maybe PossibleProjectGist
toPossibleProjectGist gist = do
  let
    fileNames =
      Array.fromFoldable
        <<< (coerce :: List String -> List FileName)
        <<< Map.Internal.keys
        $ gist ^. gistFiles
  projectLanguage <- Bundle.detectLanguage fileNames
  pure { gist, projectLanguage }

-- component =
--   H.mkComponent
--     { -- First, we provide our function that describes how to produce the first state
--       initialState
--       -- Then, we provide our function that describes how to produce HTML from the state
--     , render
--       -- Finally, we provide our function that describes how to handle actions that
--       -- occur while the component is running, which updates the state.
--     , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
--     }

component = H.component \{ outputToken } _ -> H.do
  loading /\ putLoading <- usePutState true
  gists /\ putGists <- usePutState NotAsked
  gistLoadingFailure /\ putGistLoadingFailure <- usePutState Nothing

  H.useLifecycleEffect $ do
    allGistsResponse <- withOverlay
      $ lift
      $ RemoteData.fromEither <$> getApiGists
    putGists
      -- $ rmap sortGists
      $ map catMaybes
      $ map (map toPossibleProjectGist)
      $ lmap printAjaxError
      $ allGistsResponse
    pure Nothing

  H.pure $ render
    { raise: H.raise outputToken, putGistLoadingFailure }
    { gists, gistLoadingFailure }

render
  :: forall m p
   . Monad m
  => MonadAffAjaxStore m
  => { putGistLoadingFailure :: _ -> H.HookM m Unit
     , raise :: _ -> H.HookM m Unit
     }
  -> _
  -> HookHTML _ m
render actions state =
  -- | Loader should be internal part of this workflow
  HH.div_
    [ modalHeader "Open Project" (Just $ actions.raise Nothing)
    , HH.div [ classes [ modalContent, ClassName "open-project-modal" ] ]
        [ body actions state ]
    ]

type HookHTML slots m = HTML (ComponentSlot slots m (HookM m Unit))
  (HookM m Unit)

body
  :: forall m
   . Monad m
  => MonadAffAjaxStore m
  => { putGistLoadingFailure :: _ -> H.HookM m Unit
     , raise :: _ -> H.HookM m Unit
     }
  -> _
  -> HookHTML _ m
body actions { gists: (Success []) } = span
  [ class_ (ClassName "empty-result") ]
  [ text "No Marlowe projects found" ]

body actions { gists: (Success gists'), gistLoadingFailure } = projectList
  actions
  { gistLoadingFailure
  , gists: gists'
  }

body _ { gists: (Failure _) } = span [ class_ (ClassName "error") ]
  [ text "Failed to load gists" ]

body _ _ = text "…"

sortGists :: Array PossibleProjectGist -> Array PossibleProjectGist
sortGists = sortBy (f `on` _.gist)
  where
  dt :: String -> DateTime
  dt s = fromMaybe bottom $ map unwrap $ hush
    (decodeJson $ fromString s :: _ _ ISO)

  f (Gist { _gistUpdatedAt: a }) (Gist { _gistUpdatedAt: b }) = invert $ compare
    (dt a)
    (dt b)

projectList
  :: forall m p
   . Monad m
  => MonadAffAjaxStore m
  => { putGistLoadingFailure :: _ -> H.HookM m Unit
     , raise :: _ -> H.HookM m Unit
     }
  -> { gists :: Array PossibleProjectGist
     , gistLoadingFailure :: Maybe PossibleProjectGist
     }
  -> HookHTML _ m
projectList actions state =
  HH.div_ $ AB.unsafeBuild $
    errors
      <: HH.div
        [ classes $ AB.unsafeBuild $
            Monoid.guard projectLoadingFailed (AB.cons $ ClassName "error")
              <: ClassName "project-list"
        ]
        (headers <> items)
  where
  projectLoadingFailed = not <<< null $ state.gistLoadingFailure

  errors = Monoid.guard projectLoadingFailed $ AB.cons $ HH.div_
    $ Array.singleton
    $ span
        []
        [ text "Failed to load given gist as project" ]

  headers =
    [ "Name", "Created", "Last Updated", "Open" ]
      <#> \name ->
        HH.div [ classes [ textSm, fontSemibold ] ] [ text name ]

  -- items :: Array (HTML p _)
  items =
    state.gists
      >>= \r@{ gist } ->
        [ div [ classes [ ClassName "project-name", paddingRight ] ]
            [ gist ^. (gistDescription <<< to text) ]
        , div [ classes [ ClassName "date", paddingRight ] ]
            [ gist ^. (gistCreatedAt <<< to formatDate <<< to text) ]
        , div [ classes [ ClassName "date", paddingRight ] ]
            [ gist ^. (gistUpdatedAt <<< to formatDate <<< to text) ]
        , loadLink r JavascriptWorkflow [ smallPaddingRight, fontSemibold ]
        , loadLink r HaskellWorkflow [ smallPaddingRight, fontSemibold ]
        , loadLink r MarloweWorkflow [ smallPaddingRight, fontSemibold ]
        , loadLink r BlocklyWorkflow [ fontSemibold ]
        ]

  canOpenAs Marlowe BlocklyWorkflow = true
  canOpenAs Marlowe MarloweWorkflow = true
  canOpenAs Haskell HaskellWorkflow = true
  canOpenAs Javascript JavascriptWorkflow = true
  canOpenAs _ _ = false

  printWorkflow BlocklyWorkflow = "Blockly"
  printWorkflow MarloweWorkflow = "Marlowe"
  printWorkflow JavascriptWorkflow = "Javascript"
  printWorkflow HaskellWorkflow = "Haskell"

  -- loadLink :: PossibleProjectGist -> Workflow -> Array ClassName -> HTML p _
  loadLink g workflow classNames = do
    let
      canOpen = g.projectLanguage `canOpenAs` workflow
      props =
        if canOpen then
          [ onClick $ const $ do
              withOverlay $ do
                actions.putGistLoadingFailure Nothing
                gist <- lift $ getApiGistsByGistId (g.gist ^. gistId)

                case hush gist >>= Project.fromGist of
                  Just project -> actions.raise $ Just $ LoadProject project
                  Nothing -> actions.putGistLoadingFailure (Just g)
          ]
        else []
    HH.div
      [ classes $ AB.unsafeBuild
          $ AB.cons (ClassName "language-link")
              <> Monoid.guard (not canOpen) (AB.cons $ ClassName "disabled")
              <> AB.consArray classNames
      ]
      [ a props
          [ text $ printWorkflow workflow ]
      ]

formatDate :: String -> String
formatDate s = case decodeJson $ fromString s :: _ _ ISO of
  Left _ -> "Unknown Date"
  Right iso ->
    format
      ( fromFoldable
          [ DayOfMonth
          , Placeholder "/"
          , MonthTwoDigits
          , Placeholder "/"
          , YearFull
          , Placeholder " "
          , Hours24
          , Placeholder ":"
          , MinutesTwoDigits
          ]
      )
      (unwrap iso)
