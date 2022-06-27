module Session where

import Prologue

import Auth (AuthRole(..), authStatusAuthRole)
import Contrib.HTML.Window.Child as Window.Child
import Data.DateTime (DateTime)
import Data.DateTime (adjust) as D
import Data.Generic.Rep (class Generic)
import Data.Lens (view)
import Data.Time.Duration (Days(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDateTime)
import Marlowe (Api)
import Marlowe as Server
import Network.RemoteData (RemoteData(..))
import Servant.PureScript (class MonadAjax)
import Types (WebData)
import Web.HTML as Web
import Web.HTML.Window (outerHeight, outerWidth)
import Web.HTML.WindowExtra (close)

-- Currently session contains only expiration time
data Session
  = Anonymous
  | GithubAuth DateTime

derive instance Eq Session
derive instance Ord Session
derive instance Generic Session _

type AuthResponse = WebData (Maybe Session)

-- | Relies on plutus-apps/playground-common/src/Auth.hs
expiryDuration :: Days
expiryDuration = Days 14.0

-- | Few points about auth flow:
-- |
-- | * We don't want to redirect this app window to oauth handling
-- | endpoint and perform login redirect dance because we want keep our
-- | app open.
-- |
-- | * We create a separate window where we load the request
-- |
-- | * In the child window you should call `handleAuthResponse` when redirection flow
-- | hits back to our app.
-- |
-- | * So `handleAuthResponse` grabs the auth status from the backend and notifies
-- | (using `Window.postMessage` api) the `login` about the results.
-- | It also closes the "helper login window".
login
  :: forall m
   . MonadAff m
  => MonadEffect m
  => MonadAjax Api m
  => m AuthResponse
login = do
  window <- liftEffect Web.window
  let
    popupHeight = 620

    popupWidth = 600

    url = "/api/oauth/github"

  features <- liftEffect $ do
    top <-
      outerHeight window
        <#> \windowHeight -> windowHeight / 2 - popupHeight / 2
    left <-
      outerWidth window
        <#> \windowWidth -> windowWidth / 2 - popupWidth / 2
    pure $ "width="
      <> show popupWidth
      <> ",height="
      <> show popupHeight
      <> ",top="
      <> show top
      <> ",left="
      <> show left
      <> ",menubar=no,status=no,location=no"

  traceM "OPENING WINDOW?"

  liftAff (Window.Child.openAwaitClosed { url, features, window }) >>= case _ of
    Just _ -> fetchSession
    Nothing -> pure $ NotAsked

-- | This effect should be fired when login redirect hits back in
-- | the popup window after auth redirects. It just closes the window.
onAuthResponse :: Effect Unit
onAuthResponse = do
  window <- Web.window
  close window

-- | Performs a hit to the backend which reads the `httpOnly` session
-- | cookie and reports back the status.
-- | In general you should not use this function because the Session
-- | object contains expiration time so you should just store that
-- | value.
fetchSession :: forall m. MonadEffect m => MonadAjax Api m => m AuthResponse
fetchSession = do
  Server.getApiOauthStatus >>= map (view authStatusAuthRole) >>> case _ of
    (Right GithubUser) -> do
      utcDateTime <- liftEffect $ nowDateTime
      -- In theory `adjust` can fail but this should never happen
      -- as our expiery duratino is a constant.
      pure $ Success $ GithubAuth <$> D.adjust expiryDuration utcDateTime
    (Right _) -> pure $ Success Nothing
    Left err -> pure $ Failure err

isAuthenticated :: AuthResponse -> Effect Boolean
isAuthenticated (Success (Just (GithubAuth expirationTime))) = do
  n <- nowDateTime
  pure $ n < expirationTime
isAuthenticated _ = pure false

possiblyAuthenticated :: AuthResponse -> Boolean
possiblyAuthenticated (Success (Just _)) = true
possiblyAuthenticated _ = false

sessionTimeout :: AuthResponse -> Maybe DateTime
sessionTimeout (Success (Just (GithubAuth t))) = Just t
sessionTimeout _ = Nothing
