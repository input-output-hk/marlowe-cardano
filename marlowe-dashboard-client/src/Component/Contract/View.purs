module Component.Contract.View
  ( firstLetterInCircle
  , participantWithNickname
  , renderParty
  , startingStepActions
  , timeoutString
  ) where

import Prologue hiding (div)

import Data.ContractUserParties
  ( ContractUserParties
  , getNickname
  , isCurrentUser
  )
import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Function (on)
import Data.Lens ((^.))
import Data.Maybe (maybe')
import Data.String (take)
import Data.String.Extra (capitalize)
import Data.WalletNickname as WN
import Halogen.Css (classNames)
import Halogen.HTML (HTML, div, text)
import Humanize (humanizeDuration)
import Marlowe.Execution.Lenses (_mNextTimeout)
import Marlowe.Execution.State (isClosed)
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics (Party(..))

timeoutString :: Instant -> Execution.State -> String
timeoutString currentTime executionState =
  let
    mNextTimeout = executionState ^. _mNextTimeout
  in
    maybe'
      ( \_ ->
          if isClosed executionState then "Contract closed"
          else "Timed out"
      )
      ( \nextTimeout ->
          humanizeDuration $ on diff toDateTime nextTimeout currentTime
      )
      mNextTimeout

-- TODO: In zeplin all participants have a different color. We need to decide how are we going to assing
--       colors to users. For now they all have purple
renderParty :: forall p a. ContractUserParties -> Party -> HTML p a
renderParty contractUserParties party =
  let
    participantName = participantWithNickname contractUserParties party

  in
    div [ classNames $ [ "text-xs", "flex", "gap-1" ] ]
      [ firstLetterInCircle
          { styles:
              [ "bg-gradient-to-r"
              , "from-purple"
              , "to-lightpurple"
              , "text-white"
              ]
          , name: participantName
          }
      , div [ classNames [ "font-semibold" ] ]
          [ text $
              if isCurrentUser party contractUserParties then participantName <>
                " (you)"
              else participantName
          ]
      ]

participantWithNickname :: ContractUserParties -> Party -> String
participantWithNickname contractUserParties party =
  let
    mNickname = getNickname party contractUserParties
  in
    capitalize case party, mNickname of
      PK publicKey, _ -> publicKey
      Role roleName, Just nickname -> roleName <> " (" <> WN.toString nickname
        <> ")"
      Role roleName, Nothing -> roleName

firstLetterInCircle
  :: forall p a. { styles :: Array String, name :: String } -> HTML p a
firstLetterInCircle { styles, name } =
  div
    [ classNames
        $
          [ "rounded-full"
          , "w-5"
          , "h-5"
          , "text-center"
          , "font-semibold"
          ]
            <> styles
    ]
    [ text $ take 1 name ]

startingStepActions :: forall p a. HTML p a
startingStepActions =
  div [ classNames [ "space-y-6" ] ]
    [ placeholderAccount
    , placeholderContent
    ]
  where
  placeholderAccount =
    div [ classNames [ "flex", "items-center", "gap-1" ] ]
      [ placeholderAvatar
      , placeholderName
      ]

  placeholderAvatar = div
    [ classNames [ "bg-gray", "rounded-full", "w-5", "h-5" ] ]
    []

  placeholderName = div
    [ classNames [ "bg-gray", "rounded-sm", "w-1/2", "h-6" ] ]
    []

  placeholderContent = div
    [ classNames [ "bg-gray", "rounded-full", "w-full", "h-12" ] ]
    []
