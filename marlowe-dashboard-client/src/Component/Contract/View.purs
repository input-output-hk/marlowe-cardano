module Component.Contract.View
  ( firstLetterInCircle
  , participantWithNickname
  , renderParty
  , startingStepActions
  , timeoutString
  ) where

import Prologue hiding (div)

import Component.Popper (Placement(..))
import Component.Tooltip.State (tooltip)
import Component.Tooltip.Types (ReferenceId(..))
import Data.Address (Address)
import Data.Compactable (compact)
import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.Lens ((^.), (^?))
import Data.Maybe (maybe')
import Data.String (take)
import Data.String.Extra (capitalize)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML (HTML, div, text)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (id)
import Humanize (humanizeDuration)
import Marlowe.Execution.Lenses (_mNextTimeout)
import Marlowe.Execution.State (isClosed)
import Marlowe.Execution.Types as Execution
import Marlowe.Run.Contract.V1.Types (_utxoAddress)
import Marlowe.Semantics (CurrencySymbol, Party(..), Token(..))
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Store.RoleTokens (RoleTokenStore, getDisplayName, getRoleToken)

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
renderParty
  :: forall slots a m
   . MonadAff m
  => (Address -> a)
  -> Int
  -> CurrencySymbol
  -> RoleTokenStore
  -> Party
  -> H.ComponentHTML a
       (tooltipSlot :: forall query. H.Slot query Void ReferenceId | slots)
       m
renderParty onClick stepNumber currencySymbol roleTokens party =
  let
    participantName = participantWithNickname currencySymbol roleTokens party
    mToken = case party of
      Role tokenName -> Just $ Token currencySymbol tokenName
      _ -> Nothing
    mRoleToken = flip getRoleToken roleTokens =<< fromMaybe mToken
    mAddress = mRoleToken ^? _Success <<< _utxoAddress
    mOnClick = onClick <$> mAddress
    itemId = "party-" <> participantName <> "-step-" <> show stepNumber
    mTooltipMsg = case mRoleToken of
      NotAsked -> Nothing
      Loading -> Just "Loading role details..."
      Failure _ -> Just "Failed to load role details."
      Success _ -> Just "Click to copy address."
  in
    div
      ( compact
          [ pure $ classNames $ compact
              [ pure "text-xs"
              , pure "flex"
              , pure "gap-1"
              , pure "whitespace-nowrap"
              , "cursor-pointer" <$ mOnClick
              ]
          , pure $ id itemId
          , onClick_ <$> mOnClick
          ]
      )
      ( compact
          [ pure $ firstLetterInCircle
              { styles:
                  [ "bg-gradient-to-r"
                  , "from-purple"
                  , "to-lightpurple"
                  , "text-white"
                  ]
              , name: participantName
              }
          , pure $ div
              [ classNames
                  [ "font-semibold"
                  , "overflow-ellipsis"
                  , "overflow-hidden"
                  , "w-4/5"
                  ]
              ]
              [ text participantName ]
          , tooltip <$> mTooltipMsg <@> (RefId itemId) <@> Bottom
          ]
      )

participantWithNickname
  :: CurrencySymbol -> RoleTokenStore -> Party -> String
participantWithNickname currencySymbol roleTokens party =
  capitalize case party of
    PK publicKey -> publicKey
    Role tokenName ->
      let
        suffix = foldMap (\n -> " (" <> n <> ")")
          $ getDisplayName (Token currencySymbol tokenName) roleTokens
      in
        tokenName <> suffix

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
