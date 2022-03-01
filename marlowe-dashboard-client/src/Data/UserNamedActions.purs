module Data.UserNamedActions
  ( UserNamedActions
  , _Actions
  , empty
  , getParticipantsWithAction
  , haveActions
  , mapActions
  , userNamedActions
  ) where

import Prologue

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.ContractUserParties
  ( ContractUserParties
  , getParticipants
  , getUserParties
  )
import Data.Lens (Lens', Traversal', _2, iso, traversed, view)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Marlowe.Execution.State (getActionParticipant)
import Marlowe.Execution.Types (NamedAction)
import Marlowe.Semantics (Party)

-- These are the Execution.NamedActions viewed as an User (grouped by participants
-- and sorted by "logged-in" user)
newtype UserNamedActions =
  UserNamedActions (Array (Tuple Party (Array NamedAction)))

derive instance Eq UserNamedActions

_UserNamedActions :: Lens' UserNamedActions
  (Array (Tuple Party (Array NamedAction)))
_UserNamedActions = iso
  (\(UserNamedActions m) -> m)
  (\m -> UserNamedActions m)

userNamedActions
  :: ContractUserParties
  -> Array NamedAction
  -> UserNamedActions
userNamedActions contractUserParties actions =
  -- First we expand the actions that can be taken by every party
  expandedActions
    # Array.sortBy currentPartiesFirst
    # Array.groupBy sameParty
    # map extractGroupedParty
    # UserNamedActions
  where
  userParties = getUserParties contractUserParties
  allParticipants = getParticipants contractUserParties

  -- If an action has a participant, just use that, if it doesn't expand it to all
  -- participants
  expandedActions :: Array (Tuple Party NamedAction)
  expandedActions =
    actions
      # Array.foldMap \action -> case getActionParticipant action of
          Just participant -> [ participant /\ action ]
          Nothing -> Set.toUnfoldable allParticipants <#> \participant ->
            participant /\ action

  isUserParty party = Set.member party userParties

  currentPartiesFirst (Tuple party1 _) (Tuple party2 _)
    | isUserParty party1 == isUserParty party2 = compare party1 party2
    | otherwise = if isUserParty party1 then LT else GT

  sameParty a b = fst a == fst b

  extractGroupedParty
    :: NonEmptyArray (Tuple Party NamedAction)
    -> Tuple Party (Array NamedAction)
  extractGroupedParty group = case NonEmptyArray.unzip group of
    tokens /\ actions' -> NonEmptyArray.head tokens /\ NonEmptyArray.toArray
      actions'

empty :: UserNamedActions
empty = UserNamedActions mempty

getParticipantsWithAction :: UserNamedActions -> Set Party
getParticipantsWithAction = Set.fromFoldable <<< map fst <<< view
  _UserNamedActions

haveActions :: UserNamedActions -> Boolean
haveActions = (\l -> l > 0) <<< Array.length <<< view _UserNamedActions

mapActions
  :: forall b. (Party -> Array NamedAction -> b) -> UserNamedActions -> Array b
mapActions f (UserNamedActions actions) = Tuple.uncurry f <$> actions

_Actions :: Traversal' UserNamedActions NamedAction
_Actions = _UserNamedActions <<< traversed <<< _2 <<< traversed
