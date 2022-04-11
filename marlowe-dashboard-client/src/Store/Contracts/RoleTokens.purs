module Store.Contracts.RoleTokens
  ( RoleTokenStore
  , getRoleToken
  , isMyRoleToken
  , loadRoleTokenFailed
  , loadRoleTokens
  , mkRoleTokenStore
  , roleTokenLoaded
  , updateMyRoleTokens
  ) where

import Prologue

import Control.Bind (bindFlipped)
import Data.Filterable (filter)
import Data.Lens ((^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Marlowe.Run.Contract.V1.Types (RoleToken, _token)
import Marlowe.Semantics (Assets(..), Token(..))
import Network.RemoteData (RemoteData(..))
import Servant.PureScript (printAjaxError)
import Types (JsonAjaxError)

newtype RoleTokenStore = RoleTokenStore
  { myRoleTokens :: Set Token
  , roleTokens :: Map Token (RemoteData String RoleToken)
  }

derive instance Eq RoleTokenStore

mkRoleTokenStore :: RoleTokenStore
mkRoleTokenStore = RoleTokenStore
  { myRoleTokens: mempty
  , roleTokens: Map.empty
  }

-- | Given a collection of assets the current user owns, update the contract
-- | store with which contract role tokens are owned by the current user.
updateMyRoleTokens :: Assets -> RoleTokenStore -> RoleTokenStore
updateMyRoleTokens (Assets assets) (RoleTokenStore store) = RoleTokenStore
  store { myRoleTokens = walletTokens }
  where
  walletTokens = Set.fromFoldable
    $ filter (not <<< isAda)
    $ bindFlipped case _ of
        Tuple currency values ->
          Token currency <$> Set.toUnfoldable (Map.keys values)
    $ (Map.toUnfoldable assets :: Array _)
  isAda (Token "" "") = false
  isAda _ = true

-- | Determines if the current user owns the given role token.
isMyRoleToken :: Token -> RoleTokenStore -> Boolean
isMyRoleToken token (RoleTokenStore store) = Set.member token store.myRoleTokens

-- | Set the `RemoteData` status for each of the provided Tokens to `Loading`.
loadRoleTokens :: Set Token -> RoleTokenStore -> RoleTokenStore
loadRoleTokens tokens (RoleTokenStore store) = RoleTokenStore store
  { roleTokens = Map.union (Loading <$ Set.toMap tokens) store.roleTokens }

-- | Set the `RemoteData` status for the provided Token to `Failure`.
loadRoleTokenFailed
  :: Token -> JsonAjaxError -> RoleTokenStore -> RoleTokenStore
loadRoleTokenFailed token ajaxError (RoleTokenStore store) =
  RoleTokenStore
    store
      { roleTokens = Map.insert
          token
          (Failure $ printAjaxError ajaxError)
          store.roleTokens
      }

-- | Set the `RemoteData` status for the provided Token to `Success`.
roleTokenLoaded :: RoleToken -> RoleTokenStore -> RoleTokenStore
roleTokenLoaded roleToken (RoleTokenStore store) =
  RoleTokenStore
    store
      { roleTokens = Map.insert
          (roleToken ^. _token)
          (Success roleToken)
          store.roleTokens
      }

-- | Get a `RoleToken` for a Token.
getRoleToken :: Token -> RoleTokenStore -> RemoteData String RoleToken
getRoleToken token (RoleTokenStore store) =
  fromMaybe NotAsked $ Map.lookup token store.roleTokens
