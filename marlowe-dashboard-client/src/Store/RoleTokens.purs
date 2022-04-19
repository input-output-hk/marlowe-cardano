module Store.RoleTokens
  ( RoleTokenStore
  , getDisplayName
  , getRoleToken
  , getNickname
  , isMyRoleToken
  , loadRoleTokenFailed
  , loadRoleTokens
  , mkRoleTokenStore
  , roleTokenLoaded
  , updateMyRoleTokens
  , getEligiblePayouts
  , newPayoutsReceived
  , addPendingPayout
  , removePendingPayout
  ) where

import Prologue

import Control.Alternative (guard)
import Control.Bind (bindFlipped)
import Data.Address (Address)
import Data.Address as Address
import Data.AddressBook (AddressBook, lookupNickname)
import Data.Array (null)
import Data.Filterable (filter)
import Data.Foldable (oneOf)
import Data.Lens (_Just, preview, to, (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (over, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Language.Marlowe.Client (UnspentPayouts(..))
import Language.Marlowe.Client.History (RolePayout(..))
import Marlowe.Run.Contract.V1.Types (RoleToken, _token, _utxoAddress)
import Marlowe.Semantics (Assets(..), MarloweParams, Token(..), _rolesCurrency)
import Network.RemoteData (RemoteData(..), toMaybe)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Servant.PureScript (printAjaxError)
import Types (JsonAjaxError)

type RoleTokenWithNickname =
  { roleToken :: RoleToken
  , nickname :: Maybe WalletNickname
  }

newtype RoleTokenStore = RoleTokenStore
  { myRoleTokens :: Set Token
  , roleTokens :: Map Token (RemoteData String RoleTokenWithNickname)
  , unspentPayouts :: Map MarloweParams UnspentPayouts
  , pendingPayouts :: Set TxOutRef
  }

derive instance Eq RoleTokenStore

mkRoleTokenStore :: RoleTokenStore
mkRoleTokenStore = RoleTokenStore
  { myRoleTokens: mempty
  , roleTokens: Map.empty
  , unspentPayouts: Map.empty
  , pendingPayouts: mempty
  }

-- | Given a collection of assets the current user owns, update the contract
-- | store with which contract role tokens are owned by the current user.
updateMyRoleTokens :: Assets -> RoleTokenStore -> RoleTokenStore
updateMyRoleTokens (Assets assets) (RoleTokenStore store) = RoleTokenStore
  store
    { myRoleTokens = Set.fromFoldable
        $ filter (not <<< isAda)
        $ bindFlipped case _ of
            Tuple currency values ->
              Token currency <$> Set.toUnfoldable (Map.keys values)
        $ (Map.toUnfoldable assets :: Array _)
    }
  where
  isAda (Token "" "") = true
  isAda _ = false

-- | Determines if the current user owns the given role token.
getEligiblePayouts :: RoleTokenStore -> Map MarloweParams UnspentPayouts
getEligiblePayouts (RoleTokenStore store) = filter notEmpty
  $ map (over UnspentPayouts $ filter notPending)
  $ Map.mapMaybeWithKey (map Just <<< filterEligible) store.unspentPayouts
  where
  notEmpty (UnspentPayouts payouts) = not $ null payouts
  filterEligible marloweParams = over UnspentPayouts
    $ filter
    $ isEligible marloweParams
  isEligible marloweParams (RolePayout { rolePayoutName }) = Set.member
    ( Token
        (marloweParams ^. _rolesCurrency)
        (unwrap rolePayoutName).unTokenName
    )
    store.myRoleTokens
  notPending (RolePayout { rolePayoutTxOutRef }) =
    not $ Set.member rolePayoutTxOutRef store.pendingPayouts

newPayoutsReceived
  :: MarloweParams -> UnspentPayouts -> RoleTokenStore -> RoleTokenStore
newPayoutsReceived marloweParams payouts (RoleTokenStore store) = RoleTokenStore
  store
    { unspentPayouts = Map.insert marloweParams payouts store.unspentPayouts
    }

addPendingPayout :: RolePayout -> RoleTokenStore -> RoleTokenStore
addPendingPayout (RolePayout { rolePayoutTxOutRef }) (RoleTokenStore store) =
  RoleTokenStore
    store
      { pendingPayouts = Set.insert rolePayoutTxOutRef store.pendingPayouts
      }

removePendingPayout :: RolePayout -> RoleTokenStore -> RoleTokenStore
removePendingPayout (RolePayout { rolePayoutTxOutRef }) (RoleTokenStore store) =
  RoleTokenStore
    store
      { pendingPayouts = Set.delete rolePayoutTxOutRef store.pendingPayouts
      }

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
roleTokenLoaded :: AddressBook -> RoleToken -> RoleTokenStore -> RoleTokenStore
roleTokenLoaded addressBook roleToken (RoleTokenStore store) =
  RoleTokenStore
    store
      { roleTokens = Map.insert
          token
          (Success { roleToken, nickname })
          store.roleTokens
      }
  where
  address = roleToken ^. _utxoAddress
  token = roleToken ^. _token
  nickname = lookupNickname address addressBook

-- | Get a `RoleToken` for a Token.
getRoleToken :: Token -> RoleTokenStore -> RemoteData String RoleToken
getRoleToken token (RoleTokenStore store) =
  fromMaybe NotAsked $ map _.roleToken <$> Map.lookup token store.roleTokens

getAddress :: Token -> RoleTokenStore -> Maybe Address
getAddress token = preview
  $ to (toMaybe <<< getRoleToken token) <<< _Just <<< _utxoAddress

getNickname :: Token -> RoleTokenStore -> Maybe WalletNickname
getNickname token (RoleTokenStore store) =
  _.nickname =<< toMaybe =<< Map.lookup token store.roleTokens

getDisplayName :: Token -> RoleTokenStore -> Maybe String
getDisplayName token store = oneOf
  [ "you" <$ guard (isMyRoleToken token store)
  , WN.toString <$> getNickname token store
  , Address.toString <$> getAddress token store
  ]
