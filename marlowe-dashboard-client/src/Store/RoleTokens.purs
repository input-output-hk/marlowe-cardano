module Store.RoleTokens
  ( RoleTokenStore
  , Payout
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
  ) where

import Prologue

import Control.Alternative (guard)
import Control.Bind (bindFlipped)
import Data.Address (Address)
import Data.Address as Address
import Data.AddressBook (AddressBook, lookupNickname)
import Data.Filterable (filter)
import Data.Foldable (oneOf)
import Data.Lens (_Just, preview, to, (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Language.Marlowe.Client (UnspentPayouts(..))
import Language.Marlowe.Client.History (RolePayout(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Assets(..)
  , MarloweParams
  , Token(..)
  , TokenName
  , _rolesCurrency
  )
import Marlowe.Run.Contract.V1.Types (RoleToken, _token, _utxoAddress)
import Network.RemoteData (RemoteData(..), toMaybe)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Servant.PureScript (printAjaxError)
import Types (JsonAjaxError)

type RoleTokenWithNickname =
  { roleToken :: RoleToken
  , nickname :: Maybe WalletNickname
  }

type Payout =
  { txOutRef :: TxOutRef
  , tokenName :: TokenName
  , marloweParams :: MarloweParams
  }

newtype RoleTokenStore = RoleTokenStore
  { myRoleTokens :: Set Token
  , roleTokens :: Map Token (RemoteData String RoleTokenWithNickname)
  , payouts :: Map TxOutRef Payout
  }

derive instance Eq RoleTokenStore

mkRoleTokenStore :: RoleTokenStore
mkRoleTokenStore = RoleTokenStore
  { myRoleTokens: mempty
  , roleTokens: Map.empty
  , payouts: Map.empty
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
getEligiblePayouts :: RoleTokenStore -> Set Payout
getEligiblePayouts (RoleTokenStore store) =
  Set.filter isEligible $ Set.fromFoldable store.payouts
  where
  isEligible ({ marloweParams, tokenName }) = Set.member
    (Token (marloweParams ^. _rolesCurrency) tokenName)
    store.myRoleTokens

newPayoutsReceived
  :: MarloweParams -> UnspentPayouts -> RoleTokenStore -> RoleTokenStore
newPayoutsReceived
  marloweParams
  (UnspentPayouts newPayouts)
  (RoleTokenStore store) = RoleTokenStore
  store
    { payouts =
        Map.union store.payouts $ Map.fromFoldable $ map toKvp newPayouts
    }
  where
  toKvp (RolePayout payout) = Tuple
    payout.rolePayoutTxOutRef
    { txOutRef: payout.rolePayoutTxOutRef
    , tokenName: (unwrap payout.rolePayoutName).unTokenName
    , marloweParams
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
