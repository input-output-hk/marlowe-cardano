module Data.ContractUserParties
  ( ContractUserParties
  , contractUserParties
  , getNickname
  , getParticipants
  , getUserParties
  , isCurrentUser
  ) where

import Prologue

import Data.Foldable (foldMap)
import Data.Lens (view)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.PABConnectedWallet (PABConnectedWallet, _assets, _pubKeyHash)
import Data.PaymentPubKeyHash (_PaymentPubKeyHash)
import Data.PubKeyHash (_PubKeyHash)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.WalletNickname (WalletNickname)
import Marlowe.HasParties (getParties)
import Marlowe.Semantics (Contract, MarloweParams, Party(..), _rolesCurrency)

-- This data type represents how a user sees the different Party's of
-- a contract.
newtype ContractUserParties =
  ContractUserParties
    { -- How the user sees the different Parties of a contract
      -- TODO: SCP-3557 Create a data type that represents
      --                `Either Address WalletNickname`
      participants :: Map Party (Maybe WalletNickname)
    -- The Parties (Roles and PubKey's) that the "logged-in" user has for an
    -- instace of a contract.
    , userParties :: Set Party
    }

derive instance Eq ContractUserParties

contractUserParties
  :: PABConnectedWallet -> MarloweParams -> Contract -> ContractUserParties
contractUserParties wallet marloweParams contract =
  ContractUserParties
    let
      -- TODO: SCP-3557
      --            Add an endpoint that given a currencyId and a list of
      --            roles it gives you the Address of the owners With the
      --            AddressBook and the Address of the owners we should
      --            change this to a data type that represents
      --            `Either Address WalletNickname`
      participants = Map.fromFoldable
        $ map (\x -> x /\ Nothing)
        $ (Set.toUnfoldable :: Set _ -> Array _)
        $ getParties contract

      -- the Payment PubKeyHash of the `logged-user`
      pubKeyHash = view
        (_pubKeyHash <<< _PaymentPubKeyHash <<< _PubKeyHash)
        wallet

      userPubKeyParty = PK pubKeyHash

      assets = view _assets wallet

      rolesCurrency = view _rolesCurrency marloweParams

      -- See if we have any role for the currencySymbol of the contract
      mCurrencyTokens = Map.lookup rolesCurrency (unwrap assets)

      userRoles = foldMap (Set.map Role <<< Map.keys <<< Map.filter ((/=) zero))
        mCurrencyTokens

      userParties = Set.insert userPubKeyParty userRoles
    in
      { participants
      , userParties
      }

-- TODO: SCP-3557 change this to return a String for the Address or Nickname
getNickname :: Party -> ContractUserParties -> Maybe WalletNickname
getNickname party (ContractUserParties { participants }) = join
  $ Map.lookup party
  $ participants

getParticipants :: ContractUserParties -> Set Party
getParticipants (ContractUserParties { participants }) = Map.keys participants

getUserParties :: ContractUserParties -> Set Party
getUserParties (ContractUserParties { userParties }) = userParties

isCurrentUser :: Party -> ContractUserParties -> Boolean
isCurrentUser party (ContractUserParties { userParties }) =
  Set.member party userParties
