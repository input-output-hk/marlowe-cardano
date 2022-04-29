module Component.Transfer.Types where

import Prologue

import Data.BigInt.Argonaut (BigInt)
import Marlowe.Semantics
  ( AccountId
  , CurrencySymbol
  , Party(..)
  , Payee(..)
  , Payment(..)
  , Token(..)
  , adaToken
  , getAda
  )
import Store.RoleTokens
  ( RoleTokenStore
  , getDisplayName
  , isMyRoleToken
  )

-- Here's my justification for why this module should exist:
-- In the semantics, there are two types that are used to represent the
-- transfer of assets. The IDeposit Action is used to represent the transfer of
-- assets from a wallet to an account. The Payment type is used to represent
-- the transfer of assets from an account to either an account or a wallet,
-- depending on the constructor used to create the Payee (Party = Wallet,
-- Account = Account)
--
-- But this is a view model, and we want to reuse the component used to render
-- this information. So we use one type to represent all three of these cases.
--
-- Note that all the types are basically the same. The additional data
-- constructors are purely to make the model more self-documenting
type Transfer =
  { sender :: Participant
  , recipient :: Participant
  , token :: Token
  , quantity :: BigInt
  , termini :: Termini
  }

data Termini
  = AccountToAccount AccountId AccountId
  | AccountToWallet AccountId Party
  | WalletToAccount Party AccountId

type Participant =
  { nickname :: Maybe String
  , isCurrentUser :: Boolean
  }

paymentToTransfer :: RoleTokenStore -> CurrencySymbol -> Payment -> Transfer
paymentToTransfer roleTokens currencySymbol (Payment sender payee money) =
  case payee of
    Party recipient ->
      makeTransfer recipient
        $ AccountToWallet sender recipient
    Account recipient ->
      makeTransfer recipient
        $ AccountToAccount sender recipient
  where
  makeTransfer recipient termini =
    { sender: partyToParticipant roleTokens currencySymbol sender
    , recipient: partyToParticipant roleTokens currencySymbol recipient
    , token: adaToken
    , quantity: getAda money
    , termini
    }

partyToParticipant :: RoleTokenStore -> CurrencySymbol -> Party -> Participant
partyToParticipant roleTokens currencySymbol (Role tokenName) =
  { nickname: getDisplayName token roleTokens
  , isCurrentUser: isMyRoleToken token roleTokens
  }
  where
  token = Token currencySymbol tokenName
partyToParticipant _ _ _ =
  -- FIXME pass the user's wallet in and check the pub key hash here. Better
  -- yet, Update the Marlowe Semantics to use addresses instead of pub key
  -- hashes.
  { nickname: Nothing
  , isCurrentUser: false
  }
