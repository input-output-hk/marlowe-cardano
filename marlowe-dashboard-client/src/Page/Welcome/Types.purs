module Page.Welcome.Types
  ( Action(..)
  , Card(..)
  , CreateWalletStep(..)
  , NewWalletDetails
  , State
  ) where

import Prologue

import Analytics (class IsEvent, defaultEvent)
import Clipboard (Action) as Clipboard
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.Wallet (WalletDetails)
import Page.Welcome.RestoreWallet.Types (RestoreWalletFields)
import Page.Welcome.RestoreWallet.Types as RestoreWallet

-- TODO (possibly): The Contacts submodule used in the Dashboard has some properties and
-- functionality that's similar to some of what goes on here. It might be worth generalising it so
-- it works in both cases, and including it as a submodule here too.
type State =
  { card :: Maybe Card
  -- Note [CardOpen]: As well as making the card a Maybe, we add an additional cardOpen flag.
  -- When closing a card we set this to false instead of setting the card to Nothing, and that
  -- way we can use CSS transitions to animate it on the way out as well as the way in. This is
  -- preferable to using the Halogen.Animation module (used for toasts), because in this case we
  -- need to simultaneously animate (fade in/out) the overlay, and because the animation for
  -- cards has to be different for different screen sizes (on large screens some cards slide in
  -- from the right) - and that's much easier to do with media queries.
  , cardOpen :: Boolean
  , enteringDashboardState :: Boolean
  , restoreFields :: RestoreWalletFields
  }

type NewWalletDetails =
  { mnemonic :: MnemonicPhrase
  , walletDetails :: WalletDetails
  }

-- This type is probably not testnet specific.
data CreateWalletStep
  = CreateWalletSetWalletName
  | CreateWalletPresentMnemonic NewWalletDetails
  | CreateWalletConfirmMnemonic NewWalletDetails

derive instance Eq CreateWalletStep

-- TODO: When we implement another wallet connetctor, we should probably move this to
-- Welcome.Testnet and split the Actions into general wallet logic and Testnet wallet logic
data Card
  = GetStartedHelpCard
  | CreateWalletHelpCard
  | CreateWalletCard CreateWalletStep
  | RestoreWalletCard
  | LocalWalletMissingCard

derive instance eqCard :: Eq Card

data Action
  = OpenCard Card
  | CloseCard
  | ConnectWallet WalletDetails
  | ClearLocalStorage
  | ClipboardAction Clipboard.Action
  | OnRestoreWalletMsg RestoreWallet.Msg

-- | Here we decide which top-level queries to track as GA events, and how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent (OpenCard _) = Nothing
  toEvent CloseCard = Nothing
  toEvent (ConnectWallet _) = Just $ defaultEvent "ConnectWallet"
  toEvent ClearLocalStorage = Just $ defaultEvent "ClearLocalStorage"
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
  toEvent (OnRestoreWalletMsg _) = Nothing
