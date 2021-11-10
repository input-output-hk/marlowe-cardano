module Page.Welcome.Types
  ( Action(..)
  , Card(..)
  , State
  , WalletMnemonicError(..)
  ) where

import Prologue
import Analytics (class IsEvent, defaultEvent, toEvent)
import Clipboard (Action) as Clipboard
import Component.Contacts.Types
  ( WalletDetails
  , WalletLibrary
  , WalletNickname
  , WalletNicknameError
  )
import Component.InputField.Types (Action, State) as InputField
import Component.InputField.Types (class InputFieldError)
import Marlowe.PAB (PlutusAppId)
import Types (NotFoundWebData)

-- TODO (possibly): The Contacts submodule used in the Dashboard has some properties and
-- functionality that's similar to some of what goes on here. It might be worth generalising it so
-- it works in both cases, and including it as a submodule here too.
type State
  =
  { card :: Maybe Card
  -- Note [CardOpen]: As well as making the card a Maybe, we add an additional cardOpen flag.
  -- When closing a card we set this to false instead of setting the card to Nothing, and that
  -- way we can use CSS transitions to animate it on the way out as well as the way in. This is
  -- preferable to using the Halogen.Animation module (used for toasts), because in this case we
  -- need to simultaneously animate (fade in/out) the overlay, and because the animation for
  -- cards has to be different for different screen sizes (on large screens some cards slide in
  -- from the right) - and that's much easier to do with media queries.
  , cardOpen :: Boolean
  , walletLibrary :: WalletLibrary
  , walletNicknameInput :: InputField.State WalletNicknameError
  , walletMnemonicInput :: InputField.State WalletMnemonicError
  , walletId :: PlutusAppId
  , remoteWalletDetails :: NotFoundWebData WalletDetails
  , enteringDashboardState :: Boolean
  }

data WalletMnemonicError
  = MnemonicAmountOfWords
  | InvalidMnemonicFromServer

derive instance eqWalletMnemonicError :: Eq WalletMnemonicError

instance inputFieldErrorWalletMnemonicError ::
  InputFieldError WalletMnemonicError where
  inputErrorToString MnemonicAmountOfWords = "Mnemonic phrases have 24 words"
  inputErrorToString InvalidMnemonicFromServer =
    "The phrase is an invalid mnemonic"

-- TODO: When we implement another wallet connetctor, we should probably move this to
-- Welcome.Testnet and split the Actions into general wallet logic and Testnet wallet logic
data Card
  = GetStartedHelpCard
  | GenerateWalletHelpCard
  | UseNewWalletCard
  | UseWalletCard
  | RestoreTestnetWalletCard
  | LocalWalletMissingCard

derive instance eqCard :: Eq Card

data Action
  = OpenCard Card
  | CloseCard
  | GenerateWallet
  | RestoreTestnetWallet
  | WalletMnemonicInputAction (InputField.Action WalletMnemonicError)
  | OpenUseWalletCardWithDetails WalletDetails
  | WalletNicknameInputAction (InputField.Action WalletNicknameError)
  | ConnectWallet WalletNickname
  | ClearLocalStorage
  | ClipboardAction Clipboard.Action

-- | Here we decide which top-level queries to track as GA events, and how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent (OpenCard _) = Nothing
  toEvent CloseCard = Nothing
  toEvent GenerateWallet = Just $ defaultEvent "GenerateWallet"
  toEvent RestoreTestnetWallet = Just $ defaultEvent "RestoreTestnetWallet"
  toEvent (WalletMnemonicInputAction inputFieldAction) = toEvent
    inputFieldAction
  toEvent (OpenUseWalletCardWithDetails _) = Nothing
  toEvent (WalletNicknameInputAction inputFieldAction) = toEvent
    inputFieldAction
  toEvent (ConnectWallet _) = Just $ defaultEvent "ConnectWallet"
  toEvent ClearLocalStorage = Just $ defaultEvent "ClearLocalStorage"
  toEvent (ClipboardAction _) = Just $ defaultEvent "ClipboardAction"
