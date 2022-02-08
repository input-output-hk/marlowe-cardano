module Page.Welcome.Forms.ConfirmMnemonicForm where

import Prologue

import Capability.Marlowe (class ManageMarlowe)
import Control.Monad.Rec.Class (class MonadRec)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase (validator) as MP
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Forms (InputSlots, input, printMnemonicPhraseError)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (Form)
import Halogen.Form (mkForm) as Form
import Halogen.Form.Component as FC
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Page.Welcome.Forms.Render as Welcome.Forms
import Page.Welcome.Types
  ( Action(..)
  , Card(..)
  , CreateWalletStep(..)
  , NewWalletDetails
  )
import Page.Welcome.Types as Welcome
import Polyform.Validator (check) as Validator
import Polyform.Validator (lmapValidator)
import Type.Proxy (Proxy(..))

type Input = NewWalletDetails

type Component q m = H.Component q Input Welcome.Action m

confirmMnemonic
  :: forall pa s m
   . Monad m
  => MonadEffect m
  => MnemonicPhrase
  -> Form pa (InputSlots pa s) m String MnemonicPhrase
confirmMnemonic expected = do
  let
    validator =
      Validator.check
        ( const
            "Given mnemonic differs from your new wallet mnemonic."
        )
        (eq expected)
        <<< lmapValidator printMnemonicPhraseError MP.validator
  Form.mkForm
    { validator: validator
    , render: input "wallet-mnemonic" "Mnemonic phrase" identity
    }

component
  :: forall q m
   . MonadAff m
  => MonadRec m
  => ManageMarlowe m
  => Component q m
component = Hooks.component
  \{ outputToken } newWalletDetails@{ mnemonic, walletDetails } ->
    Hooks.do
      Tuple result resultId <- Hooks.useState Nothing

      form <- Hooks.captures { mnemonic } Hooks.useMemo \_ ->
        FC.component
          { form: confirmMnemonic mnemonic
          , formClasses: [ "relative", "space-y-4" ]
          }
      let
        connectWallet = Hooks.raise outputToken $ Welcome.ConnectWallet
          walletDetails
      Hooks.pure $ Welcome.Forms.renderForm
        { body:
            [ HH.slot (Proxy :: _ "form") unit form "" case _ of
                FC.Updated res -> Hooks.put resultId res
                FC.Raised welcomeAction -> Hooks.raise outputToken welcomeAction
            , HH.p
                [ classNames [ "pb-4" ] ]
                [ HH.text
                    "You can confirm your new wallet mnemonic now."
                ]
            ]
        , inProgress: false
        , onCancel:
            { action: Just $ Hooks.raise outputToken $ OpenCard
                $ CreateWalletCard
                $ CreateWalletPresentMnemonic newWalletDetails
            , label: "Back"
            }
        , onSubmit:
            { action: const connectWallet <$> result
            , label: "Ok"
            }
        , onSkip: Just
            { action: Just connectWallet
            , label: "Skip"
            }
        , title: "Confirm mnemonic"
        }
